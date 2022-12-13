package org.geneontology.rules.util

import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.graph.{NodeFactory, Node_ANY, Node_Blank, Node_Literal, Node_URI, Node_Variable, Node => JenaNode, Triple => JenaTriple}
import org.apache.jena.reasoner.rulesys.{ClauseEntry, Rule => JenaRule}
import org.apache.jena.reasoner.{TriplePattern => JenaTriplePattern}
import org.geneontology.rules.engine._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{AxiomType, OWLOntology}
import scalaz.Scalaz._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Bridge {

  private val RDFType = URI("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

  def rulesFromJena(jenaRules: Iterable[JenaRule]): Iterable[Rule] = jenaRules.flatMap(ruleFromJena)

  def ruleFromJena(jenaRule: JenaRule): Option[Rule] = for {
    body <- jenaRule.getBody.map(patternFromJena).toList.sequence
    head <- jenaRule.getHead.map(patternFromJena).toList.sequence
  } yield Rule(Option(jenaRule.getName), body, head)

  def patternFromJena(clauseEntry: ClauseEntry): Option[TriplePattern] =
    if (clauseEntry.isInstanceOf[JenaTriplePattern]) {
      val pattern = clauseEntry.asInstanceOf[JenaTriplePattern]
      Option(TriplePattern(nodeFromJena(pattern.getSubject),
        nodeFromJena(pattern.getPredicate),
        nodeFromJena(pattern.getObject)))
    } else None

  def tripleFromJena(triple: JenaTriple): Triple = Triple(
    nodeFromJena(triple.getSubject).asInstanceOf[Resource],
    nodeFromJena(triple.getPredicate).asInstanceOf[URI],
    nodeFromJena(triple.getObject).asInstanceOf[ConcreteNode])

  def nodeFromJena(node: JenaNode): Node = node match {
    case any: Node_ANY           => AnyNode
    case variable: Node_Variable => Variable(variable.getName)
    case uri: Node_URI           => URI(uri.getURI)
    case blank: Node_Blank       => BlankNode(blank.getBlankNodeLabel)
    case literal: Node_Literal   => Literal(literal.getLiteralLexicalForm, URI(literal.getLiteralDatatypeURI), Option(literal.getLiteralLanguage))
  }

  def jenaFromTriple(triple: Triple): JenaTriple = JenaTriple.create(
    jenaFromNode(triple.s),
    jenaFromNode(triple.p),
    jenaFromNode(triple.o))

  def jenaFromNode(node: Node): JenaNode = node match {
    case AnyNode                                   => JenaNode.ANY
    case Variable(name)                            => NodeFactory.createVariable(name)
    case URI(text)                                 => NodeFactory.createURI(text)
    case BlankNode(id)                             => NodeFactory.createBlankNode(id)
    case Literal(lexicalForm, URI(datatype), lang) => lang match {
      case None           => NodeFactory.createLiteral(lexicalForm, TypeMapper.getInstance.getSafeTypeByName(datatype))
      case Some(language) => NodeFactory.createLiteral(lexicalForm, language, TypeMapper.getInstance.getSafeTypeByName(datatype))
    }
  }

  class IndirectTypesContainer {

    val indirectTypes: mutable.Map[URI, Set[URI]] = mutable.AnyRefMap.empty

  }

  def injectIndirectTypeActions(rules: Set[Rule], ontology: OWLOntology): Set[Rule] = {
    val subToSupers = (for {
      SubClassOf(_, Class(sub), Class(sup)) <- ontology.getAxioms(AxiomType.SUBCLASS_OF).asScala.toSet
    } yield URI(sub.toString) -> URI(sup.toString)).groupMap(_._1)(_._2)

    def updateIndirectTypes(indirects: Set[URI], token: Token, container: IndirectTypesContainer): Unit = {
      token.bindings.values.foreach {
        case named: URI => container.indirectTypes.updateWith(named) {
          case Some(current) => Some(current ++ indirects)
          case None          => Some(indirects)
        }
        case _          => ()
      }
    }

    val updatedRules = rules.map {
      case rule @ Rule(_, TriplePattern(_: Variable, RDFType, _: URI) :: Nil, TriplePattern(_: Variable, RDFType, superclass: URI) :: Nil, _) =>
        val indirects = subToSupers.get(superclass).toSet.flatten
        val action = (_: Rule, token: Token, wm: WorkingMemory) => {
          wm.userInfo match {
            case container: IndirectTypesContainer => updateIndirectTypes(indirects, token, container)
            case _                                 =>
              val container = new IndirectTypesContainer()
              wm.userInfo = container
              updateIndirectTypes(indirects, token, container)
          }
        }
        rule.copy(action = Some(action))
      case other                                                                                                                              => other
    }
    updatedRules
  }

}