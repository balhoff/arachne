package org.geneontology.rules.util

import org.apache.jena.graph.{ Node => JenaNode }
import org.apache.jena.graph.Node_ANY
import org.apache.jena.graph.Node_Blank
import org.apache.jena.graph.Node_Literal
import org.apache.jena.graph.Node_URI
import org.apache.jena.graph.Node_Variable
import org.apache.jena.graph.{ Triple => JenaTriple }
import org.apache.jena.reasoner.{ TriplePattern => JenaTriplePattern }
import org.apache.jena.reasoner.rulesys.ClauseEntry
import org.apache.jena.reasoner.rulesys.{ Rule => JenaRule }
import org.geneontology.rules.engine._
import org.geneontology.rules.engine.Node

import scalaz._
import scalaz.Scalaz._

object Bridge {

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

  def tripleFromJena(triple: JenaTriple): Triple =
    Triple(nodeFromJena(triple.getSubject).asInstanceOf[Resource],
      nodeFromJena(triple.getPredicate).asInstanceOf[URI],
      nodeFromJena(triple.getObject).asInstanceOf[ConcreteNode])

  def nodeFromJena(node: JenaNode): Node = node match {
    case any: Node_ANY           => AnyNode
    case variable: Node_Variable => Variable(variable.getName)
    case uri: Node_URI           => URI(uri.getURI)
    case blank: Node_Blank       => BlankNode(blank.getBlankNodeLabel)
    case literal: Node_Literal   => Literal(literal.getLiteralLexicalForm, URI(literal.getLiteralDatatypeURI), Option(literal.getLiteralLanguage))
  }

}