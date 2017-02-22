package org.geneontology.rules

import org.apache.jena.reasoner.rulesys.{ Rule => JenaRule }
import org.apache.jena.reasoner.{ TriplePattern => JenaTriplePattern }
import org.apache.jena.graph.{ Triple => JenaTriple }
import org.apache.jena.graph.{ Node => JenaNode }
import org.apache.jena.reasoner.rulesys.ClauseEntry
import org.apache.jena.graph.Node_ANY
import org.apache.jena.graph.Node_Variable
import org.apache.jena.graph.Node_URI
import org.apache.jena.graph.Node_Blank
import org.apache.jena.graph.Node_Literal

object Bridge {

  def ruleFromJena(jenaRule: JenaRule): Rule =
    Rule(Option(jenaRule.getName), jenaRule.getBody.map(patternFromJena).toList, jenaRule.getHead.map(patternFromJena).toList)

  def patternFromJena(clauseEntry: ClauseEntry): TriplePattern = {
    val pattern = clauseEntry.asInstanceOf[JenaTriplePattern]
    TriplePattern(nodeFromJena(pattern.getSubject),
      nodeFromJena(pattern.getPredicate),
      nodeFromJena(pattern.getObject))
  }

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