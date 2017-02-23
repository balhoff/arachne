package org.geneontology.rules

import scalaz._
import Scalaz._
import scala.collection.immutable.Seq

sealed trait Node

sealed trait ConcreteNode extends Node

sealed trait Resource extends ConcreteNode

final case class URI(uri: String) extends Resource

final case class BlankNode(id: String) extends Resource

final case class Literal(lexicalForm: String, datatype: URI, lang: Option[String]) extends ConcreteNode

sealed trait FluidNode extends Node

final case class Variable(name: String) extends FluidNode

final case object AnyNode extends FluidNode

sealed trait TripleLike {

  def s: Node
  def p: Node
  def o: Node

}

final case class TriplePattern(s: Node, p: Node, o: Node) extends TripleLike {

  def variables: Map[Variable, Set[TestSpecField]] = {
    val subjectVariable: Map[Variable, Set[TestSpecField]] =
      if (this.s.isInstanceOf[Variable]) Map(this.s.asInstanceOf[Variable] -> Set(SubjectField)) else Map.empty
    val predicateVariable: Map[Variable, Set[TestSpecField]] =
      if (this.p.isInstanceOf[Variable]) Map(this.p.asInstanceOf[Variable] -> Set(PredicateField)) else Map.empty
    val objectVariable: Map[Variable, Set[TestSpecField]] =
      if (this.o.isInstanceOf[Variable]) Map(this.o.asInstanceOf[Variable] -> Set(ObjectField)) else Map.empty
    subjectVariable |+| predicateVariable |+| objectVariable
  }

  private def replaceVar(node: Node) = node match {
    case c: ConcreteNode => c
    case f: FluidNode    => AnyNode
  }

  def blankVariables: TriplePattern = TriplePattern(replaceVar(this.s), replaceVar(this.p), replaceVar(this.o))

}

final case class Triple(s: Resource, p: URI, o: ConcreteNode) extends TripleLike

final case class Rule(name: Option[String], body: List[TriplePattern], head: Seq[TriplePattern])
