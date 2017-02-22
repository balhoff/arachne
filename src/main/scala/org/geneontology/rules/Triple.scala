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

  private val subjectGetter: Triple => Node = (triple: Triple) => triple.s
  private val predicateGetter: Triple => Node = (triple: Triple) => triple.p
  private val objectGetter: Triple => Node = (triple: Triple) => triple.o

  def variables: Map[Variable, Set[TestSpecField]] = {
    val sFunction: Map[Variable, Set[TestSpecField]] =
      if (this.s.isInstanceOf[Variable]) Map(this.s.asInstanceOf[Variable] -> Set(SubjectField)) else Map.empty
    val pFunction: Map[Variable, Set[TestSpecField]] =
      if (this.p.isInstanceOf[Variable]) Map(this.p.asInstanceOf[Variable] -> Set(PredicateField)) else Map.empty
    val oFunction: Map[Variable, Set[TestSpecField]] =
      if (this.o.isInstanceOf[Variable]) Map(this.o.asInstanceOf[Variable] -> Set(ObjectField)) else Map.empty
    sFunction |+| pFunction |+| oFunction //TODO rename
  }

}

final case class Triple(s: Resource, p: URI, o: ConcreteNode) extends TripleLike

final case class Rule(name: Option[String], body: List[TriplePattern], head: Seq[TriplePattern])
