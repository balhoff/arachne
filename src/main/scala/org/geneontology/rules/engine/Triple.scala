package org.geneontology.rules.engine

sealed trait Node

sealed trait ConcreteNode extends Node

sealed trait Resource extends ConcreteNode

final case class URI(uri: String) extends Resource {

  override val hashCode: Int = uri.hashCode

}

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

  def variables: Set[Variable] = {
    var vars: Set[Variable] = Set.empty
    if (this.s.isInstanceOf[Variable]) vars += this.s.asInstanceOf[Variable]
    if (this.p.isInstanceOf[Variable]) vars += this.p.asInstanceOf[Variable]
    if (this.o.isInstanceOf[Variable]) vars += this.o.asInstanceOf[Variable]
    vars
  }

  private def replaceVar(node: Node) = node match {
    case c: ConcreteNode => c
    case f: FluidNode    => AnyNode
  }

  def blankVariables: TriplePattern = TriplePattern(replaceVar(this.s), replaceVar(this.p), replaceVar(this.o))

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Triple(s: Resource, p: URI, o: ConcreteNode) extends TripleLike {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Rule(name: Option[String], body: List[TriplePattern], head: List[TriplePattern])
