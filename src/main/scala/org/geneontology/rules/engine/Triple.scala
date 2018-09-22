package org.geneontology.rules.engine

sealed trait Node

sealed trait ConcreteNode extends Node

sealed trait Resource extends ConcreteNode

final case class URI(uri: String) extends Resource {

  override val hashCode: Int = uri.hashCode

  override def toString: String = s"<$uri>"

}

final case class BlankNode(id: String) extends Resource {

  override def toString: String = s"_:$id"

}

final case class Literal(lexicalForm: String, datatype: URI, lang: Option[String]) extends ConcreteNode {

  override def toString: String = {
    val ln = lang.map(t => s"@$t").getOrElse("")
    s""""$lexicalForm"^^$datatype$ln"""
  }

}

sealed trait FluidNode extends Node

final case class Variable(name: String) extends FluidNode {

  override def toString: String = name

}

case object AnyNode extends FluidNode

sealed trait TripleLike {

  def s: Node

  def p: Node

  def o: Node

  override def toString: String = s"($s $p $o)"

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
    case _: FluidNode    => AnyNode
  }

  def blankVariables: TriplePattern = TriplePattern(replaceVar(this.s), replaceVar(this.p), replaceVar(this.o))

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Triple(s: Resource, p: URI, o: ConcreteNode) extends TripleLike {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Rule(name: Option[String], body: List[TriplePattern], head: List[TriplePattern]) {

  override def toString: String = s"[${name.getOrElse("")} ${body.mkString(" ^ ")} -> ${head.mkString(" ^ ")} ]"

}
