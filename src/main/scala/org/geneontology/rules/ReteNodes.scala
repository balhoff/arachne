package org.geneontology.rules

final class AlphaNode(pattern: TriplePattern) {

  var triples: List[Triple] = Nil

  var children: List[JoinNode] = Nil

  def addChild(node: JoinNode): Unit = if (!children.contains(node)) children = node :: children

  def activate(triple: Triple): Unit = {
    triples = triple :: triples
    children.foreach(_.rightActivate(triple))
  }

  def orderChildren(ancestors: Map[JoinNode, Set[JoinNode]]): Unit =
    children = children.sortWith((a, b) => !ancestors(b)(a))

}

sealed trait BetaNode {

  def tokens: List[List[Triple]]

  def spec: JoinNodeSpec

  def addChild(node: BetaNode): Unit

  def leftActivate(token: List[Triple]): Unit

}

final class JoinNode(val leftParent: BetaNode, rightParent: AlphaNode, val spec: JoinNodeSpec) extends BetaNode {

  var tokens: List[List[Triple]] = Nil

  var children: List[BetaNode] = Nil

  def addChild(node: BetaNode): Unit =
    if (!children.contains(node)) children = node :: children

  val tests = spec.tests.tests

  def leftActivate(token: List[Triple]): Unit = {
    val newTokens = for {
      triple <- rightParent.triples
      newToken = triple :: token
      if tests.forall(test => test.map { case (level, field) => field.get(newToken(level)) }.toSet.size == 1)
    } yield newToken

    tokens = newTokens ::: tokens
    activateChildren(newTokens)
  }

  def rightActivate(triple: Triple): Unit = {
    val newTokens = for {
      token <- leftParent.tokens
      newToken = triple :: token
      if tests.forall(test => test.map { case (level, field) => field.get(newToken(level)) }.toSet.size == 1)
    } yield newToken
    
    tokens = newTokens ::: tokens
    activateChildren(newTokens)
  }

  private def activateChildren(newTokens: List[List[Triple]]): Unit = {
    for {
      child <- children
      token <- newTokens
    } child.leftActivate(token)
  }

}

final object BetaRoot extends BetaNode {

  def leftActivate(token: List[Triple]): Unit = ()
  val tokens: List[List[Triple]] = List(Nil)
  def addChild(node: BetaNode): Unit = ()
  val spec: JoinNodeSpec = JoinNodeSpec(Nil, TestSpec(Set.empty))

}

final class ProductionNode(rule: Rule, parent: BetaNode, engine: RuleEngine) extends BetaNode {

  private val variableMap: Map[Variable, List[(Variable, Int, TestSpecField)]] = (for {
    (pattern, index) <- rule.body.reverse.zipWithIndex
    (variable, fields) <- pattern.variables
    field <- fields
  } yield (variable, index, field)).groupBy(_._1).map {
    case (variable, uses) => variable -> uses.sortBy(_._2)
  }

  val tokens: List[List[Triple]] = Nil

  def leftActivate(token: List[Triple]): Unit = {
    for {
      pattern <- rule.head
    } {
      //FIXME get rid of casting
      engine.processTriple(
        Triple(produceNode(pattern.s, token).asInstanceOf[Resource],
          produceNode(pattern.p, token).asInstanceOf[URI],
          produceNode(pattern.o, token)))
    }
  }

  private def produceNode(node: Node, token: List[Triple]): ConcreteNode = node match {
    case c: ConcreteNode => c
    case v: Variable =>
      val (_, index, field) = variableMap(v).head
      field.get(token(index))
    //case AnyNode => error
  }

  def addChild(node: BetaNode): Unit = ()

  val spec: JoinNodeSpec = JoinNodeSpec(Nil, TestSpec(Set.empty))

}

case class JoinNodeSpec(patterns: List[TriplePattern], tests: TestSpec)

sealed trait TestSpecField {

  def get(triple: Triple): ConcreteNode

}

case object SubjectField extends TestSpecField {

  def get(triple: Triple): Resource = triple.s

}

case object PredicateField extends TestSpecField {

  def get(triple: Triple): URI = triple.p

}

case object ObjectField extends TestSpecField {

  def get(triple: Triple): ConcreteNode = triple.o

}

case class TestSpec(tests: Set[Set[(Int, TestSpecField)]])
