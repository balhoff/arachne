package org.geneontology.rules

import scala.collection.mutable

class RuleEngine(val rules: Iterable[Rule]) {

  var workingMemory: Set[Triple] = Set.empty
  private val alphaIndex: Map[TriplePattern, AlphaNode] = processRules(rules)
  println(alphaIndex.size)

  private def processRules(rules: Iterable[Rule]): Map[TriplePattern, AlphaNode] = {
    val alphaIndex: mutable.Map[TriplePattern, AlphaNode] = mutable.Map.empty
    val joinIndex: mutable.Map[JoinNodeSpec, JoinNode] = mutable.Map.empty
    for { rule <- rules } {
      def processRulePatterns(patterns: List[TriplePattern], parent: BetaNode, parentPatterns: List[TriplePattern]): Unit = patterns match {
        //        case pattern :: Nil if parent == BetaRoot => {
        //          val blankPattern = blankVariables(pattern)
        //          val alphaNode = alphaIndex.getOrElseUpdate(blankPattern, new AlphaNode(blankPattern))
        //          val pNode = new ProductionNode(rule, alphaNode, this)
        //          alphaNode.addChild(pNode)
        //        }
        case pattern :: rest => {
          val blankPattern = blankVariables(pattern)
          val alphaNode = alphaIndex.getOrElseUpdate(blankPattern, new AlphaNode(blankPattern))
          val thisPatternSequence = pattern :: parentPatterns
          val thisPatternVariables = pattern.variables.keySet
          val tests = (for {
            (pattern, index) <- thisPatternSequence.zipWithIndex
            (variable, fields) <- pattern.variables
            if thisPatternVariables(variable)
            field <- fields
          } yield (variable, index, field)).groupBy(_._1).collect {
            case (variable, uses) if uses.size > 1 => uses.map(u => (u._2, u._3)).toSet
          }.toSet
          val joinSpec = JoinNodeSpec(blankPattern :: parent.spec.patterns, TestSpec(tests))
          val joinNode = joinIndex.getOrElseUpdate(joinSpec, new JoinNode(parent, alphaNode, joinSpec))
          joinNode.addRule(rule)
          parent.addChild(joinNode)
          alphaNode.addChild(joinNode)
          processRulePatterns(rest, joinNode, thisPatternSequence)
        }
        case Nil => {
          val pNode = new ProductionNode(rule, parent, this)
          parent.addChild(pNode)
        }
      }
      processRulePatterns(rule.body, BetaRoot, Nil)
    }
    val parentsMap = joinIndex.valuesIterator.map(j => j -> parentPath(j)).toMap
    for {
      alpha <- alphaIndex.valuesIterator
    } {
      alpha.orderChildren(parentsMap)
    }
    alphaIndex.toMap
  }

  def parentPath(joinNode: JoinNode): Set[JoinNode] = {
    val parent = joinNode.leftParent
    if (parent.isInstanceOf[JoinNode]) {
      parentPath(parent.asInstanceOf[JoinNode]) + parent.asInstanceOf[JoinNode]
    } else Set.empty
  }

  def processTriples(triples: Iterable[Triple]): Unit = {
    for {
      triple <- triples
      if !workingMemory(triple)
    } {
      workingMemory += triple
      println(s"Working memory size: ${workingMemory.size}")
      processTriple(triple)
    }
    //workingMemory ++= triples
    //triples.foreach(processTriple)
  }

  private def replaceVar(node: Node) = node match {
    case c: ConcreteNode => c
    case f: FluidNode    => AnyNode
  }

  private def blankVariables(pattern: TriplePattern): TriplePattern = TriplePattern(replaceVar(pattern.s), replaceVar(pattern.p), replaceVar(pattern.o))

  private val DegeneratePattern = TriplePattern(AnyNode, AnyNode, AnyNode)

  private def processTriple(triple: Triple) = {
    val patterns = List(DegeneratePattern,
      TriplePattern(AnyNode, AnyNode, triple.o),
      TriplePattern(AnyNode, triple.p, AnyNode),
      TriplePattern(AnyNode, triple.p, triple.o),
      TriplePattern(triple.s, AnyNode, AnyNode),
      TriplePattern(triple.s, AnyNode, triple.o),
      TriplePattern(triple.s, triple.p, AnyNode),
      TriplePattern(triple.s, triple.p, triple.o))
    for {
      pattern <- patterns
      memory <- alphaIndex.get(pattern)
    } memory.rightActivate(triple)
  }

}

sealed trait ReteNode {

  def leftActivate(token: List[Triple]): Unit
  def rightActivate(triple: Triple): Unit

}

final class AlphaNode(pattern: TriplePattern) extends ReteNode {

  var triples: List[Triple] = Nil

  var children: List[ReteNode] = Nil

  def addChild(node: BetaNode): Unit = if (!children.contains(node)) children = node :: children

  def leftActivate(token: List[Triple]): Unit = ()

  def rightActivate(triple: Triple): Unit = {
    triples = triple :: triples
    children.foreach(_.rightActivate(triple))
  }

  def orderChildren(parentsMap: Map[JoinNode, Set[JoinNode]]): Unit = {
    children = children.sortWith((a, b) => (a, b) match {
      case (p1: ProductionNode, p2: ProductionNode) => false //p1.hashCode < p2.hashCode
      case (n: JoinNode, p: ProductionNode)         => false
      case (p: ProductionNode, n: JoinNode)         => true
      case (n1: JoinNode, n2: JoinNode)             => !parentsMap(n2)(n1)
      //        {
      //        if (!parentsMap(n1)(n2) && !parentsMap(n2)(n1)) false //n1.hashCode < n2.hashCode
      //        else parentsMap(n1)(n2)
      //      }
    })
  }

}

sealed trait BetaNode extends ReteNode {

  def tokens: Set[List[Triple]]

  def spec: JoinNodeSpec

  def addChild(node: BetaNode): Unit

}

final class JoinNode(var leftParent: BetaNode, rightParent: AlphaNode, val spec: JoinNodeSpec) extends BetaNode {

  //var tokens: List[List[Triple]] = Nil
  var tokens: Set[List[Triple]] = Set.empty

  var children: Vector[ReteNode] = Vector.empty

  var rules: Set[Rule] = Set.empty

  def addRule(rule: Rule) = rules += rule

  def addChild(node: BetaNode): Unit = {
    if (!children.contains(node)) children = node +: children

    // childrenSize += 1
  }

  val tests = spec.tests.tests

  def leftActivate(token: List[Triple]): Unit = {
    //    println(s"Left activate: $this $token")
    //    println(this.spec)
    //    rules.foreach(println)
    //search right memory and if find, make token and put in tokens; send token to children
    if (!tokens(token)) {
      val newTokens = (for {
        triple <- rightParent.triples
        newToken = triple :: token
        if tests.forall(test => test.map { case (level, field) => field.get(newToken(level)) }.toSet.size == 1)
      } yield newToken).toSet
      //    val newTokens = rightParent.triples
      //      .filter(triple => tests.forall(test => test(token, triple)))
      //      .map(_ :: token)

      //tokens = newTokens ::: tokens
      tokens ++= newTokens
      //println("Activating children: " + children.size)
      activateChildren(newTokens)
    } else println("Already seen")

  }

  def rightActivate(triple: Triple): Unit = {
    //    println(s"Right activate: $this $triple")
    val newTokens = for {
      token <- leftParent.tokens
      newToken = triple :: token
      // _ = println("Testing:")
      //_ = println(newToken)
      //_ = println("Tests: " + tests)
      if tests.forall(test => test.map { case (level, field) => field.get(newToken(level)) }.toSet.size == 1)
    } yield newToken
    //    val newTokens = leftParent.tokens.filter(token => tests.forall(test => test(token, triple)))
    //      .map(triple :: _)

    //tokens = newTokens ::: tokens
    tokens ++= newTokens
    activateChildren(newTokens)
  }

  private def activateChildren(newTokens: Set[List[Triple]]): Unit = {
    for {
      child <- children
      token <- newTokens
    } child.leftActivate(token)
  }

}

final object BetaRoot extends BetaNode {

  def leftActivate(token: List[Triple]): Unit = ()
  def rightActivate(triple: Triple): Unit = ()
  //val tokens: List[List[Triple]] = List(Nil)
  val tokens: Set[List[Triple]] = Set(Nil)
  val children: List[ReteNode] = Nil
  def addChild(node: BetaNode): Unit = ()
  val spec: JoinNodeSpec = JoinNodeSpec(Nil, TestSpec(Set.empty))

}

final class ProductionNode(rule: Rule, parent: ReteNode, engine: RuleEngine) extends BetaNode {

  private val variables = rule.head.flatMap(_.variables.keys).toSet

  private val reversePatterns = rule.body.reverse

  private val variableMap: Map[Variable, List[(Variable, Int, TestSpecField)]] = (for {
    (pattern, index) <- reversePatterns.zipWithIndex
    (variable, fields) <- pattern.variables
    field <- fields
  } yield (variable, index, field)).groupBy(_._1).map {
    case (variable, uses) => variable -> uses.sortBy(_._2)
  }

  //val tokens: List[List[Triple]] = Nil
  val tokens: Set[List[Triple]] = Set.empty

  def rightActivate(triple: Triple): Unit = produce(List(triple))

  def leftActivate(token: List[Triple]): Unit = produce(token)

  def produce(token: List[Triple]): Unit = {
    val triples = for {
      pattern <- rule.head
    } yield {
      //FIXME get rid of casting
      Triple(produceNode(pattern.s, token).asInstanceOf[Resource], produceNode(pattern.p, token).asInstanceOf[URI], produceNode(pattern.o, token))
    }
    engine.processTriples(triples)
    // println(s"Produce rule: ${rule.body} -> ${rule.head}")
    //println(s"Token: $token")
  }

  private def produceNode(node: Node, token: List[Triple]): ConcreteNode = node match {
    case c: ConcreteNode => c
    case v: Variable =>
      val (_, index, field) = variableMap(v).head
      field.get(token(index))
    //case _ => error
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

