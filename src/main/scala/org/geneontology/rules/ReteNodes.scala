package org.geneontology.rules

import scalaz._
import scalaz.Scalaz._

final class AlphaNode(pattern: TriplePattern) {

  var triples: List[Triple] = Nil

  var tripleIndexS: Map[ConcreteNode, Set[Triple]] = Map.empty
  var tripleIndexP: Map[ConcreteNode, Set[Triple]] = Map.empty
  var tripleIndexO: Map[ConcreteNode, Set[Triple]] = Map.empty

  var children: List[JoinNode] = Nil

  def addChild(node: JoinNode): Unit = if (!children.contains(node)) children = node :: children

  def orderChildren(ancestors: Map[JoinNode, Set[JoinNode]]): Unit =
    children = children.sortWith((a, b) => !ancestors(b)(a))

  def activate(triple: Triple): Unit = {
    triples = triple :: triples
    tripleIndexS = tripleIndexS |+| Map(triple.s -> Set(triple))
    tripleIndexP = tripleIndexP |+| Map(triple.p -> Set(triple))
    tripleIndexO = tripleIndexO |+| Map(triple.o -> Set(triple))
    children.foreach(_.rightActivate(triple))
  }

}

sealed trait BetaNode {

  def tokens: List[Token]

  def tokenIndex: Map[(Variable, ConcreteNode), Set[Token]]

  def spec: List[TriplePattern]

  def addChild(node: BetaNode): Unit

  def leftActivate(token: Token): Unit

}

case class Token(bindings: Map[Variable, ConcreteNode], triples: List[Triple]) {

  def extend(tripleBindings: Map[Variable, ConcreteNode], triple: Triple): Token =
    Token(bindings ++ tripleBindings, triple :: triples)

}

final class JoinNode(val leftParent: BetaNode, rightParent: AlphaNode, val spec: List[TriplePattern]) extends BetaNode {

  private val thisPattern = spec.head
  private val parentBoundVariables = spec.drop(1).flatMap(_.variables).toSet
  private val thisPatternVariables = thisPattern.variables
  private val matchVariables = parentBoundVariables intersect thisPatternVariables

  var tokens: List[Token] = Nil
  var tokenIndex: Map[(Variable, ConcreteNode), Set[Token]] = Map.empty
  var children: List[BetaNode] = Nil

  def addChild(node: BetaNode): Unit =
    if (!children.contains(node)) children = node :: children

  def leftActivate(token: Token): Unit = {
    var valid = true
    var possibleTriples: List[Set[Triple]] = Nil
    if (thisPattern.s.isInstanceOf[Variable]) {
      val v = thisPattern.s.asInstanceOf[Variable]
      if (parentBoundVariables(v)) {
        rightParent.tripleIndexS.get(token.bindings(v)) match {
          case Some(triples) => possibleTriples = triples :: possibleTriples
          case None          => valid = false
        }
      }
    }
    if (valid) {
      if (thisPattern.p.isInstanceOf[Variable]) {
        val v = thisPattern.p.asInstanceOf[Variable]
        if (parentBoundVariables(v)) {
          rightParent.tripleIndexP.get(token.bindings(v)) match {
            case Some(triples) => possibleTriples = triples :: possibleTriples
            case None          => valid = false
          }
        }
      }
    }
    if (valid) {
      if (thisPattern.o.isInstanceOf[Variable]) {
        val v = thisPattern.o.asInstanceOf[Variable]
        if (parentBoundVariables(v)) {
          rightParent.tripleIndexO.get(token.bindings(v)) match {
            case Some(triples) => possibleTriples = triples :: possibleTriples
            case None          => valid = false
          }
        }
      }
    }
    if (valid) {
      val newTriples = possibleTriples match {
        case Nil          => rightParent.triples
        case first :: Nil => possibleTriples.head
        case _            => possibleTriples.reduce(_ intersect _)
      }
      var tokensToSend: List[Token] = Nil
      for {
        newTriple <- newTriples
        tripleBindings = makeBindings(newTriple)
        newToken = token.extend(tripleBindings, newTriple)
        _ = tokens = newToken :: tokens
        _ = tokensToSend = newToken :: tokensToSend
        binding <- newToken.bindings
      } {
        tokenIndex = tokenIndex |+| Map(binding -> Set(newToken))
      }
      activateChildren(tokensToSend)
    }
  }

  private val checkTriple: Triple => Boolean = thisPattern match {
    case TriplePattern(s: Variable, p: Variable, o: Variable) if (s == p) && (p == o) => (triple: Triple) => (triple.s == triple.p) && (triple.p == triple.o)
    case TriplePattern(s: Variable, _, o: Variable) if s == o => (triple: Triple) => triple.s == triple.o
    case TriplePattern(s: Variable, p: Variable, _) if s == p => (triple: Triple) => triple.s == triple.p
    case TriplePattern(_, p: Variable, o: Variable) if p == o => (triple: Triple) => triple.p == triple.o
    case _ => (triple: Triple) => true
  }

  private val makeBindings: Triple => Map[Variable, ConcreteNode] = {
    var funcs: List[Triple => (Variable, ConcreteNode)] = Nil
    if (thisPattern.s.isInstanceOf[Variable]) funcs = ((triple: Triple) => thisPattern.s.asInstanceOf[Variable] -> triple.s) :: funcs
    if (thisPattern.p.isInstanceOf[Variable]) funcs = ((triple: Triple) => thisPattern.p.asInstanceOf[Variable] -> triple.p) :: funcs
    if (thisPattern.o.isInstanceOf[Variable]) funcs = ((triple: Triple) => thisPattern.o.asInstanceOf[Variable] -> triple.o) :: funcs
    (triple: Triple) => {
      var bindings: Map[Variable, ConcreteNode] = Map.empty
      funcs.foreach(f => bindings += f(triple))
      bindings
    }
  }

  def rightActivate(triple: Triple): Unit = {
    if (checkTriple(triple)) {
      val bindings = makeBindings(triple)
      var tokensToSend: List[Token] = Nil
      val goodTokens = if (matchVariables.nonEmpty) {
        val requiredToMatch = bindings.filterKeys(matchVariables)
        requiredToMatch.map(binding => leftParent.tokenIndex.getOrElse(binding, Set.empty)).reduce(_ intersect _)
      } else leftParent.tokens
      for {
        parentToken <- goodTokens
        newToken = parentToken.extend(bindings, triple)
        _ = tokens = newToken :: tokens
        _ = tokensToSend = newToken :: tokensToSend
        binding <- newToken.bindings
      } {
        tokenIndex = tokenIndex |+| Map(binding -> Set(newToken))
      }
      activateChildren(tokensToSend)
    }
  }

  private def activateChildren(newTokens: List[Token]): Unit = {
    for {
      child <- children
      token <- newTokens
    } child.leftActivate(token)
  }

}

final object BetaRoot extends BetaNode {

  def leftActivate(token: Token): Unit = ()
  val tokens: List[Token] = List(Token(Map.empty, Nil))
  val tokenIndex: Map[(Variable, ConcreteNode), Set[Token]] = Map.empty
  def addChild(node: BetaNode): Unit = ()
  val spec: List[TriplePattern] = Nil

}

final class ProductionNode(rule: Rule, parent: BetaNode, engine: RuleEngine) extends BetaNode {

  val tokens: List[Token] = List.empty
  val tokenIndex: Map[(Variable, ConcreteNode), Set[Token]] = Map.empty

  def leftActivate(token: Token): Unit = {
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

  private def produceNode(node: Node, token: Token): ConcreteNode = node match {
    case c: ConcreteNode => c
    case v: Variable     => token.bindings(v)
    //case AnyNode => error
  }

  def addChild(node: BetaNode): Unit = ()

  val spec: List[TriplePattern] = Nil

}

