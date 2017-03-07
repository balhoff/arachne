package org.geneontology.rules

import scala.collection.mutable

import scalaz._
import scalaz.Scalaz._

final class AlphaNode(val pattern: TriplePattern) {

  var children: List[JoinNode] = Nil
  private val childrenSet: mutable.Set[JoinNode] = mutable.Set.empty

  def addChild(node: JoinNode): Unit = if (childrenSet.add(node)) children = node :: children

  def orderChildren(ancestors: Map[JoinNode, Set[JoinNode]]): Unit =
    children = children.sortWith((a, b) => !ancestors(b)(a))

  def activate(triple: Triple, memory: WorkingMemory): Unit = {
    val alphaMem = memory.alpha.getOrElseUpdate(pattern, new AlphaMemory(pattern))
    alphaMem.triples = triple :: alphaMem.triples
    alphaMem.tripleIndexS = alphaMem.tripleIndexS |+| Map(triple.s -> Set(triple))
    alphaMem.tripleIndexP = alphaMem.tripleIndexP |+| Map(triple.p -> Set(triple))
    alphaMem.tripleIndexO = alphaMem.tripleIndexO |+| Map(triple.o -> Set(triple))
    children.foreach(_.rightActivate(triple, memory))
  }

}

sealed trait BetaNode {

  def spec: List[TriplePattern]

  def addChild(node: BetaNode): Unit

  def leftActivate(token: Token, memory: WorkingMemory): Unit

}

final object BetaRoot extends BetaNode {

  def leftActivate(token: Token, memory: WorkingMemory): Unit = ()
  def addChild(node: BetaNode): Unit = ()
  val spec: List[TriplePattern] = Nil
  val memory: BetaMemory = new BetaMemory(spec)
  memory.tokens = Token(Map.empty, Nil) :: memory.tokens

}

final case class Token(bindings: Map[Variable, ConcreteNode], triples: List[Triple]) {

  def extend(tripleBindings: Map[Variable, ConcreteNode], triple: Triple): Token =
    Token(bindings ++ tripleBindings, triple :: triples)

}

final class JoinNode(val leftParent: BetaNode, rightParent: AlphaNode, val spec: List[TriplePattern]) extends BetaNode {

  private val thisPattern = spec.head
  private val parentBoundVariables = spec.drop(1).flatMap(_.variables).toSet
  private val thisPatternVariables = thisPattern.variables
  private val matchVariables = parentBoundVariables intersect thisPatternVariables
  private val rightParentPattern = rightParent.pattern

  var children: List[BetaNode] = Nil
  private val childrenSet: mutable.Set[BetaNode] = mutable.Set.empty

  def addChild(node: BetaNode): Unit = if (childrenSet.add(node)) children = node :: children

  def leftActivate(token: Token, memory: WorkingMemory): Unit = {
    val betaMem = memory.beta.getOrElseUpdate(spec, new BetaMemory(spec))
    val alphaMem = memory.alpha.getOrElseUpdate(rightParentPattern, new AlphaMemory(rightParentPattern))
    var valid = true
    var possibleTriples: List[Set[Triple]] = Nil
    if (thisPattern.s.isInstanceOf[Variable]) {
      val v = thisPattern.s.asInstanceOf[Variable]
      if (parentBoundVariables(v)) {
        alphaMem.tripleIndexS.get(token.bindings(v)) match {
          case Some(triples) => possibleTriples = triples :: possibleTriples
          case None          => valid = false
        }
      }
    }
    if (valid) {
      if (thisPattern.p.isInstanceOf[Variable]) {
        val v = thisPattern.p.asInstanceOf[Variable]
        if (parentBoundVariables(v)) {
          alphaMem.tripleIndexP.get(token.bindings(v)) match {
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
          alphaMem.tripleIndexO.get(token.bindings(v)) match {
            case Some(triples) => possibleTriples = triples :: possibleTriples
            case None          => valid = false
          }
        }
      }
    }
    if (valid) {
      val newTriples = possibleTriples match {
        case Nil          => alphaMem.triples
        case first :: Nil => first
        case _            => possibleTriples.reduce(_ intersect _)
      }
      var tokensToSend: List[Token] = Nil
      for {
        newTriple <- newTriples
        tripleBindings = makeBindings(newTriple)
        newToken = token.extend(tripleBindings, newTriple)
        _ = betaMem.tokens = newToken :: betaMem.tokens
        _ = tokensToSend = newToken :: tokensToSend
        binding <- newToken.bindings
      } {
        betaMem.tokenIndex = betaMem.tokenIndex |+| Map(binding -> Set(newToken))
      }
      activateChildren(tokensToSend, memory)
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

  def rightActivate(triple: Triple, memory: WorkingMemory): Unit = {
    val betaMem = memory.beta.getOrElseUpdate(spec, new BetaMemory(spec))
    val parentMem = memory.beta.getOrElseUpdate(leftParent.spec, new BetaMemory(leftParent.spec))
    if (checkTriple(triple)) {
      val bindings = makeBindings(triple)
      var tokensToSend: List[Token] = Nil
      val goodTokens = if (matchVariables.nonEmpty) {
        val requiredToMatch = bindings.filterKeys(matchVariables)
        requiredToMatch.map(binding => parentMem.tokenIndex.getOrElse(binding, Set.empty)).reduce(_ intersect _)
      } else parentMem.tokens
      for {
        parentToken <- goodTokens
        newToken = parentToken.extend(bindings, triple)
        _ = betaMem.tokens = newToken :: betaMem.tokens
        _ = tokensToSend = newToken :: tokensToSend
        binding <- newToken.bindings
      } {
        betaMem.tokenIndex = betaMem.tokenIndex |+| Map(binding -> Set(newToken))
      }
      activateChildren(tokensToSend, memory)
    }
  }

  private def activateChildren(newTokens: List[Token], memory: WorkingMemory): Unit = {
    for {
      child <- children
      token <- newTokens
    } child.leftActivate(token, memory)
  }

}

final class ProductionNode(rule: Rule, parent: BetaNode, engine: RuleEngine) extends BetaNode {

  def leftActivate(token: Token, memory: WorkingMemory): Unit = {
    for {
      pattern <- rule.head
    } {
      //FIXME get rid of casting
      val newTriple = Triple(
        produceNode(pattern.s, token).asInstanceOf[Resource],
        produceNode(pattern.p, token).asInstanceOf[URI],
        produceNode(pattern.o, token))
      if (engine.storeDerivations) engine.processDerivedTriple(newTriple, Derivation(token, rule), memory)
      else engine.processTriple(newTriple, memory)
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
