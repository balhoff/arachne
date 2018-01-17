package org.geneontology.rules.engine

import scala.collection.mutable

final class AlphaNode(val pattern: TriplePattern) {

  var children: List[JoinNode] = Nil
  private val childrenSet: mutable.Set[JoinNode] = mutable.Set.empty

  def addChild(node: JoinNode): Unit = if (childrenSet.add(node)) children = node :: children

  def orderChildren(ancestors: Map[JoinNode, Set[JoinNode]]): Unit =
    children = children.sortWith((a, b) => !ancestors(b)(a))

  def activate(triple: Triple, memory: WorkingMemory): Unit = {
    val alphaMem = memory.alpha.getOrElseUpdate(pattern, new AlphaMemory(pattern))
    alphaMem.triples = triple :: alphaMem.triples
    alphaMem.tripleIndexS += triple.s -> (triple :: alphaMem.tripleIndexS.getOrElse(triple.s, Nil))
    alphaMem.tripleIndexP += triple.p -> (triple :: alphaMem.tripleIndexP.getOrElse(triple.p, Nil))
    alphaMem.tripleIndexO += triple.o -> (triple :: alphaMem.tripleIndexO.getOrElse(triple.o, Nil))
    alphaMem.linkedChildren.foreach(_.rightActivate(triple, memory))
  }

}

sealed trait BetaNode {

  def spec: JoinNodeSpec

  def addChild(node: BetaNode): Unit

  def leftActivate(token: Token, memory: WorkingMemory): Unit

}

sealed trait BetaParent {

  def children: List[BetaNode]

}

final object BetaRoot extends BetaNode with BetaParent {

  def leftActivate(token: Token, memory: WorkingMemory): Unit = ()
  def addChild(node: BetaNode): Unit = ()
  val spec: JoinNodeSpec = JoinNodeSpec(Nil)
  val memory: BetaMemory = new BetaMemory(spec, Nil)
  val children = Nil
  memory.tokens = Token(Map.empty, Nil) :: memory.tokens

}

final case class Token(bindings: Map[Variable, ConcreteNode], triples: List[Triple]) {

  //override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  override def equals(that: Any): Boolean =
    if (this.hashCode == that.hashCode) {
      if (that.isInstanceOf[Token]) {
        val thatToken = that.asInstanceOf[Token]
        this.eq(thatToken) || ((this.bindings == thatToken.bindings) && (this.triples == thatToken.triples))
      } else false
    } else false

  def extend(tripleBindings: Map[Variable, ConcreteNode], triple: Triple): Token =
    Token(bindings ++ tripleBindings, triple :: triples)

}

final class JoinNode(val leftParent: BetaNode with BetaParent, rightParent: AlphaNode, val spec: JoinNodeSpec) extends BetaNode with BetaParent {

  private val thisPattern = spec.pattern.head
  private val parentBoundVariables = spec.pattern.drop(1).flatMap(_.variables).toSet
  private val thisPatternVariables = thisPattern.variables
  private val matchVariables = parentBoundVariables intersect thisPatternVariables
  private val rightParentPattern = rightParent.pattern

  var children: List[BetaNode] = Nil
  private val childrenSet: mutable.Set[BetaNode] = mutable.Set.empty

  def addChild(node: BetaNode): Unit = if (childrenSet.add(node)) children = node :: children

  def leftActivate(token: Token, memory: WorkingMemory): Unit = {
    val alphaMem = memory.alpha.getOrElseUpdate(rightParentPattern, new AlphaMemory(rightParentPattern))
    val betaMem = memory.beta.getOrElseUpdate(spec, new BetaMemory(spec, children))
    if (betaMem.checkRightLink) linkToAlpha(memory)
    if (alphaMem.triples.isEmpty) { // left-unlink
      val parentMem = memory.beta(leftParent.spec)
      parentMem.linkedChildren = parentMem.linkedChildren.filterNot(_ == this)
      betaMem.checkLeftLink = true
    }
    var valid = true
    var possibleTriples: List[List[Triple]] = Nil
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
        (bindingVar, bindingValue) <- newToken.bindings
      } {
        val currentMap = betaMem.tokenIndex.getOrElseUpdate(bindingVar, mutable.AnyRefMap.empty)
        val currentList = currentMap.getOrElse(bindingValue, Nil)
        currentMap(bindingValue) = newToken :: currentList
      }
      activateChildren(tokensToSend, betaMem.linkedChildren, memory)
    }
  }

  def linkToAlpha(memory: WorkingMemory): Unit = {
    val betaMem = memory.beta.getOrElseUpdate(spec, new BetaMemory(spec, children))
    val alphaMem = memory.alpha.getOrElseUpdate(rightParentPattern, new AlphaMemory(rightParentPattern))
    betaMem.checkRightLink = false
    alphaMem.linkedChildren = this :: alphaMem.linkedChildren
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
    val betaMem = memory.beta.getOrElseUpdate(spec, new BetaMemory(spec, children))
    val parentMem = memory.beta.getOrElseUpdate(leftParent.spec, new BetaMemory(leftParent.spec, leftParent.children))
    if (betaMem.checkLeftLink) {
      parentMem.linkedChildren = this :: parentMem.linkedChildren
      betaMem.checkLeftLink = false
    }
    if (checkTriple(triple)) {
      val bindings = makeBindings(triple)
      var tokensToSend: List[Token] = Nil
      val goodTokens = if (matchVariables.nonEmpty) {
        val requiredToMatch = bindings.filterKeys(matchVariables)
        requiredToMatch.map { case (bindingVar, bindingValue) => parentMem.tokenIndex.getOrElse(bindingVar, mutable.AnyRefMap.empty).getOrElse(bindingValue, Nil) }.reduce(_ intersect _) //TODO can immutable empties be used here
      } else parentMem.tokens
      for {
        parentToken <- goodTokens
        newToken = parentToken.extend(bindings, triple)
        _ = betaMem.tokens = newToken :: betaMem.tokens
        _ = tokensToSend = newToken :: tokensToSend
        (bindingVar, bindingValue) <- newToken.bindings
      } {
        //betaMem.tokenIndex.getOrElseUpdate(binding, mutable.Set.empty).add(newToken)
        val currentMap = betaMem.tokenIndex.getOrElseUpdate(bindingVar, mutable.AnyRefMap.empty)
        val currentList = currentMap.getOrElse(bindingValue, Nil)
        currentMap(bindingValue) = newToken :: currentList
        //.add(newToken)
        //val set = betaMem.tokenIndex(binding)
        //if (set.size > 1000000) println(s"Big: $binding")
      }
      activateChildren(tokensToSend, betaMem.linkedChildren, memory)
    }
  }

  private def activateChildren(newTokens: List[Token], linkedChildren: List[BetaNode], memory: WorkingMemory): Unit = for {
    child <- linkedChildren
    token <- newTokens
  } child.leftActivate(token, memory)

}

final class ProductionNode(rule: Rule, parent: BetaNode, engine: RuleEngine) extends BetaNode {

  def leftActivate(token: Token, memory: WorkingMemory): Unit = {
    for {
      pattern <- rule.head
    } {
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
    case AnyNode         => throw new RuntimeException("Invalid rule head containing AnyNode")
  }

  def addChild(node: BetaNode): Unit = ()

  val spec: JoinNodeSpec = JoinNodeSpec(Nil)

}

final case class JoinNodeSpec(pattern: List[TriplePattern]) {

  override val hashCode: Int = pattern.hashCode

}