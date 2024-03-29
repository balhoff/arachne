package org.geneontology.rules.engine

import scala.annotation.tailrec
import scala.collection.mutable

final class RuleEngine(inputRules: Iterable[Rule], val storeDerivations: Boolean) {

  val rules: Set[Rule] = inputRules.toSet
  val (alphaIndex: Map[TriplePattern, AlphaNode], topBetaNodes: Set[JoinNode]) = processRules(rules)

  private def processRules(rules: Iterable[Rule]): (Map[TriplePattern, AlphaNode], Set[JoinNode]) = {
    val alphaNodeIndex: mutable.Map[TriplePattern, AlphaNode] = mutable.AnyRefMap.empty
    val joinIndex: mutable.Map[List[TriplePattern], JoinNode] = mutable.AnyRefMap.empty
    var topJoinNodes: Set[JoinNode] = Set.empty

    @tailrec
    def processRulePatterns(patterns: List[TriplePattern], parent: BetaNode with BetaParent, parentPatterns: List[TriplePattern], rule: Rule): Unit =
      patterns match {
        case pattern :: rest =>
          val blankPattern = pattern.blankVariables
          val alphaNode = alphaNodeIndex.getOrElseUpdate(blankPattern, new AlphaNode(blankPattern))
          val thisPatternSequence = pattern :: parentPatterns
          val joinNode = joinIndex.getOrElseUpdate(thisPatternSequence, new JoinNode(parent, alphaNode, JoinNodeSpec(thisPatternSequence)))
          parent.addChild(joinNode)
          alphaNode.addChild(joinNode)
          if (parent == BetaRoot) topJoinNodes += joinNode
          processRulePatterns(rest, joinNode, thisPatternSequence, rule)
        case Nil             =>
          val pNode = new ProductionNode(rule, parent, this)
          parent.addChild(pNode)
      }

    for {
      rule <- rules
    } processRulePatterns(rule.body, BetaRoot, Nil, rule)
    val parentsMap = joinIndex.values.map(j => j -> parentPath(j)).toMap
    for {
      alpha <- alphaNodeIndex.values
    } alpha.orderChildren(parentsMap)
    (alphaNodeIndex.toMap, topJoinNodes)
  }

  private def parentPath(joinNode: JoinNode): Set[JoinNode] = {
    val parent = joinNode.leftParent
    if (parent.isInstanceOf[JoinNode]) {
      parentPath(parent.asInstanceOf[JoinNode]) + parent.asInstanceOf[JoinNode]
    } else Set.empty
  }

  def processTriples(triples: Iterable[Triple]): WorkingMemory = {
    val memory = new WorkingMemory(triples.toSet)
    for {
      node <- topBetaNodes
    } node.linkToAlpha(memory)
    memory.assertedAgenda.pushAll(triples)
    while (memory.assertedAgenda.nonEmpty) {
      val triple = memory.assertedAgenda.pop()
      injectTriple(triple, memory)
    }
    while (memory.agenda.nonEmpty) {
      val triple = memory.agenda.pop()
      injectTriple(triple, memory)
    }
    memory
  }

  private val DegeneratePattern = TriplePattern(AnyNode, AnyNode, AnyNode)

  protected[engine] def processTriple(triple: Triple, memory: WorkingMemory): Unit =
    if (memory.facts.add(triple)) {
      memory.agenda.push(triple)
    }

  protected[engine] def processDerivedTriple(triple: Triple, derivation: Derivation, memory: WorkingMemory): Unit =
    if (memory.facts.add(triple)) {
      memory.derivations += triple -> (derivation :: memory.derivations.getOrElse(triple, Nil))
      memory.agenda.push(triple)
    }

  private def injectTriple(triple: Triple, memory: WorkingMemory): Unit = {
    activateAlphaNode(DegeneratePattern, triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(AnyNode, AnyNode, triple.o), triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(AnyNode, triple.p, AnyNode), triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(AnyNode, triple.p, triple.o), triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(triple.s, AnyNode, AnyNode), triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(triple.s, AnyNode, triple.o), triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(triple.s, triple.p, AnyNode), triple: Triple, memory: WorkingMemory)
    activateAlphaNode(TriplePattern(triple.s, triple.p, triple.o), triple: Triple, memory: WorkingMemory)
  }

  private def activateAlphaNode(pattern: TriplePattern, triple: Triple, memory: WorkingMemory): Unit =
    alphaIndex.get(pattern) match {
      case Some(alphaNode) => alphaNode.activate(triple, memory)
      case None            => ()
    }

}

