package org.geneontology.rules

import scala.collection.mutable

class RuleEngine(val rules: Iterable[Rule]) {

  var workingMemory: Set[Triple] = Set.empty
  private val alphaIndex: Map[TriplePattern, AlphaNode] = processRules(rules)

  private def processRules(rules: Iterable[Rule]): Map[TriplePattern, AlphaNode] = {
    val alphaNodeIndex: mutable.Map[TriplePattern, AlphaNode] = mutable.Map.empty
    val joinIndex: mutable.Map[List[TriplePattern], JoinNode] = mutable.Map.empty
    for { rule <- rules } {
      def processRulePatterns(patterns: List[TriplePattern], parent: BetaNode, parentPatterns: List[TriplePattern]): Unit = patterns match {
        case pattern :: rest => {
          val blankPattern = pattern.blankVariables
          val alphaNode = alphaNodeIndex.getOrElseUpdate(blankPattern, new AlphaNode(blankPattern))
          val thisPatternSequence = pattern :: parentPatterns
          val joinNode = joinIndex.getOrElseUpdate(thisPatternSequence, new JoinNode(parent, alphaNode, thisPatternSequence))
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
      alpha <- alphaNodeIndex.valuesIterator
    } {
      alpha.orderChildren(parentsMap)
    }
    alphaNodeIndex.toMap
  }

  def parentPath(joinNode: JoinNode): Set[JoinNode] = {
    val parent = joinNode.leftParent
    if (parent.isInstanceOf[JoinNode]) {
      parentPath(parent.asInstanceOf[JoinNode]) + parent.asInstanceOf[JoinNode]
    } else Set.empty
  }

  def processTriples(triples: Iterable[Triple]): Unit = triples.foreach(processTriple)

  private val DegeneratePattern = TriplePattern(AnyNode, AnyNode, AnyNode)

  def processTriple(triple: Triple) = {
    if (!workingMemory(triple)) {
      workingMemory += triple
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
        alphaNode <- alphaIndex.get(pattern)
      } alphaNode.activate(triple)
    }
  }

}


