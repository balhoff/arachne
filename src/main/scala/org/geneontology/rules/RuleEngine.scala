package org.geneontology.rules

import scala.collection.mutable

class RuleEngine(val rules: Iterable[Rule]) {

  var workingMemory: Set[Triple] = Set.empty
  private val alphaIndex: Map[TriplePattern, AlphaNode] = processRules(rules)

  private def processRules(rules: Iterable[Rule]): Map[TriplePattern, AlphaNode] = {
    val alphaIndex: mutable.Map[TriplePattern, AlphaNode] = mutable.Map.empty
    val joinIndex: mutable.Map[JoinNodeSpec, JoinNode] = mutable.Map.empty
    for { rule <- rules } {
      def processRulePatterns(patterns: List[TriplePattern], parent: BetaNode, parentPatterns: List[TriplePattern]): Unit = patterns match {
        case pattern :: rest => {
          val blankPattern = pattern.blankVariables
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

  def processTriples(triples: Iterable[Triple]): Unit = triples.foreach(processTriple)

  private val DegeneratePattern = TriplePattern(AnyNode, AnyNode, AnyNode)

  var size = 0

  def processTriple(triple: Triple) = {
    if (!workingMemory(triple)) {
      workingMemory += triple
      size += 1
      if (size % 10000 == 0) println(s"Working memory size: $size")
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


