package org.geneontology.rules

import scala.collection.mutable
import scala.collection.mutable.AnyRefMap

final class WorkingMemory {

  var facts: Set[Triple] = Set.empty
  var derivations: Map[Triple, List[Derivation]] = Map.empty
  val alpha: mutable.Map[TriplePattern, AlphaMemory] = AnyRefMap.empty

  val beta: mutable.Map[List[TriplePattern], BetaMemory] = AnyRefMap.empty
  beta += (BetaRoot.spec -> BetaRoot.memory)

}

final class AlphaMemory(pattern: TriplePattern) {

  var triples: List[Triple] = Nil
  var tripleIndexS: Map[ConcreteNode, Set[Triple]] = Map.empty
  var tripleIndexP: Map[ConcreteNode, Set[Triple]] = Map.empty
  var tripleIndexO: Map[ConcreteNode, Set[Triple]] = Map.empty

}

final class BetaMemory(val spec: List[TriplePattern]) {

  var tokens: List[Token] = Nil
  var tokenIndex: Map[(Variable, ConcreteNode), Set[Token]] = Map.empty

}

final case class Derivation(token: Token, rule: Rule)
