package org.geneontology.rules.engine

import scala.collection.mutable

final class WorkingMemory(val asserted: Set[Triple]) {

  var agenda: mutable.Stack[Triple] = mutable.Stack.empty
  val facts: mutable.Set[Triple] = mutable.Set.empty ++ asserted
  var derivations: Map[Triple, List[Derivation]] = Map.empty

  val alpha: mutable.Map[TriplePattern, AlphaMemory] = mutable.AnyRefMap.empty
  val beta: mutable.Map[JoinNodeSpec, BetaMemory] = mutable.AnyRefMap.empty
  beta += (BetaRoot.spec -> BetaRoot.memory)

  def explain(triple: Triple): Set[Explanation] = explainAll(Set(triple))

  private def explainAll(triples: Set[Triple]): Set[Explanation] = {
    val subExplanations = for {
      triple <- triples
    } yield derivations.get(triple) match {
      case None                    => Set(Explanation(Set(triple), Set.empty))
      case Some(tripleDerivations) => for {
        derivation <- tripleDerivations.toSet[Derivation]
        current = Explanation(Set.empty, Set(derivation.rule))
        subExplanation <- explainAll(derivation.token.triples.toSet).map(e => combine(e, current))
      } yield subExplanation
    }
    cartesianProduct(subExplanations).map(_.reduce(combine))
  }

  private def combine(a: Explanation, b: Explanation): Explanation = Explanation(a.facts ++ b.facts, a.rules ++ b.rules)

  private def cartesianProduct[T](xss: Set[Set[T]]): Set[Set[T]] = xss match {
    case e if e.isEmpty => Set(Set.empty)
    case f              =>
      val head = f.head
      val tail = f - head
      for {
        xh <- head
        xt <- cartesianProduct(tail)
      } yield xt + xh
  }

}

final class AlphaMemory(pattern: TriplePattern) {

  var triples: List[Triple] = Nil
  var tripleIndexS: Map[ConcreteNode, List[Triple]] = Map.empty
  var tripleIndexP: Map[ConcreteNode, List[Triple]] = Map.empty
  var tripleIndexO: Map[ConcreteNode, List[Triple]] = Map.empty
  var linkedChildren: List[JoinNode] = Nil

}

final class BetaMemory(val spec: JoinNodeSpec, initialLinkedChildren: List[BetaNode]) {

  var tokens: List[Token] = Nil
  var checkRightLink: Boolean = true
  var checkLeftLink: Boolean = false
  val tokenIndex: mutable.Map[Variable, mutable.Map[ConcreteNode, List[Token]]] = mutable.AnyRefMap.empty
  var linkedChildren: List[BetaNode] = initialLinkedChildren

}

final case class Derivation(token: Token, rule: Rule)

final case class Explanation(facts: Set[Triple], rules: Set[Rule]) {

  override def toString: String = "Facts:\n" + facts.mkString("\n") + "\nRules:\n" + rules.mkString("\n")

}
