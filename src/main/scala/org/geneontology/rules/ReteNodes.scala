package org.geneontology.rules

import scalaz._
import Scalaz._

//import scala.collection.mutable

//import scala.collection.Map

//class MyHashMap[A, B](initSize : Int) extends mutable.HashMap[A, B] {
//  override def initialSize: Int = initSize // 16 - by default
//}

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
    //tripleIndexS = tripleIndexS + (triple.s -> (tripleIndexS.getOrElse(triple.s, Set.empty) + triple))
    //tripleIndexP = tripleIndexP + (triple.p -> (tripleIndexP.getOrElse(triple.p, Set.empty) + triple))
    //tripleIndexO = tripleIndexO + (triple.o -> (tripleIndexO.getOrElse(triple.o, Set.empty) + triple))
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
    this.copy(bindings ++ tripleBindings, triple :: triples)

}

final class JoinNode(val leftParent: BetaNode, rightParent: AlphaNode, val spec: List[TriplePattern]) extends BetaNode {

  private val thisPattern = spec.head
  private val parentBoundVariables = spec.drop(1).flatMap(_.variables.keys).toSet

  var tokens: List[Token] = Nil
  var tokenIndex: Map[(Variable, ConcreteNode), Set[Token]] = Map.empty
  //var tokenIndex: mutable.Map[(Variable, ConcreteNode), Set[Token]] = mutable.Map.empty
  //var tokenIndex: mutable.Map[(Variable, ConcreteNode), Set[Token]] = new MyHashMap(1024)

  var children: List[BetaNode] = Nil

  def addChild(node: BetaNode): Unit =
    if (!children.contains(node)) children = node :: children

  def leftActivate(token: Token): Unit = {
    //    println("Left activate: " + token)
    val tokenBoundVariables = token.bindings.keySet
    var valid = true
    var possibleTriples: List[Set[Triple]] = Nil
    if (thisPattern.s.isInstanceOf[Variable]) {
      val v = thisPattern.s.asInstanceOf[Variable]
      if (tokenBoundVariables(v)) {
        rightParent.tripleIndexS.get(token.bindings(v)) match {
          case Some(triples) => possibleTriples = triples :: possibleTriples
          case None          => valid = false
        }
      } else rightParent.triples.toSet :: possibleTriples

    }
    if (valid) {
      if (thisPattern.p.isInstanceOf[Variable]) {
        val v = thisPattern.p.asInstanceOf[Variable]
        if (tokenBoundVariables(v)) {
          rightParent.tripleIndexP.get(token.bindings(v)) match {
            case Some(triples) => possibleTriples = triples :: possibleTriples
            case None          => valid = false
          }
        } else rightParent.triples.toSet :: possibleTriples
      }
    }
    if (valid) {
      if (thisPattern.o.isInstanceOf[Variable]) {
        val v = thisPattern.o.asInstanceOf[Variable]
        if (tokenBoundVariables(v)) {
          rightParent.tripleIndexO.get(token.bindings(v)) match {
            case Some(triples) => possibleTriples = triples :: possibleTriples
            case None          => valid = false
          }
        } else rightParent.triples.toSet :: possibleTriples
      }
    }

    def makeBindings(triple: Triple): Map[Variable, ConcreteNode] = {
      var bindings: Map[Variable, ConcreteNode] = Map.empty
      if (thisPattern.s.isInstanceOf[Variable]) bindings += thisPattern.s.asInstanceOf[Variable] -> triple.s
      if (thisPattern.p.isInstanceOf[Variable]) bindings += thisPattern.p.asInstanceOf[Variable] -> triple.p
      if (thisPattern.o.isInstanceOf[Variable]) bindings += thisPattern.o.asInstanceOf[Variable] -> triple.o
      bindings
    }
    if (valid) {
      val newTriples = possibleTriples.reduce(_ intersect _)
      var tokensToSend: List[Token] = Nil
      for {
        newTriple <- newTriples
        tripleBindings = makeBindings(newTriple)
        newToken = token.extend(tripleBindings, newTriple)
        _ = tokens = newToken :: tokens
        _ = tokensToSend = newToken :: tokensToSend
        binding <- newToken.bindings
      } {
        tokenIndex = tokenIndex + (binding -> (tokenIndex.getOrElse(binding, Set.empty) + newToken))
        //tokenIndex = tokenIndex |+| Map(binding -> Set(newToken))
      }
      activateChildren(tokensToSend)
    }
  }

  def rightActivate(triple: Triple): Unit = {
    //    println("Right activate: " + this + " " + triple)
    var bindings: Map[Variable, ConcreteNode] = Map.empty
    var valid = true
    if (thisPattern.s.isInstanceOf[Variable]) {
      val sv = thisPattern.s.asInstanceOf[Variable]
      bindings = Map(sv -> triple.s)
    }
    if (thisPattern.p.isInstanceOf[Variable]) {
      val pv = thisPattern.p.asInstanceOf[Variable]
      bindings.get(pv) match {
        case Some(node) => valid = node == triple.p
        case None       => bindings += pv -> triple.p
      }
    }
    if (valid && thisPattern.o.isInstanceOf[Variable]) {
      val ov = thisPattern.o.asInstanceOf[Variable]
      bindings.get(ov) match {
        case Some(node) => valid = node == triple.o
        case None       => bindings += ov -> triple.o
      }
    }
    //    println("Bindings: " + bindings)
    if (valid) {
      var tokensToSend: List[Token] = Nil
      val requiredToMatch = bindings.filterKeys(parentBoundVariables)
      if (requiredToMatch.nonEmpty) {
        //        println("need to match")
        val goodTokens = requiredToMatch.map(binding => leftParent.tokenIndex.getOrElse(binding, Set.empty)).reduce(_ intersect _)
        for {
          parentToken <- goodTokens
          newToken = parentToken.extend(bindings, triple)
          _ = tokens = newToken :: tokens
          _ = tokensToSend = newToken :: tokensToSend
          binding <- newToken.bindings
        } {
          tokenIndex = tokenIndex + (binding -> (tokenIndex.getOrElse(binding, Set.empty) + newToken))
          //tokenIndex = tokenIndex |+| Map(binding -> Set(newToken))
        }
      } else {
        //        println("add to all tokens")
        //        println("all tokens: " + leftParent.tokens)
        for {
          parentToken <- leftParent.tokens
          //          _ = println("parent token: " + parentToken)
          newToken = parentToken.extend(bindings, triple)
          //          _ = println("new token: " + newToken)
          _ = tokens = newToken :: tokens
          _ = tokensToSend = newToken :: tokensToSend
          binding <- newToken.bindings
        } {
          tokenIndex = tokenIndex + (binding -> (tokenIndex.getOrElse(binding, Set.empty) + newToken))
          //tokenIndex = tokenIndex |+| Map(binding -> Set(newToken))
        }
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

  private val variableMap: Map[Variable, List[(Variable, Int, TestSpecField)]] = (for {
    (pattern, index) <- rule.body.reverse.zipWithIndex
    (variable, fields) <- pattern.variables
    field <- fields
  } yield (variable, index, field)).groupBy(_._1).map {
    case (variable, uses) => variable -> uses.sortBy(_._2)
  }

  val tokens: List[Token] = List.empty
  val tokenIndex: Map[(Variable, ConcreteNode), Set[Token]] = Map.empty

  def leftActivate(token: Token): Unit = {
    for {
      pattern <- rule.head
    } {
      //FIXME get rid of casting
      engine.processTriple(
        Triple(produceNode(pattern.s, token.triples).asInstanceOf[Resource],
          produceNode(pattern.p, token.triples).asInstanceOf[URI],
          produceNode(pattern.o, token.triples)))
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

  val spec: List[TriplePattern] = Nil

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
