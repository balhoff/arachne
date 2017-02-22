package org.geneontology.rules

import scala.collection.immutable.Seq

final case class Rule(name: Option[String], body: List[TriplePattern], head: Seq[TriplePattern])
