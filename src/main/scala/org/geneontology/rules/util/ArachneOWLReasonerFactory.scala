package org.geneontology.rules.util

import org.geneontology.jena.OWLtoRules
import org.geneontology.rules.engine.Rule
import org.geneontology.rules.engine.RuleEngine
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.BufferingMode
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

/**
 * Creates OWLReasoners which use the given rules (or ontology translated to rules) for materializing
 * inferred triples from input ontologies. Somewhat hacky and should be used with care.
 */
class ArachneOWLReasonerFactory(rules: Set[Rule]) extends OWLReasonerFactory {

  val arachne = new RuleEngine(rules, true)

  def this(ontology: OWLOntology) = this(Bridge.rulesFromJena(OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true, true)).toSet ++
    Bridge.rulesFromJena(OWLtoRules.indirectRules(ontology)))

  def dispose(): Unit = ()

  override def createNonBufferingReasoner(ont: OWLOntology): OWLReasoner = new ArachneOWLReasoner(ont, BufferingMode.NON_BUFFERING, arachne)

  override def createNonBufferingReasoner(ont: OWLOntology, config: OWLReasonerConfiguration): OWLReasoner = new ArachneOWLReasoner(ont, BufferingMode.NON_BUFFERING, arachne)

  override def createReasoner(ont: OWLOntology): OWLReasoner = new ArachneOWLReasoner(ont, BufferingMode.BUFFERING, arachne)

  override def createReasoner(ont: OWLOntology, config: OWLReasonerConfiguration): OWLReasoner = new ArachneOWLReasoner(ont, BufferingMode.BUFFERING, arachne)

  override def getReasonerName(): String = "arachne-owl-reasoner"

}