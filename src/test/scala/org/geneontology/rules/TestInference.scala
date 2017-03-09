package org.geneontology.rules

import org.apache.jena.rdf.model.ModelFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.geneontology.jena.OWLtoRules
import org.semanticweb.owlapi.model.parameters.Imports
import scala.collection.JavaConversions._
import org.geneontology.rules.util.Bridge
import org.geneontology.rules.engine.RuleEngine
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner

class TestInference extends UnitSpec {

  "Inferences" should "match Jena output" in {
    val dataModel = ModelFactory.createDefaultModel()
    val manager = OWLManager.createOWLOntologyManager()
    dataModel.read(this.getClass.getResourceAsStream("57c82fad00000639.ttl"), "", "ttl")
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("ro-merged.owl"))
    val jenaRules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true, true)
    val rules = Bridge.rulesFromJena(jenaRules)
    val triples = dataModel.listStatements.map(_.asTriple).map(Bridge.tripleFromJena).toSeq

    val engine = new RuleEngine(rules, true)
    val memory = engine.processTriples(triples)
    val arachneInferredTriples = memory.facts

    arachneInferredTriples.size shouldEqual 611

    val reasoner = new GenericRuleReasoner(jenaRules.toList)
    reasoner.setMode(GenericRuleReasoner.FORWARD_RETE)
    val infModel = ModelFactory.createInfModel(reasoner, dataModel)
    infModel.prepare()
    val jenaInferredTriples = infModel.listStatements().toSet.map(_.asTriple).map(Bridge.tripleFromJena)

    arachneInferredTriples shouldEqual jenaInferredTriples
  }

}