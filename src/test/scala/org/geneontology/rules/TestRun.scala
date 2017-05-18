package org.geneontology.rules

import java.util.Date

import scala.collection.JavaConversions._

import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner
import org.apache.jena.vocabulary.RDF
import org.geneontology.jena.OWLtoRules
import org.geneontology.rules.engine._
import org.geneontology.rules.util.Bridge
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.parameters.Imports
import java.io.FileReader
import java.io.File
import org.geneontology.jena.Explanation
import org.semanticweb.owlapi.model.IRI

object TestRun extends App {

  val dataModel = ModelFactory.createDefaultModel()
  val manager = OWLManager.createOWLOntologyManager()
  dataModel.read(new FileReader(new File("/Users/jim/Documents/Eclipse/rdfox-cli/fb-lego.ttl")), "", "ttl")
  //dataModel.read(this.getClass.getResourceAsStream("57c82fad00000639.ttl"), "", "ttl")
  val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("ro-merged.owl"))
  //val ontology = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go/extensions/go-lego.owl"))
  //val ontology = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go/extensions/go-lego.owl"))
  //val ontology = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go.owl"))
  val jenaRules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, false, true) ++ OWLtoRules.indirectRules(ontology)
  val rules = Bridge.rulesFromJena(jenaRules) //++ indirectRules
  println(s"Rules: ${rules.size}")
  println("Rule sizes: " + rules.map(_.body.size).toSet)
  println(new Date())
  val engine = new RuleEngine(rules, true)
  println("Processed rules: ")
  println(new Date())
  val triples = dataModel.listStatements.map(_.asTriple).map(Bridge.tripleFromJena).toSet
  println(s"Starting triples: ${triples.size}")
  val startTime = System.currentTimeMillis
  val memory = engine.processTriples(triples)
  val endTime = System.currentTimeMillis
  println(s"Reasoned in: ${endTime - startTime} ms")
  println(new Date())
  println(s"Ending triples: ${memory.facts.size}")
  println("Results:::::")
  memory.facts.filter(_.o == URI("http://purl.obolibrary.org/obo/CHEBI_23367")).foreach(println)
  val oneInferred = (memory.facts -- triples).drop(1).head
  val oneInferredJena = dataModel.asStatement(Bridge.jenaFromTriple(oneInferred))
  println(oneInferred)
  println(memory.explain(oneInferred))

  val reasoner = new GenericRuleReasoner(jenaRules.toList)
  reasoner.setMode(GenericRuleReasoner.FORWARD_RETE)
  reasoner.setDerivationLogging(true)
  val infModel = ModelFactory.createInfModel(reasoner, dataModel)
  println("Jena infer model: " + new Date())
  val jStartTime = System.currentTimeMillis
  infModel.prepare()
  val jEndTime = System.currentTimeMillis
  println(s"Jena reasoned in: ${jEndTime - jStartTime} ms")
  println("Jena inferred model: " + new Date())
  println(s"Jena size: ${infModel.size}")
  println("Rule sizes: " + rules.map(_.body.size).toSet)
  println(Explanation.explain(oneInferredJena, infModel))

  val test = Triple(URI("http://model.geneontology.org/57c82fad00000639/57c82fad00000654"), URI("http://purl.obolibrary.org/obo/RO_0000056"), URI("http://model.geneontology.org/57c82fad00000639/57c82fad00000653"))
  println(triples(test))
  println(dataModel.contains(dataModel.asStatement(Bridge.jenaFromTriple(test))))
  println(infModel.contains(dataModel.asStatement(Bridge.jenaFromTriple(test))))

}