package org.geneontology.rules

import org.apache.jena.rdf.model.ModelFactory
import org.geneontology.jena.OWLtoRules
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import java.util.Date
import org.semanticweb.owlapi.model.IRI
import scala.collection.JavaConversions._
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import scala.util.Random
import java.io.FileReader
import java.io.File
import org.semanticweb.owlapi.model.AxiomType
import org.apache.jena.vocabulary.RDF

object TestRun extends App {

  val dataModel = ModelFactory.createDefaultModel()
  val manager = OWLManager.createOWLOntologyManager()
  //dataModel.read(new FileReader(new File("/Users/jbalhoff/Documents/Eclipse/rdfox-cli/fb-lego.ttl")), "", "ttl")
  dataModel.read(this.getClass.getResourceAsStream("57c82fad00000639.ttl"), "", "ttl")
  val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("ro-merged.owl"))
  //val ontology = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go/extensions/go-lego.owl"))
  //val ontology = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go/extensions/go-lego.owl"))
  val indirectRules = for {
    axiom <- ontology.getAxioms(AxiomType.SUBCLASS_OF, Imports.INCLUDED)
    if !axiom.getSubClass.isAnonymous
    if !axiom.getSuperClass.isAnonymous
  } yield {
    Rule(None,
      List(
        TriplePattern(Variable("?x"), URI(RDF.`type`.getURI), URI(axiom.getSuperClass.asOWLClass.getIRI.toString)),
        TriplePattern(Variable("?x"), URI(RDF.`type`.getURI), URI(axiom.getSubClass.asOWLClass.getIRI.toString))),
      List(TriplePattern(Variable("?x"), URI("http://example.org/indirect_type"), URI(axiom.getSuperClass.asOWLClass.getIRI.toString))))
  }
  //val ontology = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go.owl"))
  val jenaRules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true)
  val rules = jenaRules.map(Bridge.ruleFromJena) ++ indirectRules
  println(s"Rules: ${rules.size}")
  println("Rule sizes: " + rules.map(_.body.size).toSet)
  println(new Date())
  val engine = new RuleEngine(rules, true)
  println("Processed rules: ")
  println(new Date())
  val triples = dataModel.listStatements.map(_.asTriple).map(Bridge.tripleFromJena).toVector
  println(s"Starting triples: ${triples.size}")
  val startTime = new Date().getTime
  val memory = engine.processTriples(triples)
  val endTime = new Date().getTime
  println(s"Reasoned in: ${endTime - startTime} ms")
  println(new Date())
  println(s"Ending triples: ${memory.facts.size}")

  val reasoner = new GenericRuleReasoner(jenaRules.toList)
  reasoner.setMode(GenericRuleReasoner.FORWARD_RETE)
  val infModel = ModelFactory.createInfModel(reasoner, dataModel)
  println("Jena infer model: " + new Date())
  val jStartTime = new Date().getTime
  infModel.prepare()
  val jEndTime = new Date().getTime
  println(s"Jena reasoned in: ${jEndTime - jStartTime} ms")
  println("Jena inferred model: " + new Date())
  println(s"Jena size: ${infModel.size}")
  println("Rule sizes: " + rules.map(_.body.size).toSet)

}