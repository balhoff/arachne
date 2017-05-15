package org.geneontology.rules.util

import java.util.{ List => JList }
import java.util.{ Set => JSet }
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.JavaConverters._

import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.geneontology.jena.SesameJena.ontologyAsTriples
import org.geneontology.rules.engine.RuleEngine
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLDataProperty
import org.semanticweb.owlapi.model.OWLDataPropertyExpression
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyChange
import org.semanticweb.owlapi.model.OWLOntologyChangeListener
import org.semanticweb.owlapi.reasoner.BufferingMode
import org.semanticweb.owlapi.reasoner.FreshEntityPolicy
import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy
import org.semanticweb.owlapi.reasoner.InferenceType
import org.semanticweb.owlapi.reasoner.Node
import org.semanticweb.owlapi.reasoner.NodeSet
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.reasoner.impl.NodeFactory
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet
import org.semanticweb.owlapi.util.Version

class ArachneOWLReasoner(ontology: OWLOntology, bufferingMode: BufferingMode, arachne: RuleEngine) extends OWLReasoner {

  private val IndirectType = "http://arachne.geneontology.org/indirect_type"

  private val pendingChanges: AtomicBoolean = new AtomicBoolean(true)

  private object ChangeListener extends OWLOntologyChangeListener {

    override def ontologiesChanged(changes: JList[_ <: OWLOntologyChange]): Unit = {
      pendingChanges.set(true)
      if (bufferingMode == BufferingMode.NON_BUFFERING) flush()
    }

  }

  ontology.getOWLOntologyManager.addOntologyChangeListener(ChangeListener)

  private val model: Model = ModelFactory.createDefaultModel()

  flush()

  override def dispose(): Unit = {
    ontology.getOWLOntologyManager.removeOntologyChangeListener(ChangeListener)
  }

  override def flush(): Unit = {
    if (pendingChanges.getAndSet(false)) {
      val asserted = ontologyAsTriples(ontology)
      val wm = arachne.processTriples(asserted.map(_.asTriple).map(Bridge.tripleFromJena))
      model.synchronized {
        model.removeAll()
        model.add(wm.facts.map(t => model.asStatement(Bridge.jenaFromTriple(t))).toSeq.asJava)
      }
    }
  }

  override def getDifferentIndividuals(ind: OWLNamedIndividual): NodeSet[OWLNamedIndividual] = {
    val query = s"""
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?o
      WHERE {
        <${ind.getIRI}> owl:differentFrom ?o .
        FILTER(isIRI(?o))
      }      
    """
    val inds: Iterable[Node[OWLNamedIndividual]] = executeSelect(query).map(res => NodeFactory.getOWLNamedIndividualNode(Individual(res.getResource("o").getURI)))
    new OWLNamedIndividualNodeSet(inds.toSet.asJava)
  }

  override def getInstances(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLNamedIndividual] = ce match {
    case Class(iri) =>
      val query = if (direct) {
        s"""
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT DISTINCT ?s
        WHERE {
          ?s rdf:type <$iri>  .
          FILTER(isIRI(?s))
          FILTER NOT EXISTS {
            ?s <$IndirectType> <$iri> . 
          }
        }      
        """
      } else {
        s"""
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT DISTINCT ?s
        WHERE {
          ?s rdf:type <$iri>  .
          FILTER(isIRI(?s))
        }      
        """
      }
      val inds: Iterable[Node[OWLNamedIndividual]] = executeSelect(query).map(res => NodeFactory.getOWLNamedIndividualNode(Individual(res.getResource("s").getURI)))
      new OWLNamedIndividualNodeSet(inds.toSet.asJava)
    case _ => ???
  }

  override def getObjectPropertyValues(ind: OWLNamedIndividual, pe: OWLObjectPropertyExpression): NodeSet[OWLNamedIndividual] = {
    val predicate = pe match {
      case ObjectProperty(iri)                  => s"<$iri>"
      case ObjectInverseOf(ObjectProperty(iri)) => s"^<$iri>"
    }
    val query = s"""
      SELECT DISTINCT ?o
      WHERE {
        <${ind.getIRI}> $predicate ?o .
        FILTER(isIRI(?o))
      }      
    """
    val inds: Iterable[Node[OWLNamedIndividual]] = executeSelect(query).map(res => NodeFactory.getOWLNamedIndividualNode(Individual(res.getResource("o").getURI)))
    new OWLNamedIndividualNodeSet(inds.toSet.asJava)
  }

  override def getDataPropertyValues(ind: OWLNamedIndividual, dp: OWLDataProperty): JSet[OWLLiteral] = {
    val factory = OWLManager.getOWLDataFactory
    val query = s"""
      SELECT DISTINCT ?o
      WHERE {
        <${ind.getIRI}> <${dp.getIRI}> ?o .
        FILTER(isLiteral(?o))
      }      
    """
    (for {
      res <- executeSelect(query)
      literal = res.getLiteral("o") //FIXME check if literal has datatype
    } yield factory.getOWLLiteral(
      literal.getLexicalForm,
      factory.getOWLDatatype(IRI.create(literal.getDatatypeURI))))
      .toSet.asJava
  }

  override def getReasonerName(): String = "arachne-owl-reasoner"

  override def getReasonerVersion(): Version = ???

  override def getRootOntology(): OWLOntology = ontology

  override def getSameIndividuals(ind: OWLNamedIndividual): Node[OWLNamedIndividual] = {
    val query = s"""
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?o
      WHERE {
        <${ind.getIRI}> owl:sameAs ?o .
        FILTER(isIRI(?o))
      }      
      """
    val inds = executeSelect(query).map(res => Individual(res.getResource("o").getURI))
    NodeFactory.getOWLNamedIndividualNode((inds.toSet + ind).asJava)
  }

  override def getSubClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] = ce match {
    case Class(iri) => {
      val query = if (direct) {
        s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?s
      WHERE {
        ?s rdfs:subClassOf <$iri> .
        FILTER(isIRI(?s))
      }      
      """
      } else {
        s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?s
      WHERE {
        ?s rdfs:subClassOf+ <$iri> .
        FILTER(isIRI(?s))
      }      
      """
      }
      val classes: Iterable[Node[OWLClass]] = executeSelect(query).map(res => NodeFactory.getOWLClassNode(Class(res.getResource("s").getURI)))
      new OWLClassNodeSet(classes.toSet.asJava)
    }
    case _ => ???
  }

  override def getSubDataProperties(dp: OWLDataProperty, direct: Boolean): NodeSet[OWLDataProperty] = ???

  override def getSubObjectProperties(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLObjectPropertyExpression] = ???

  override def getSuperClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] = ce match {
    case Class(iri) => {
      val query = if (direct) {
        s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?o
      WHERE {
        <$iri> rdfs:subClassOf ?o .
        FILTER(isIRI(?o))
      }      
      """
      } else {
        s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?o
      WHERE {
        <$iri> rdfs:subClassOf+ ?o .
        FILTER(isIRI(?o))
      }      
      """
      }
      val classes: Iterable[Node[OWLClass]] = executeSelect(query).map(res => NodeFactory.getOWLClassNode(Class(res.getResource("o").getURI)))
      new OWLClassNodeSet(classes.toSet.asJava)
    }
    case _ => ???
  }

  override def getSuperDataProperties(dp: OWLDataProperty, direct: Boolean): NodeSet[OWLDataProperty] = ???

  override def getSuperObjectProperties(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLObjectPropertyExpression] = ???

  override def getTimeOut(): Long = Long.MaxValue

  override def getTopClassNode(): Node[OWLClass] = ???

  override def getTopDataPropertyNode(): Node[OWLDataProperty] = ???

  override def getTopObjectPropertyNode(): Node[OWLObjectPropertyExpression] = ???

  override def getBottomClassNode(): Node[OWLClass] = ???

  override def getBottomDataPropertyNode(): Node[OWLDataProperty] = ???

  override def getBottomObjectPropertyNode(): Node[OWLObjectPropertyExpression] = ???

  override def getBufferingMode(): BufferingMode = bufferingMode

  override def getTypes(ind: OWLNamedIndividual, direct: Boolean): NodeSet[OWLClass] = {
    val query = if (direct) {
      s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?o
      WHERE {
        <${ind.getIRI}> rdf:type ?o .
        FILTER(isIRI(?o))
        FILTER(?o != owl:NamedIndividual)
        FILTER NOT EXISTS {
          <${ind.getIRI}> <$IndirectType> ?o . 
        }
      }      
      """
    } else {
      s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      SELECT DISTINCT ?o
      WHERE {
        <${ind.getIRI}> rdf:type ?o .
        FILTER(isIRI(?o))
        FILTER(?o != owl:NamedIndividual)
      }      
      """
    }
    val classes: Iterable[Node[OWLClass]] = executeSelect(query).map(res => NodeFactory.getOWLClassNode(Class(res.getResource("o").getURI)))
    new OWLClassNodeSet(classes.toSet.asJava)
  }

  override def interrupt(): Unit = ()

  override def isConsistent(): Boolean = {
    val query = s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      ASK
      WHERE {
        ?s rdf:type owl:Nothing .
      }      
      """
    !executeAsk(query)
  }

  override def isEntailed(axiom: OWLAxiom): Boolean = {
    val pattern = axiom match {
      case ClassAssertion(_, Class(cls), NamedIndividual(ind)) =>
        s"<$ind> rdf:type <$cls> ."
      case ObjectPropertyAssertion(_, ObjectProperty(pred), NamedIndividual(subj), NamedIndividual(obj)) =>
        s"<$subj> <$pred> <$obj> ."
      case ObjectPropertyAssertion(_, ObjectInverseOf(ObjectProperty(pred)), NamedIndividual(subj), NamedIndividual(obj)) =>
        s"<$subj> ^<$pred> <$obj> ."
      case _ => ???
    }
    val query = s"""
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>      
      PREFIX owl: <http://www.w3.org/2002/07/owl#>      
      ASK 
      WHERE {
        $pattern
      }      
      """
    executeAsk(query)
  }

  override def isEntailed(axioms: JSet[_ <: OWLAxiom]): Boolean = axioms.asScala.forall(isEntailed) //TODO this could be faster as a single query

  override def precomputeInferences(infType: InferenceType*): Unit = ()

  def executeSelect(query: String): Iterable[QuerySolution] = {
    val execution = QueryExecutionFactory.create(QueryFactory.create(query), model)
    val result = execution.execSelect.asScala.toVector
    execution.close()
    result
  }

  def executeConstruct(query: String): Model = {
    val execution = QueryExecutionFactory.create(QueryFactory.create(query), model)
    val result = execution.execConstruct()
    execution.close()
    result
  }

  def executeAsk(query: String): Boolean = {
    val execution = QueryExecutionFactory.create(QueryFactory.create(query), model)
    val result = execution.execAsk()
    execution.close()
    result
  }

  override def getDataPropertyDomains(arg0: OWLDataProperty, arg1: Boolean): NodeSet[OWLClass] = ???

  override def getDisjointClasses(arg0: OWLClassExpression): NodeSet[OWLClass] = ???

  override def getDisjointDataProperties(arg0: OWLDataPropertyExpression): NodeSet[OWLDataProperty] = ???

  override def getDisjointObjectProperties(arg0: OWLObjectPropertyExpression): NodeSet[OWLObjectPropertyExpression] = ???

  override def getEquivalentClasses(arg0: OWLClassExpression): Node[OWLClass] = ???

  override def getEquivalentDataProperties(arg0: OWLDataProperty): Node[OWLDataProperty] = ???

  override def getEquivalentObjectProperties(arg0: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] = ???

  override def getFreshEntityPolicy(): FreshEntityPolicy = ???

  override def getIndividualNodeSetPolicy(): IndividualNodeSetPolicy = ???

  override def getInverseObjectProperties(ope: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] = ???

  override def getObjectPropertyDomains(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLClass] = ???

  override def getObjectPropertyRanges(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLClass] = ???

  override def getPendingAxiomAdditions(): JSet[OWLAxiom] = ???

  override def getPendingAxiomRemovals(): JSet[OWLAxiom] = ???

  override def getPendingChanges(): JList[OWLOntologyChange] = ???

  override def getPrecomputableInferenceTypes(): JSet[InferenceType] = ???

  override def isEntailmentCheckingSupported(axiomType: AxiomType[_]): Boolean = ???

  override def isPrecomputed(inferenceType: InferenceType): Boolean = ???

  override def isSatisfiable(ce: OWLClassExpression): Boolean = ???

  override def getUnsatisfiableClasses(): Node[OWLClass] = ???

}