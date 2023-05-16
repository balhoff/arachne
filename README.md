# Arachne

Arachne is a rule engine for RDF. It implements the Rete/UL algorithm as described in [Production Matching for Large Learning Systems](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf) by Robert B. Doorenbos, an extension of [Forgy's](http://dx.doi.org/10.1016/0004-3702%2882%2990020-0) original Rete algorithm.

### Goals
Arachne was developed initially to support reasoning within the [Noctua/Minerva](https://github.com/geneontology/noctua) system. This use case calls for:
* Scalable handling of very large ontologies (millions of axioms/rules).
* Adequate performance, with large rule sets, for smallish datasets of hundreds to thousands of triples (materialize all inferences in <1 second).
* Simultaneous reuse of reasoner, once all rules have been loaded, for any number of datasets, without re-incurring initialization time.

## Usage

```
Usage

 arachne [options] : Command-line operations for Arachne RDF rule engine

Options

   --data           : file or folder of RDF data files
   --export         : export RDF triples to Turtle file
   --indirect-types : mark indirect types with additional triple
   --inferred-only  : export inferred triples only
   --ontology       : OWL ontology to import into reasoning rules
   --rules          : Jena-syntax rules file to import
```

To compute Abox inferences for an instance dataset `data.ttl` which uses classes and properties from some ontology `ont.owl`, a typical command line would be:

```
arachne --ontology=ont.owl --data=data.ttl --export=output.ttl
```

## Building

Install `sbt` (Scala Build Tool) on your system. For Mac OS X, it is easily done using [Homebrew](http://brew.sh):  `brew install sbt`. `sbt` requires a working Java installation, but you do not need to otherwise install Scala.

`sbt compile`
