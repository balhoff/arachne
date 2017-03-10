[![Build Status](https://travis-ci.org/balhoff/arachne.svg?branch=master)](https://travis-ci.org/balhoff/arachne)

# Arachne

Arachne is a rule engine for RDF. The implementation is largely based on [Forgy's](http://dx.doi.org/10.1016/0004-3702%2882%2990020-0) Rete algorithm as described in [Production Matching for Large Learning Systems](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf) by Robert B. Doorenbos.

### Goals
Arachne was developed initially to support reasoning within the [Noctua/Minerva](https://github.com/geneontology/noctua) system. This use case calls for:
* Scalable handling of very large ontologies (millions of axioms/rules).
* Adequate performance, with large rule sets, for smallish datasets of hundreds to thousands of triples (materialize all inferences in <1 second).
* Simultaneous reuse of reasoner, once all rules have been loaded, for any number of datasets, without re-incurring initialization time.

## Building

Install `sbt` (Scala Build Tool) on your system. For Mac OS X, it is easily done using [Homebrew](http://brew.sh):  `brew install sbt`. `sbt` requires a working Java installation, but you do not need to otherwise install Scala.

`sbt compile`
