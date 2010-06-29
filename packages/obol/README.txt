---+ Obol - Open Bio Ontologies Language

---++ Abstract


Obol is a collection of grammars for parsing and generating logical
class expressions from controlled natural language.

For example, given the term "germ cell nucleus", this can be parsed as
the class expression *nucleus* ∩ part_of(*germ_cell*). Conversely,
given the previous class expression, biologist-friendly controlled
terms can be generated (as well as synonyms and textual definitions).

Obol differs from projects such as ACE and Rabbit in that it does not
seek to have a single generic transform for all FOL or OWL
expressions. Instead domain-specific grammars are constructed that
make use of community naming conventions resulting in more intuitive
terms

---++ Examples

The following rule parses terms like "germ cell nucleus":

==
cellular_component(CC that part_of(Cell)) --> cell(Cell),cellular_component(CC).
==

Rules can be recursive allowing for the parsing of complex nested terms such as:

* active evasion of immune response of other organism via regulation
  of antigen processing and presentation in other organism during
  symbiotic interaction
  http://amigo.geneontology.org/cgi-bin/amigo/term-details.cgi?term=GO:0051813

---++ Using Obol

The obol script serves as a general wrapper to the capabilities of the library

For help:

==
obol -h
==

The following Makefile is used for GO http://www.geneontology.org/scratch/obol_results/Makefile

---++ Organization

The module classdef_parser.pro does most of the work.

The individual grammars are named obol_<domain1>_xp_<domain2>, which
defines terms in domain1 using classes from domain2. For example
obol_cellular_component_xp_cell.pro has rules for defining cellular
component classes such as "germ cell nucleus" using cell types such as
"germ cell".

Browsing the source is the recommended way to explore these.

---++ Web Service

Obol can also be run as a web service. The current web service is not actively maintained

---++ Prerequisites

http://www.blipkit.org/

---++ Further info

http://wiki.geneontology.org/index.php/Obol


