:- module(ontol_bridge_to_serql,[]).

:- use_module(ontol_bridge_to_owl).
:- use_module(serql(rdfql_runtime)).			% runtime tests
:- use_module(library('semweb/rdf_db'),
	      [ rdf_global_id/2,
		rdf_reachable/3,
		rdf_has/3,
		rdf_subject/1,
		rdf_equal/2
	      ]).
:- use_module(library('semweb/rdfs'),
	      [ rdfs_subclass_of/2,
		rdfs_subproperty_of/2
	      ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.  This   one  does  (still a lousy) job
realising RDFS entailment on top of rdf_db.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

term_expansion((rdf(S0, P0, O0) :- Body),
	       (rdf(S,  P,  O)  :- Body)) :-
	rdf_global_id(S0, S),
	rdf_global_id(P0, P),
	rdf_global_id(O0, O).

rdf(S,P,O):- get_triple(S,P,O).
%rdf(bfo:a,bfo:b,bfo:c).
rdf(bfo:a,rdfs:subClassOf,bfo:c).
rdf('http://purl.org/x','http://www.w3.org/2000/01/rdf-schema#subClassOf','http://purl.org/y').


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	serql:entailment/2.

serql:entailment(rdfs, ontol_bridge_to_serql).

