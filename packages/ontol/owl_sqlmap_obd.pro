:- module(owl_sqlmap_obd,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).

:- load_schema_defs(bio('sql_schema/schema_obd')).
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

:- multifile
	system:term_expansion/2.

term_expansion((rdf(S0, P0, O0) :- Body),
	       (rdf(S,  P,  O)  :- Body)) :-
	rdf_global_id(S0, S),
	rdf_global_id(P0, P),
	rdf_global_id(O0, O).

% INTERNAL
link_asserted(Node,Pred,Obj) <- link(_,_,Node,Pred,Obj,f,_). % asserted
link(Node,Pred,Obj,Implied)  <- link(_,_,Node,Pred,Obj,Implied,_). 
node(IID,URI,N,T) <- node(IID,_,N,T,_,_,'f',_,_,URI).
node(IID,URI,N,T,ID) <- node(IID,ID,N,T,_,_,'f',_,_,URI).
%ontol_db:parent0(X,R,Y) <- node(XI,X,_,'C'),link_asserted(XI,RI,YI),node(RI,R,_,_),node(YI,Y,_,'C').
%node_desc(ID,ScopeIID,TypeIID,Label) <- node(IID,ID,_,_,_,_,_,_,_,_),description(_,IID,ScopeIID,TypeIID,Label).

rdf(S,rdfs:label,literal(N)):-
        getrdb(Rdb),
        Q=node(_,_,N,_,_,_,_,_,_,S),
        rdb_query(Rdb,S-N,Q).
rdf(S,rdfs:subClassOf,O):-
        getrdb(Rdb),
        Q=(node(S1,S,_,_),
           node(O1,O,_,_),
           node(P1,_,_,_,'OBO_REL:is_a'),
           link_asserted(S1,_,O1)), 
        rdb_query(Rdb,S-O,Q).


getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	serql:entailment/2.

serql:entailment(rdfs, owl_sqlmap_obd).


% ----------------------------------------
% TESTS
% ----------------------------------------
unittest:testq(id(ID),(entity_label(ID,molecular_function))).
unittest:testq(child(Y,XN),(entity_label(Y,apoptosis),parent(X,Y),entity_label(X,XN))).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                rdb_connect(Rdb,obd2),
                forall(testq(Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           nl))),
            true)).


unittest(test(inline,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                owl_sqlmap_obd:test_inline),
            true)).

test_inline:-
        ensure_loaded(bio(ontol_db)),
        rdb_connect(Rdb,obd2),
        setrdb(Rdb),
        forall(class(X,molecular_function),
               writeln(id=X)).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.10 $
  @date  $Date: 2005/09/30 16:23:45 $
  @license LGPL

  ---+ Name
%  owl_sqlmap_obd

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_db)).
  :- use_module(bio(owl_sqlmap_obd)).
  
  ==

  ---+ Description

  This is a direct bridge from an sql database to rdf/owl;   bypassed ontol_db (for efficiency)

  ==
blip -debug serql -debug sql -r rdb/obdtest -u ontol_sqlmap_obd -u owl_sqlmap_obd ontol-serql 'SELECT * FROM {X} rdfs:label {Y} WHERE label(Y)="apoptosis"'
  ==
  

  ---+ See Also


  
  */

