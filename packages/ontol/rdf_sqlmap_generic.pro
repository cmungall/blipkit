:- module(rdf_sqlmap_generic,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).

:- load_schema_defs(bio('sql_schema/schema_obd')).

:- multifile
	system:term_expansion/2.

rdf(I,rdf:type,schema:R):-
        getrdb(Rdb),
        sql_compile(relation(R,Arity)),
        functor(Goal,R,Arity),
        rdb_query(Rdb,Head,Head))]):- !.
rdb_query(

% INTERNAL
link_asserted(Node,Pred,Obj) <- link(_,_,Node,Pred,Obj,f,_). % asserted
link(Node,Pred,Obj,Implied)  <- link(_,_,Node,Pred,Obj,Implied,_). 
node(IID,ID,N,T) <- node(IID,ID,N,T,_,_,'f',_,_,_).
ontol_db:parent0(X,R,Y) <- node(XI,X,_,'C'),link_asserted(XI,RI,YI),node(RI,R,_,_),node(YI,Y,_,'C').
node_desc(ID,ScopeIID,TypeIID,Label) <- node(IID,ID,_,_,_,_,_,_,_,_),description(_,IID,ScopeIID,TypeIID,Label).



% METADATA
metadata_db:entity_label(ID,N) <- node(_,ID,N,_).
metadata_db:entity_synonym(ID,N) <- node(IID,ID,_,_),alias(IID,_,_,N).

% ONTOL
ontol_db:class(ID) <- node(_,ID,_,'C').
ontol_db:class(ID,N) <- node(_,ID,N,'C').

ontol_db:property(ID) <- node(_,ID,_,'R').
ontol_db:property(ID,N) <- node(_,ID,N,'R').
ontol_db:inst(ID) <- node(_,ID,_,'I').
ontol_db:inst(ID,N) <- node(_,ID,N,'I').
ontol_db:entity_uri(ID,URI) <- node(_,ID,_,_,_,_,_,_,_,URI).

:- abolish(ontol_db:parent/2).
ontol_db:parent(X,Y) <- node(XI,X,_,'C'),link_asserted(XI,_,YI),node(YI,Y,_,'C').

:- abolish(ontol_db:parent/3). % map to internal
ontol_db:parent(X,subclass,Y):- ontol_db:parent0(X,'OBO_REL:is_a',Y).
ontol_db:parent(X,R,Y):- ontol_db:parent0(X,R,Y),R\='OBO_REL:is_a'.

:- abolish(ontol_db:parentT/3). % TODO is_a/subclass
ontol_db:parentT(X,R,Y) <- node(XI,X,_,'C'),link(XI,RI,YI),node(RI,R,_,'C'),node(YI,Y,_,'C').

:- abolish(ontol_db:subclass/2).
ontol_db:subclass(X,Y) <- parent(X,'OBO_REL:is_a',Y).
ontol_db:restriction(X,R,Y) <- parent(X,R,Y),not(R='OBO_REL:is_a').
ontol_db:inst_of(X,Y) <- parent(X,'OBO_REL:instance_of',Y).
ontol_db:subclassT(X,Y) <- node(XI,X,_,'C'),link(XI,RI,YI,_),node(RI,'OBO_REL:is_a',_,_),node(YI,Y,_,'C').
ontol_db:noparent(X) <- class(X),not(parent(X,_)).

ontol_db:def(X,Label) <- node_desc(X,_,TypeIID,Label),node(TypeIID,definition,_,_).

lookup_class(search(S,_),ID):-
        foo.


getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

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
                rdf_sqlmap_generic:test_inline),
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
%  rdf_sqlmap_generic

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_db)).
  :- use_module(bio(rdf_sqlmap_generic)).
  
  ==

  ---+ Description

  This is a direct bridge from an sql database to rdf/owl;   bypassed ontol_db (for efficiency)

  

  ---+ See Also


  
  */

