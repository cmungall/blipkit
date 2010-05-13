/* -*- Mode: Prolog -*- */



:- module(ontol_materialize_reification,
          []).
:- use_module(bio(ontol_db)).


ontol_db:belongs(Stmt,'_'):- reification(Stmt,_).
ontol_db:inst(Stmt,''):- reification(Stmt,_).
ontol_db:inst_of(Stmt,'rdf:Statement'):- reification(Stmt,_).
ontol_db:inst_rel(Stmt,'rdf:subject',Subj):- reification(Stmt,restriction(Subj,_,_)).
ontol_db:inst_rel(Stmt,'rdf:predicate',Pred):- reification(Stmt,restriction(_,Pred,_)).
ontol_db:inst_rel(Stmt,'rdf:object',Obj):- reification(Stmt,restriction(_,_,Obj)).
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ ontol_materialize_reification
- 

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_materialize_reification)).

  ==

  ---+ Description

  see reification/2

  reifications are first class data entities in the ontol_db
module. When writing to obo and owl we must turn the reification into
an instance of rdf:Statement

**/