/* -*- Mode: Prolog -*- */



:- module(goa_bridge_to_class,
          []).

:- use_module(bio(goa_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioprolog_util)).

:- discontiguous
        ontol_db:subclass/2,
        ontol_db:restriction/3.

evidence_skolem(skolem(Role,Code),Role,Code):-
        evidence(Role,Code).

role_to_restriction(Role,restriction(Agent,Relation,Class)):-
        association(Role,Class,Agent),
        agent_role_by_class(Class,Relation).

% make a link for every assoc; note - may be duplicates
ontol_db:restriction(Agent,Relation,Class):-
        role_to_restriction(_,restriction(Agent,Relation,Class)).
ontol_db:reification(StmtID,Term):-
        role_to_restriction(_,Term),
        term_gensym(skolem(Term),'_:',StmtID).
ontol_db:inst_rel(Role,'oban:posits',StmtID):-
        role_to_restriction(Role,Term),
        term_gensym(skolem(Term),'_:',StmtID).
ontol_db:inst_rel(Role,'oban:has_evidence',Evidence):-
        evidence(Evidence,Role,_,_).

% evidence
ontol_db:inst(Ev,''):-
        evidence_skolem(Ev,_,_).
ontol_db:inst_of(Ev,Code):-
        evidence_skolem(Ev,_,Code).
ontol_db:belongs(Ev,annotation):-
        evidence_skolem(Ev,_,_).

% annotation instance
ontol_db:inst(Role,''):-
        association(Role,_,_).
ontol_db:inst_of(Role,'oban:FunctionalAnnotation'):-
        association(Role,_,_).
ontol_db:belongs(Role,annotation):-
        association(Role,_,_).

% evidence instance
ontol_db:inst(Evidence,''):-
        evidence(Evidence,_,_,_).
ontol_db:inst_of(Evidence,Type):-
        evidence(Evidence,_,Code,_),
        evidence_code_to_class(Code,Type).
ontol_db:belongs(Evidence,annotation):-
        evidence(Evidence,_,_,_).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/12/03 00:06:23 $
  @license LGPL

  ---+ Name
  ---++ goa_bridge_to_class
- view go assocs as generic classs

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(goa_bridge_to_class)).
  :- use_module(bio(seqfeature_bridge_to_class)).

  demo:-
    load_bioresource(sofa),
    load_bioresource(go),
    load_biofile(go_assoc,'gene_assoc.fb.tbl'),
    forall(instRT(Annot,'OBD:annotation'),
           write_annot(Annot)).

  write_annot(Annot):-
    restriction(Annot,posits,Stmt),
    reification(Stmt,restriction(Subj,Rel,Obj)),
    class(Subj,SubjName),
    class(Obj,ObjName),
    format('~w ~r ~w~n',SubjName,Rel,ObjName),
    forall(restriction(Annot,has_evidence,Code),
           format('  evidence: ~w~n',Code)).
  
  ==

  ---+ Description

  This maps the goa_db gene ontology association model to a generic
class model with a goa-OWL schema

  - each association is a class-level link between entity class (eg gene) and ontology class

  - the association is reified


 ---++ Command line usage

 exports a gene association file to OWL. Note that 'obo' can also be
used as an option
 
 ==
 blip -f go_assoc -i gene_assoc.fb.tbl -u goa_bridge_to_class -u goa_bridge_to_class io-convert -to owl -o assocs.owl
 ==

 ==
 blip -r so -r go -include "association ontology" -include "feature ontology" -f go_assoc -i lib/Test/data/gene_assoc.fb.tbl io-convert -u goa_bridge_to_class -u seqfeature_bridge_to_class -u ontol_db -to obo
 ==

  
**/