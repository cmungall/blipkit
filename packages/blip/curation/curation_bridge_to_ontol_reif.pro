/* -*- Mode: Prolog -*- */
:- module(curation_bridge_to_ontol_reif,
          []).

:- use_module(bio(curation_bridge_to_ontol_core)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

ontol_db:reification(StmtID,Term):-
        curation2term(_,Term),
        term_gensym_stable(skolem(Term),'_:',StmtID).

ontol_db:inst(A):-
        curation(A).
% one curation can posit many statements
ontol_db:inst_rel(A,'oban:posits',StmtID):-
        curation2term(A,Term),
        term_gensym_stable(skolem(Term),'_:',StmtID).
ontol_db:inst_of(A,'oban:Annotation'):-
        curation(A).
% treat all annotations as anonymous for now
ontol_db:is_anonymous(A):-
        curation(A).

% evidence
ontol_db:inst_rel(A,'oban:has_evidence',E):-
        curation_evidence(A,E).
ontol_db:inst(E):-
        evidence_type(E,_).
ontol_db:inst_of(E,T):-
        evidence_type(E,T).
ontol_db:inst_rel(E,'oban:with',X):-
        evidence_with(E,X).

% we do not need the following: implicit in the reification
%ontol_db:inst_of(StmtID,'rdf:Statement'):-
%        curation2term(_,Term),
%        term_gensym_stable(skolem(Term),'_:',StmtID).
%ontol_db:inst(StmtID):-
%        curation2term(_,Term),
%        term_gensym_stable(skolem(Term),'_:',StmtID).
%ontol_db:is_anonymous(StmtID):-
%        curation2term(_,Term),
%        term_gensym_stable(skolem(Term),'_:',StmtID).

term_gensym_stable(Term,Prefix,Atom):-
        term_gensym_stable(Term,Atom0),
        concat_atom(L,':',Atom0),
        concat_atom(L,'_',Atom1),
        atom_concat(Prefix,Atom1,Atom).
term_gensym_stable(Atom,Atom):- atom(Atom),!.
term_gensym_stable(Term,Atom):-
        Term=..L,
        maplist(term_gensym_stable,L,L2),
        concat_atom(L2,'__',Atom).
