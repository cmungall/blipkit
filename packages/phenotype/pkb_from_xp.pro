:- module(pkb_from_xp,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
:- use_module(bio(tabling)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

/*

  populates phenotype_quad/2 based on ontol_db logical definitions.

  assumes organism_phenotype/2 will be populated separately
  
  */

pkb_db:phenotype_quad(P,(E,Q,D,W)) :-
        genus(P,Q),
        (   differentium(P,'OBO_REL:inheres_in',E)
        ->  true
        ;   E='-'),
        (   differentium(P,Towards,D),towards(Towards)
        ->  true
        ;   D='-'),
        (   differentium(P,'OBO_REL:inheres_in_part_of',W)
        ->  true
        ;   W='-').

pkb_db:phenotype_quad(P,Q) :-
        differentium(P,has_part,P2),
        pkb_db:phenotype_quad(P2,Q).

towards(towards).
towards('OBO_REL:towards').
towards('OBOL:has_central_participant').
