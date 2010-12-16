:- use_module(bio(io)).
:- use_module(bio(tabling)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

/*

  PURPOSE: determine relationships between static and processual metabolic phenotypes

  */

abnormal_level(Ph,ab,C,E) :-
        genus(Ph,'PATO:0000033'), % concentration
        differentium(Ph,qualifier,AQ),
        is_abnormal(AQ),
        differentium(Ph,'OBO_REL:towards',C),
        (   differentium(Ph,'OBO_REL:inheres_in',E)
        ->  true
        ;   E='-'),
        \+ extra_diff(Ph,_,_).


abnormal_process(Ph,ab,C,'-',GProc) :-
        genus(Ph,'PATO:0000001'), % quality
        differentium(Ph,qualifier,AQ),
        is_abnormal(AQ),
        differentium(Ph,'OBO_REL:inheres_in',Proc),
        genus(Proc,GProc),
        differentium(Proc,_,C),
        \+ extra_diff(Ph,_,_).

abnormal_process(Ph,ab,C,'-',Proc) :-
        genus(Ph,'PATO:0000001'), % quality
        differentium(Ph,qualifier,AQ),
        is_abnormal(AQ),
        differentium(Ph,'OBO_REL:inheres_in',Proc),
        id_idspace(Proc,'GO'),
        differentium(Ph,'OBOL:has_central_participant',C).

extra_diff(X,R,Y) :-
        differentium(X,R,Y),
        \+ standard_eqr(R).

standard_eqr('OBO_REL:inheres_in').
standard_eqr('OBO_REL:towards').
standard_eqr('qualifier').

static_processual_1(SP,PP,C,E,GP,R) :-
        abnormal_level(SP,Qual,C,E),
        abnormal_process(PP,Qual,C,E,GP),
        inf_rel(SP,PP,R).

% hdr
static_processual_2('#static_phenotype',processual_phenotype,chemical,static_phenotype_location,processual_phenotype_location,process_type,phenotype_relationship,location_relationship).
static_processual_2(SP,PP,C,SE,PE,GP,R,ER) :-
        abnormal_level(SP,Qual,C,SE),
        abnormal_process(PP,Qual,C,PE,GP),
        inf_rel(SP,PP,R),
        inf_rel(SE,PE,ER).

inf_rel(X,X,=) :- !.
inf_rel(-,_,>).
inf_rel(_,-,<).
inf_rel(SP,PP,<) :- subclass(SP,PP).
inf_rel(SP,PP,>) :- subclass(PP,SP).
inf_rel(SP,PP,<<) :- \+subclass(SP,PP),subclassT(SP,PP).
inf_rel(SP,PP,>>) :- \+subclass(PP,SP),subclassT(PP,SP).

prep_mp :-
        table_pred(ontol_db:subclassT/2),
        load_bioresource(mammalian_phenotype),
        load_bioresource(mammalian_phenotype_xp),
        load_bioresource(go),
        load_bioresource(xchebi),
        load_bioresource(mouse_anatomy),
        load_bioresource(goxp(biological_process_xp_chebi)).

is_abnormal('PATO:0000460').        
