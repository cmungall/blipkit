:- module(phenotype_db,
          [
           feature_phenotype/2,
           feature_phenotype_tag_value/4,
           
           inferred_feature_phenotype/2,
           inferred_feature_lca_phenotype/2,
           feature_with_phenotype/1,
           feature_count/1,
           phenotype_subsumed_by/2,
           phenotype_lca/3,
           phenotype_frequency/2,
           atomic_subsumed_by_lca/3,
           method_feature_pair_phenosim/4,
	   feature_pair_phenosim_value/4,
           feature_phenotype_compare_best/6,
           feature_phenotype_compare/5,
           feature_phenotype_compare_nr/5,
           feature_phenotype_compare/6
           ]).

% DEPRECATED

:- use_module(bio(dbmeta)).
:- use_module(bio(ontol_db)).
:- use_module(bio(simmatrix)).
:- use_module(bio(tabling)).
:- use_module(bio(bioprolog_util),[solutions/3]).

% TODO: use same model as pkb_db, i.e. organism_phenotype
:- extensional(feature_phenotype/2).

%% inferred_feature_phenotype(?F,?P) is nondet
% true if F has a phenotype subsumed by P
inferred_feature_phenotype(F,P) :-
        feature_phenotype(F,P1),
        phenotype_subsumed_by(P1,P). % (+,?)
:- table_pred(inferred_feature_phenotype/2).


%% feature_phenotype_tag_value(?F,?P,?Tag,?Val)
:- extensional(feature_phenotype_tag_value/4).

%% inferred_feature_lca_phenotype(?F,?P) is nondet
% true if F has a phenotype subsumed by P,
% and P is the LCA of two asserted phenotypes
inferred_feature_lca_phenotype(F,P) :-
        feature_phenotype(F,P1),
        feature_phenotype(_,P2),
        phenotype_lca(P1,P2,P).

%% phenotype_frequency(+P,?Num)
% unifies Num with the number of occurrences of P (includes inferred phenotypes)
phenotype_frequency(P,Num) :-
        aggregate(count,F,inferred_feature_phenotype(F,P),Num),
        debug(phenotype,'freq(~w) =~w',[P,Num]).
:- table_pred(phenotype_frequency/2).

%% feature_count(?Num)
% unifies Num with the number of features that have at least one phenotype.
feature_count(Num) :-
        aggregate(count,F,P^feature_phenotype(F,P),Num).
:- table_pred(feature_count/1).

%% feature_with_phenotype(?F) is nondet
% true if F is a feature, and F has at least one phenotype.
% succeeds uniquely for each feature
feature_with_phenotype(F) :-
        setof(F,P^feature_phenotype(F,P),Fs),
        member(F,Fs).
:- table_pred(feature_with_phenotype/1).

%% phenotype_pval(?P,?PVal) is nondet
% probability of a feature having P (including inferred phenotypes)
phenotype_pval(P,PVal) :-
        phenotype_frequency(P,Freq),
        feature_count(Num),
        PVal is Freq/Num.

%% phenotype_ic(?P,?IC)
% shannon information content of a phenotype. IC = -log[2](p(P)).
phenotype_ic(P,IC) :-
        phenotype_pval(P,PVal),
        IC is -log(PVal)/log(2).
:- table_pred(phenotype_ic/2).
        

%% phenotype_subsumed_by(+PhenotypeChild,?PhenotypeParent) is nondet
% 
% true if Child is subsumed by Parent based on phenotype reasoning rules.
% this is reflexive, so always true if Parent=Child
% 
% Phenotype = (E,Q,D,W)
% 
% relies on pre-reasoned reflexive transitive closure of class subsumption in subclass/2.
phenotype_subsumed_by((E,Q,D,W),(E2,Q2,D2,W2)) :-
        atomic_subsumed_by(E,E2),
        atomic_subsumed_by(Q,Q2),
        atomic_subsumed_by(D,D2),
        entity_whole_part_of(E,W,W2).

%% phenotype_ca(+PhenotypeChild1,+PhenotypeChild2,?PhenotypeCommonAncestor) is nondet
% true if Child1 and Child2 are both subsumed by CA.
% reflexive, so if Child1<Child2, then Child2 will be a CA
phenotype_ca((E1,Q1,D1,W1),(E2,Q2,D2,W2),(E,Q,D,W)) :-
        atomic_subsumed_by_ca(E1,E2,E),
        atomic_subsumed_by_ca(Q1,Q2,Q),
        atomic_subsumed_by_ca(D1,D2,D),
        entity_whole_part_of_ca((E1,W1),(E2,W2),W).

%% phenotype_lca(+PhenotypeChild1,+PhenotypeChild2,?PhenotypeLeastCommonAncestor) is nondet
% true if the least common subsumer of Child1 and Child2 is LCA.
% reflexive.
phenotype_lca((E1,Q1,D1,W1),(E2,Q2,D2,W2),(E,Q,D,W)) :-
        atomic_subsumed_by_lca(E1,E2,E),
        atomic_subsumed_by_lca(Q1,Q2,Q),
        atomic_subsumed_by_lca(D1,D2,D),
        entity_whole_part_of_lca((E1,W1),(E2,W2),W).

% pre-coordinated rule, for atomic phenotypes
phenotype_lca(X,Y,A) :-
        atom(X),
        atom(Y),
	debug(phenotype,'Finding LCA of ~w -vs- ~w',[X,Y]),
        subclass(X,A),
        subclass(Y,A),
        \+ ((subclass(X,A2),
             A2\=A,
             subclass(Y,A2),
             subclass(A2,A))).

% CA of X,Y where both are atoms (pre-coordinated).
% used by phenotype_ca/3 on each element of the phenotype description.
atomic_subsumed_by_ca(X,X,X).
atomic_subsumed_by_ca(X,Y,A) :-
        atomic_subsumed_by(X,A),
        atomic_subsumed_by(Y,A).
:- table_pred(atomic_subsumed_by_ca/3).


% LCA of X,Y where both are atoms (pre-coordinated)
% used by phenotype_lca/3 on each element of the phenotype description.
atomic_subsumed_by_lca(X,Y,A) :-
        atomic_subsumed_by_ca(X,Y,A),
        \+ ((atomic_subsumed_by_ca(X,Y,A2),
             A2\=A,
             atomic_subsumed_by(A2,A))).
:- table_pred(atomic_subsumed_by_lca/3).

%% atomic_subsumed_by(+Child,?Parent) is nondet
% as subclass/2, but also includes null/top parent (-)
atomic_subsumed_by(-,-) :- !.
atomic_subsumed_by(X,Y) :- subclass(X,Y). % pre-reasoned
%atomic_subsumed_by(X,thing) :- class(X).
%atomic_subsumed_by(_X,thing).
%atomic_subsumed_by(thing,thing).
%atomic_subsumed_by((-),(-)).
atomic_subsumed_by(_,(-)).

%% entity_whole_part_of(+E,+W,?WP)
% example:
%  entity_whole_part_of(retina,-,WP). WP=eye;WP=head;WP=body;WP=organ
%  entity_whole_part_of(retina,eye,WP). WP=eye;WP=head;WP=body;WP=organ
entity_whole_part_of(E,_,WP) :-  affects(E,WP).
entity_whole_part_of(_,W,WP) :-  affects(W,WP).
entity_whole_part_of(_,W,W).
entity_whole_part_of(_,_,(-)).

entity_whole_part_of_ca((E1,W1),(E2,W2),A) :-
        entity_whole_part_of(E1,W1,A1),
	atomic_subsumed_by(A1,A),
        entity_whole_part_of(E2,W2,A2),
	atomic_subsumed_by(A2,A).

%% entity_whole_part_of_lca(+(EX,WX),+(EY,WY),?A)
%
entity_whole_part_of_lca(X,Y,A) :-
        entity_whole_part_of_ca(X,Y,A),
        \+ ((entity_whole_part_of_ca(X,Y,A2),
             A2\=A,
             (   affects(A2,A)
             ;   atomic_subsumed_by(A2,A)))).

%% feature_phenotype_compare(+F,?P,+F2,?P2,?PSub)
% given two features (F, F2), find a phenotype possessed by each (P, P2),
% and the LCA of those two phenotypes (P).
feature_phenotype_compare(F,P,F2,P2,PSub) :-
        feature_phenotype(F,P),
        feature_phenotype(F2,P2),
        phenotype_lca(P,P2,PSub).

%% feature_phenotype_compare(+F,?P,+F2,?P2,?PSub,?Freq)
% as feature_phenotype_compare/5, and in addition unify Freq with the Frequence of occurrences of
% the common subsuming phenotype, PSub
feature_phenotype_compare(F,P,F2,P2,PSub,Freq) :-
        feature_phenotype_compare(F,P,F2,P2,PSub),
        phenotype_frequency(PSub,Freq).

% used??
feature_phenotype_compare_best(F,P,F2,P2,PSub,Freq) :-
        feature_phenotype_compare(F,P,F2,P2,PSub),
        phenotype_frequency(PSub,Freq),
        \+ ((feature_phenotype(F2,P2X),
             %P2X\=P2,
             phenotype_lca(P,P2X,PSubX),
             PSubX\=PSub,
             % neither of these conditions must be satisfied
             (   phenotype_subsumed_by(PSubX,PSub)
             ->  true
             ;   phenotype_frequency(PSubX,FreqX),
                 FreqX < Freq))).

% @Deprecated
feature_phenotype_compare_nr(F,P,F2,P2,PSub) :-
        feature_phenotype_compare(F,P,F2,P2,PSub),
        \+ ((feature_phenotype(F2,P2X),
             P2X\=P2,
             phenotype_lca(P,P2X,PSubX),
             PSubX\=PSub,
             phenotype_subsumed_by(PSubX,PSub))).

%% phenotype_db:method_feature_pair_phenosim(+Method,+F1,+F2,?Results:list)
% =|use_module(phenoblast)|= if you want this to be dynamically calculated.
% otherwise this is assumed extensional / pre-calculated
% update: now always static
:- extensional(method_feature_pair_phenosim/4).

feature_pair_phenosim_value(F1,F2,S,V) :-
	feature_pair_phenosim_value_asymm(F1,F2,S,V),
	!.
feature_pair_phenosim_value(F1,F2,S,V) :-
	feature_pair_phenosim_value_asymm(F2,F1,S,V),
	!.

feature_pair_phenosim_value_asymm(F1,F2,subsumers,V) :-
	method_feature_pair_phenosim(_,F1,F2,Rs),
	member(max_ic(_,V),Rs).
feature_pair_phenosim_value_asymm(F1,F2,max_ic,V) :-
	method_feature_pair_phenosim(_,F1,F2,Rs),
	member(max_ic(V),Rs).
feature_pair_phenosim_value_asymm(F1,F2,max_ic,V) :-
	method_feature_pair_phenosim(_,F1,F2,Rs),
	member(max_ic(V,_),Rs).
feature_pair_phenosim_value_asymm(F1,F2,avg,V) :-
	method_feature_pair_phenosim(_,F1,F2,Rs),
	member(avg_ic_bestmatches(V),Rs).
feature_pair_phenosim_value_asymm(F1,F2,avg,V) :-
	method_feature_pair_phenosim(_,F1,F2,Rs),
	member(simj(V),Rs).


affects(X,Y) :- restriction(X,R,Y),context_relation(R).

context_relation(part_of).
context_relation('OBO_REL:part_of').
context_relation('OBO_REL:has_output').
context_relation('OBO_REL:results_in_formation_of').

/** <module> Models phenotype expressions

---+ Dependencies

* ontol_db is required, and reflexive closure of subclass/2 should be calculated, e.g. with ontol_reasoner. This is necessary for phenotype_lca/3

TODO - remove feature_phenotype, use PKB model  

*/
