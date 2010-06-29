:- module(curation_inference,
	  [
	   inferred_annot/5
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).


:- multifile tabling:memoize_hook/1.

tabling:memoize_hook(inferred_annot:some/4).
tabling:memoize_hook(inferred_annot:integral_to/4).


%% inferred_annot(?G,?R,+X,?Q,?Ev) is nondet
inferred_annot(G,R,X,some,Ev) :-
	some(G,R,X,Ev).
inferred_annot(G,R,X,integral_to,Ev) :-
	integral_to(G,R,X,Ev).

:- discontiguous some/4.
:- discontiguous integral_to/4.


% ----------------------------------------
% ASSERTED
% ----------------------------------------

some(G,R,X,A) :-
	curation_statement(A,G,R1,X),
	infer_property(A,X,R1,R).
integral_to(G,R,X,A) :-
	curation_statement(A,G,R1,X),
	infer_property(A,X,R1,R),
	curation_qualifier(A,integral_to,true).



% ----------------------------------------
% RULES
% ----------------------------------------


%%%

some(G,R,X,E) :- subclass(X2,X),some(G,R,X2,E).

% transitive part_of
some(G,part_of,C,E) :- parent(C2,part_of,C),some(G,part_of,C2,E).

some(G,is_active_participant_in,P,E) :- parent(P2,part_of,P),some(G,is_active_participant_in,P2,E).
some(G,is_active_participant_in,P,E) :- parent(P2,part_of,P),some(G,executes,P2,E).

% regulates
some(G,regulates,P,E) :- parent(P2,regulates,P),some(G,executes,P2,E).
some(G,negatively_regulates,P,E) :- parent(P2,negatively_regulates,P),some(G,executes,P2,E).
some(G,positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,executes,P2,E).

some(G,regulates,P,E) :- parent(P2,regulates,P),some(G,is_active_participant_in,P2,E).
some(G,negatively_regulates,P,E) :- parent(P2,negatively_regulates,P),some(G,is_active_participant_in,P2,E).
some(G,positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,is_active_participant_in,P2,E).

some(G,regulates,P,E) :- parent(P2,part_of,P),some(G,regulates,P2,E).

some(G,indirectly_regulates,P,E) :- parent(P2,regulates,P),some(G,regulates,P2,E).

some(G,indirectly_negatively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,negatively_regulates,P2,E).
some(G,indirectly_positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,positively_regulates,P2,E).
some(G,indirectly_positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,positively_regulates,P2,E).

% INTEGRAL TO

/*
integral_to(G,part_of,C_Big,E) :- parent(C_Big,has_part,C_Small),integral_to(G,part_of,C_Small,E).
integral_to(G,is_active_participant_in,C_Big,E) :- parent(C_Big,has_part,C_Small),integral_to(G,is_active_participant_in,C_Small,E).

% reverse propagation
integral_to(G,R,C_Specific,E) :- subclass(C_Specific,C_Generic),integral_to(G,R,C_Generic,E).
*/




%%%

infer_property(I,X,R1,R) :-
	infer_property1(I,X,R1,R),
	!.
infer_property(_,_,R,R) :- !.

infer_property1(_,X,_,part_of) :-
	belongs(X,cellular_component).
infer_property1(_,X,_,is_active_participant_in) :-
	belongs(X,biological_process).
infer_property1(_,X,_,executes) :-
	belongs(X,molecular_function).

