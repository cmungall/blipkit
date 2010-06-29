:- module(curation_inference_go,
	  [
	   inferred_annot/5,
           inferred_potential/4,
           inferred_integral_to/4
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).

:- multifile tabling:memoize_hook/1.

tabling:memoize_hook(subclassT/2).
tabling:memoize_hook(parentT/2).

%% inferred_annot(+C,?G,?R,,?Q,?Ev) is nondet
inferred_annot(C,G,R,some,Ev) :-
        inferred_potential(C,G,R,Ev).
inferred_annot(C,G,R,integral_to,Ev) :-
        inferred_integral_to(C,G,R,Ev).

%% inferred_potential(+Y,?G,?R,?Ev)
inferred_potential(Y,G,R,Ev) :-
        subclassRT(X,Y),
	some(G,R,X,Ev).

% assume everything propagates over part_of
inferred_potential(Y,G,R,Ev) :-
        parentT(X,part_of,Y),
	some(G,R,X,Ev).

% direct propagation of regulates
inferred_potential(Y,G,R,Ev) :-
        regulates_relation(R),
        parentT(X,R,Y),
	some(G,_TODO,X,Ev).

% inverse propagation of subclass
inferred_integral_to(C_Specific,G,R,Ev) :-
	integral_to(G,R,C_Generic,Ev),
        subclassRT(C_Specific,C_Generic).
inferred_integral_to(C_Big,G,R,Ev) :-
	integral_to(G,R,C_Small,Ev),
        parentT(C_Big,has_part,C_Small).

regulates_relation(regulates).
regulates_relation(negatively_regulates).
regulates_relation(positively_regulates).


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

