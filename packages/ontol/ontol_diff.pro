:- module(ontol_diff,
	  [
           optimize_diff/0,
           class_in/2,
           class_label_source/3,
           src_subclass/3,
           src_subclassRT/3,
           src_subclassT/3,
	   diff_subclass/3,
	   diff_subclass/8,
	   uniq_subclass/3,
	   uniq_subclass/5,
           uniq_subclass_with_defs/5,
           diff_label/5,
           diff_def/5
	   ]).

:- use_module(ontol_db).
:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(dbmeta)).

optimize_diff :-
        table_pred(src_subclassT/3),
        materialize_index(uniq_subclass_r(1,0,1)).

class_in(X,S) :-
        fact_clausesource(class(X),S).

%% src_subclass(S,X,Y)
%
% X isa Y in source S
src_subclass(S,X,Y) :-
        subclass(X,Y),
        fact_clausesource(subclass(X,Y),S).


% direct unless inferred db is used
uniq_subclass_r(X,Y,S) :-
        src_subclass(S,X,Y),
        X\=Y,
	fact_clausesource(class(X),S2),
	S2\=S,
	fact_clausesource(class(Y),S2),
	\+ src_subclass(S2,X,Y).

% NR
uniq_subclass(X,Y,S) :-
        uniq_subclass_r(X,Y,S),
        % check that X cannot be replaced by a more general Z
        \+ ((src_subclass(S,X,Z),
             X\=Z,
             src_subclass(S2,X,Z),
             S\=S2,
             uniq_subclass_r(Z,Y,S))),
        % check that Y cannot be replaced by a more specific Z
        \+ ((src_subclass(S,Z,Y),
             Z\=Y,
             src_subclass(S2,Z,Y),
             S\=S2,
             uniq_subclass_r(X,Z,S))).

uniq_subclass(X,Y,S,IsR1,IsR2) :-
        uniq_subclass_r(X,Y,S),
        is_redundant(X,Y,S,S,IsR1),
        freeze(S2,S2\=S),
        is_redundant(X,Y,S,S2,IsR2).

is_redundant(X,Y,S,S2,true) :-
        is_redundant(X,Y,S,S2),!.
is_redundant(_,_,_,_,false).

is_redundant(X,Y,S,S2) :-
        % check that X cannot be replaced by a more general Z
        \+ ((src_subclass(S2,X,Z),
             uniq_subclass_r(Z,Y,S))),
        % check that Y cannot be replaced by a more specific Z
        \+ ((src_subclass(S2,Z,Y),
             uniq_subclass_r(X,Z,S))).

        /*
uniq_subclass(X,Y,S) :-
        uniq_subclass_r(X,Y,S),
        \+ ((src_subclass(S,X,Z),
             X\=Z,
             class_in(Z,S2),
             S\=S2,
             uniq_subclass_r(Z,Y,S))).

*/        
        

/*
uniq_subclass(X,Y,Info,S) :-
        src_subclass(S,X,Y),
	fact_clausesource(class(X),S2),
	S2\=S,
	fact_clausesource(class(Y),S2),
	\+ src_subclass(S2,X,Y),
        src_subclassT(S2,X,Y),
        sformat(Info,'INDIRECT_in_~w',[S2]).


% redundant
uniq_subclass_r(X,Y,S) :-
	fact_clausesource(class(X),S),
	fact_clausesource(class(X),S2),
        S\=S2,
                writeln(S-X),

        src_subclassT(S,X,Y),
	fact_clausesource(class(Y),S2),
	\+ src_subclassT(S2,X,Y).

  */

% ----------------------------------------
% new... testing
% ----------------------------------------
diff_subclass(S,X,Y) :-
        diff_subclass(S,_,X,Y).
diff_subclass(S,S2,X,Y) :-
        src_subclass(S,X,Y),
        class_in(X,S),
        class_in(X,S2),
        class_in(Y,S),
        class_in(Y,S2),
        S\=S2,
        \+ src_subclass(S2,X,Y),
        \+ src_subclassT(S2,X,Y).

diff_subclass(S1,S2,X,Y,XN1,YN1,XN2,YN2) :-
        diff_subclass(S1,S2,X,Y),
        class_label_source(X,XN1,S1),
        class_label_source(X,XN2,S2),
        class_label_source(Y,YN1,S1),
        class_label_source(Y,YN2,S2).

% ----------------------------------------
% NON-PRE-REASONED: REASONING WITHIN SOURCE
% ----------------------------------------

src_subclassRT(S,X,Y) :-
        src_subclassT(S,X,Y).
src_subclassRT(S,X,X) :-
        class_in(X,S).
        
src_subclassT(S,X,Y) :-
        src_subclass(S,X,Y).
src_subclassT(S,X,Y) :-
        src_subclass(S,X,Z),
        src_subclassT(S,Z,Y).

% ----------------------------------------
% NON-PRE-REASONED: MOST DIRECT
% ----------------------------------------

% most direct comparable inferred link
src_subclassTC(S1,S2,X,Y) :-
        src_subclass(S1,X,Y),
        class_in(Y,S2),
        S2\=S1.
src_subclassTC(S1,S2,X,Y) :-
        src_subclass(S1,X,Z),
        \+ class_in(Z,S2),      % not comparable
        S2\=S1,
        src_subclassTC(S1,S2,Z,Y).

uniq_subclassTC(X,Y,S1) :-
	class_in(X,S2),
	class_in(X,S1),
        S1\=S2,
        src_subclassTC(S1,S2,X,Y), % most direct comparable
        \+ src_subclassT(S2,X,Y).


% ----------------------------------------
% DEFS
% ----------------------------------------

uniq_subclass_with_defs(X,Y,S,DX,DY) :-
        uniq_subclass(X,Y,S),
        (   def(X,DX),
            fact_clausesource(def(X,DX),S)
        ->  true
        ;   DX='n/a'),
        (   def(Y,DY),
            fact_clausesource(def(Y,DY),S)
        ->  true
        ;   DY='n/a').


class_label_source(X,N,S) :-
        entity_label(X,N),
        fact_clausesource(entity_label(X,N),S).

        
diff_label(X,L1,L2,S1,S2) :-
        entity_label(X,L1),
        fact_clausesource(entity_label(X,L1),S1),
        entity_label(X,L2),
        L1\=L2,
        fact_clausesource(entity_label(X,L2),S2),
        S1 @< S2. % asym

diff_def(X,L1,L2,S1,S2) :-
        def(X,L1),
        fact_clausesource(def(X,L1),S1),
        def(X,L2),
        L1\=L2,
        fact_clausesource(def(X,L2),S2),
        S1 @< S2. % asym

% ----------------------------------------
% EXPORT
% ----------------------------------------

in_both(X) :- class(X),fact_clausesource(class(X),S1),fact_clausesource(class(X),S2),S1\=S2.
relevant_to_both(X) :- class(X),\+ \+((subclassRT(X,Y),in_both(Y))),\+ \+((subclassRT(Z,X),in_both(Z))).



