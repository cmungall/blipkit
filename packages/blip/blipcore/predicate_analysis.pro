:- module(predicate_analysis,
          [predicate_logicprop/2,
           source_deductive_cycle/2
          ]).

predicate_logicprop(P,superpredicate_of(P2)):-
        match(P-[X],P2-[X]).
predicate_logicprop(P,superpredicate_of(P2)):-
        match(P-[X,Y],P2-[X,Y]).
predicate_logicprop(P,symmetric):-
        match(P-[X,Y],P-[Y,X]).
predicate_logicprop(P,reflexive):-
        match(P-[X,X],true).
predicate_logicprop(P,inverse_of(P2)):-
        match(P-[X,Y],P2-[Y,X]),
        P\=P2.
predicate_logicprop(P,transitive):-
        match(P-[X,Y],(P-[X,Z],P-[Z,Y])).
predicate_logicprop(P,transitive_closure_of(P2)):-
        match(P-[X,Y],P2-[X,Y]),
        match(P-[X,Y],(P2-[X,Z],P-[Z,Y])).
predicate_logicprop(P,transitive_over(P2)):-
        match(P-[X,Y],(P-[X,Z],P2-[Z,Y])).
predicate_logicprop(P,holds_over_chain(P1,P2)):-
        match(P-[X,Y],(P1-[X,Z],P2-[Z,Y])).
predicate_logicprop(P,holds_over_chain(P1,P2,P3)):-
        (   match(P-[X,Y],(P1-[X,A],P2-[A,B],P3-[B,Y]))
        ->  true
        ;   match(P-[X,Y],(P1-[X,A],P2-[A,B],P3I-[Y,B]))
        ->  P3=inverse(P3I)
        ;   match(P-[X,Y],(P1I-[A,X],P2-[A,B],P3I-[Y,B]))
        ->  P1=inverse(P1I),
            P3=inverse(P3I)
        ;   match(P-[X,Y],(P1I-[A,X],P2-[A,B],P3-[B,Y]))
        ->  P1=inverse(P1I)
        ;   match(P-[X,Y],(P1-[A,X],P2-[B,A],P3-[B,Y])),P2=inverse(P2)).

match(HeadT,BodyT):-
        pargs_terms(HeadT,[Head]), % e.g. P(X,Y), where P is ground
        clause(Head,Body),         % e.g. P(X,Z),P(Z,Y)
        conj_list(Body,AssertedSubGoals), 
        permutation(AssertedSubGoals,AssertedSubGoalsPerm),
        terms_pargs(AssertedSubGoalsPerm,BodyToMatch),
        numbervars(BodyToMatch,0,_),
        BodyToMatch=BodyT.

pargs_terms((A,L),[A2|L2]):-
        pargs_terms(A,A2),
        !,
        pargs_terms(L,L2).
pargs_terms(P-Args,[Term]):-
        Term =.. [P|Args].

terms_pargs([Term],P-Args):-
        Term =.. [P|Args].
terms_pargs([A|L],(A2,L2)):-
        terms_pargs([A],A2),
        !,
        terms_pargs(L,L2).

list_conj([X],X):- !.
list_conj([X|T],(X,T2)):-
        list_conj(T,T2).

conj_list((X,T),[X|T2]):-
        conj_list(T,T2).
conj_list(X,[X]):- !.


source_deductive_cycle(File,Edge) :-
        read_file_to_terms(File,Terms,[]),
        findall(Child-Parent,
                (   member( (H:-B), Terms),
                    goal_preds(H,[Parent]),
                    goal_preds(B,Children),
                    member(Child,Children)),
                Edges),
        graph_cycle(Edges,Edge).

graph_cycle(Edges,CEdge) :-
        member(C-P,Edges),
        graph_cycle(P,Edges,CEdge,[C]).
graph_cycle(C,Edges,[C,P|VL],VL) :-
        member(C-P,Edges),
        member(P,VL),
        !.
graph_cycle(C,Edges,CEdge,VL) :-
        member(C-P,Edges),
        graph_cycle(P,Edges,CEdge,[C|VL]).
        
goal_preds((A,B),[P|BL]) :-
        functor(A,P,_),
        !,
        goal_preds(B,BL).
goal_preds((A),[P]) :-
        functor(A,P,_),
        !.



        

        

/** <module> analyzes properties of predicates based on clauses

  ---+ Synopsis

==
:- use_module(bio(predicate_analysis)).

% 
demo:-
  nl.
  

==

---+ Details



@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
