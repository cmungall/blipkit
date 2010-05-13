/* -*- Mode: Prolog -*- */

:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).

%:- pred is_a_diamond('P'-'PN','C1'-'N1','C2'-'N2').
is_a_diamond(P-PN,C1-N1,C2-N2):-
        subclass(P,C1),
        subclass(P,C2),
        C1 @< C2,
        class(C1,N1),
        class(C2,N2),
        class(P,PN).

relationship_diamond(R,P-PN,C1-N1,C2-N2):-
        class(P,PN),
        property(R,_),
        parent_overT(R,P,C1),
        parent_overT(R,P,C2),
        C1 @< C2,
        \+ parent_overT(_,C1,C2),
        \+ parent_overT(_,C2,C1),
        class(C1,N1),
        class(C2,N2).

nonredundant_subclass(C,P):-
        subclass(C,P),
        \+ ((   subclassT(C,X),
                subclassT(X,P))).

univocal_violation(C1,C2,N):-
        class(C1,N),
        class(C2,N),
        C1\=C2.

% duplciated!! with changes
ontol_closure_predicate(all,type_parent(_)).
ontol_closure_predicate(R,ontol_db:parent_over_nr(R,_)):- property(R,_).
type_parent(T,ID,PID):- parent(ID,T,PID).

class_depth_profile(ID,N,R,MaxDepth,NumPaths):-
        class(ID,N),
        ontol_closure_predicate(R,RPred),
        solutions(Path,closure_path(RPred,Path,ID,_),Paths),
        length(Paths,NumPaths),
        solutions(Length,(member(Path,Paths),length(Path,Length)),Lengths),
        list_max(Lengths,MaxDepth).
        
