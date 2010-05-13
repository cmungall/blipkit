:- module(kif2cl,[
                  kif_to_cl/2,
                  kif_to_cl/3
                  ]).

kif_to_cl(T,TX) :- kif_to_cl(T,TX,_).

kif_to_cl('=>'(A,C),forall(UQVars,if(AX,CX)),[]) :-
        !,
        kif_to_cl(A,AX,UQVars),
        kif_to_cl(C,CX,_).
kif_to_cl('<=>'(A,C),forall(UQVars,iff(AX,CX)),[]) :-
        !,
        kif_to_cl(A,AX,UQVars),
        kif_to_cl(C,CX,_).
kif_to_cl(T,V,[V]) :-
        atom(T),
        atom_concat('?',V,T),
        !.
kif_to_cl(T,T,[]) :-
        atom(T),
        !.
kif_to_cl(forall(VL,T),forall(VLX,TX),[]) :-
        (   is_list(VL)
        ;   compound(VL)),
        !,
        kif_to_cl(VL,VLX,_),
        kif_to_cl(T,TX,_).
kif_to_cl(T,TX,L) :-
        T=forall(V,T2),
        !,
        kif_to_cl(forall([V],T2),TX,L).
kif_to_cl(exists(VL,T),exists(VLX,TX),[]) :-
        !,
        kif_to_cl(VL,VLX,_),
        kif_to_cl(T,TX,_).
kif_to_cl(T,TX,UQVars) :-
        is_list(T),
        !,
        kif_to_cl_list(T,TX,UQVars).
kif_to_cl(T,TX,UQVars) :-
        T=..TL,
        !,
        kif_to_cl_list(TL,TX,UQVars).

kif_to_cl_list([],[],[]) :- !.
kif_to_cl_list([T|TL],[TX|TXL],UQVars) :-
        kif_to_cl(T,TX,UQVars1),
        kif_to_cl_list(TL,TXL,UQVars2),
        append(UQVars1,UQVars2,UQVars).

%cl_to_kif(forall(VL,T),

:- begin_tests(kif2cl, []).


:- end_tests(kif2cl).
        


/** <module> 

  ---+ Synopsis

==
:- use_module(bio(kif2cl)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
