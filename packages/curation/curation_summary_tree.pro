:- module(curation_summary_tree,
	  [
	   write_xlinks_for/1
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).

/*

  e.g.

  reg and negreg some (synth and has_out some x)

  reg
    negreg
      synth
        has_in
          x
        has_out [s/-/reg]
          y
  
*/

class_xlink(C,EList) :-
        genus(C,Genus),
        differentium(C,Prop,Filler),
        !,
        L1=[
            node(C),
            node(C/Prop),
            label(C/Prop, Prop),
            node(Filler,Filler),
            label(C,Genus),
            edge(Filler,C/Prop),
            edge(C/Prop,C)],
        findall(L2,class_xlink(Filler,L2),LoLs),
        flatten([L1,LoLs],EList).

class_xlink(C,[node(C)]) :- !.

/*
class_xlink(C,EList) :-
        \+ genus(C,_),
        parent(C,_R,P),
        class_xlink(P,EList).
*/

write_xlinks_for(C) :-
        class_xlink(C,L),
        write_xlinks(L).


write_xlinks(L_u) :-
        sort(L_u,L),
        debug(xlinks,' L=~w',[L]),
        findall(N,member(node(N),L),Nodes),
        debug(xlinks,' Nodes=~w',[Nodes]),
        findall(N,(member(N,Nodes),
                   \+ member(edge(N,_),L)),
                Roots),
        debug(xlinks,'Roots=~w',[Roots]),
        forall(member(Root,Roots),
               write_xlinks(Root,[' '],L)).

write_xlinks(N,D,L) :-
        writetabs(D),
        writenode(N,L),
        nl,
        forall(member(edge(Ch,N),L),
               write_xlinks(Ch,[' '|D],L)).

writetabs(D) :- maplist(write,D).

writenode(N,EL) :-
        member(label(N,N2),EL),
        !,
        writenode(N2,EL).
writenode(N,_) :-
        class(N,Label),
        !,
        write(Label).
writenode(N,_) :-
        write(N).

                
