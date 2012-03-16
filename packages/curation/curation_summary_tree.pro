:- module(curation_summary_tree,
	  [
	   write_xgraph_for/1
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

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

  E.g. blip -r go_assoc_local/mgi -debug xlinks -r go -u curation_summary_tree -goal "write_xgraph_for('Egfr'),halt"

  
*/

class_genus_differentium(C,Genus,Prop,Filler) :-
        genus(C,Genus),
        differentium(C,Prop,Filler).

class_node(C,G) :- genus(C,G),!.
class_node(C,node(C)).


%% class_x(+Class,?GraphTerm,+RootClass) is nondet
class_x(G,E,_) :-
        curation_statement(_,G,_,C),
        class_x(C,E,C).

class_x(C,node(C),_) :-                 genus(C,_).
class_x(C,label(C,Genus),_) :-          genus(C,Genus).
class_x(C,label(C/Prop,Prop),_) :-      differentium(C,Prop,_Filler).
class_x(C,edge(Filler,C/Prop),_) :-     differentium(C,Prop,Filler).
class_x(C,edge(C/Prop,C),_) :-          differentium(C,Prop,_Filler).
class_x(C,in_path(C,Root),Root).
class_x(C,E,Root) :-
        % todo - expand G
        % todo - no genus
        differentium(C,_Prop,Filler),
        class_x(Filler,E,Root).
class_x(C,E,Root) :-
        \+ differentium(C,_,_),
        (   parent(C,P),
            differentium(P,_,_)),
        !,
        class_x(P,E,Root).
class_x(C,E,Root) :-
        \+ differentium(C,_,_),
        parent(C,P),
        class_x(P,E,Root),
        !.

class_xgraph(C,L) :- setof(X,class_x(C,X,C),L).

merge_nodes(L,L2) :-
        findall(E2,
                (   member(E,L),
                    merge_node(E,E2,L)),
                L2).

/*
merge_node(node(N),node(N2),L) :- node_relabel(N,N2), !.
merge_node(node(N),node(N2),L) :- member(label(N,N2),L),!.

node_relabel(N,N2,L) :- member(label(N,N2),L),!.
node_relabel(N,N,_).
*/
               

write_xgraph_for(Sym) :-
        entity_label(G,Sym),
        write_xgraph_for(G).
write_xgraph_for(C) :-
        class_xgraph(C,L),
        write_xgraph(L).


write_xgraph(L_u) :-
        sort(L_u,L),
        debug(xgraph,' L=~w',[L]),
        findall(N,member(node(N),L),Nodes),
        debug(xgraph,' Nodes=~w',[Nodes]),
        findall(N,(member(N,Nodes),
                   \+ member(edge(N,_),L)),
                Roots),
        debug(xgraph,'Roots=~w',[Roots]),
        forall(member(Root,Roots),
               write_xgraph(Root,[' '],L)).

write_xgraph(N,D,L) :-
        writetabs(D),
        writenode(N,L),
        write(' // '),
        write_class(N,L),
        nl,
        findall(Ch,member(edge(Ch,N),L),Chs),
        (   Chs=[]
        ->  forall(member(in_path(N,Leaf),L),
                   write_xgraph_leaf(Leaf,D,L))
        ;   forall(member(Ch,Chs),
                   write_xgraph(Ch,[' '|D],L))).

write_xgraph_leaf(N,D,L) :-
        writetabs([' '|D]),
        write('+-- '),
        write_class(N,L),
        nl.

writetabs(D) :- maplist(write,D).

writenode(N,EL) :-
        member(label(N,N2),EL),
        !,
        write_class(N2,EL).
writenode(N,EL) :-
        write_class(N,EL).
write_class(N,_) :-
        class(N,Label),
        !,
        write(Label).
write_class(N,_) :-
        write(N).

                
