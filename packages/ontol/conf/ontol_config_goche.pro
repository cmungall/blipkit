
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).

:- multifile user:graphviz_ontol_param/2.
:- multifile ontol_db:restriction/3.


user:graphviz_ontol_param(display_relation(chebi_ontology),'ChEBI').
user:graphviz_ontol_param(display_relation(chebi_ontology),'GO').

class_in(X,S) :-
        fact_clausesource(class(X),S).


x_ont(ID,Ont) :-
	entity_xref(ID,X),
	id_idspace(X,Ont).

combo_color(goche,blue).
combo_color(chebi,green).
combo_color(both,lemonchiffon).

is_chebi(chebi_lite).
is_chebi(chebi).
is_chebi('CHEBI').

ontol_db:restriction(X,'ChEBI',Y) :-
        %fact_clausesource(subclass(X,Y),S),
        %debug(ontol_rest,'FACTSRC: ~w',[S]),
        fact_clausesource(subclass(X,Y),S),
        is_chebi(S).

ontol_db:restriction(X,'GO',Y) :-
        fact_clausesource(subclass(X,Y),S),
        \+ is_chebi(S).

user:graphviz_ontol_param(node(_),style=filled).
user:graphviz_ontol_param(node(_),shape=box).
user:graphviz_ontol_param(node(X),override(label=N)) :-
        fact_clausesource(entity_label(X,N1),goche),
        fact_clausesource(entity_label(X,N2),S2),
        is_chebi(S2),
        N1\=N2,
        concat_atom(['G: ',N1,' ; ','C: ',N2],N).

user:graphviz_ontol_param(node(X),fillcolor=C):-
        class_in(X,goche),
        class_in(X,CHEBI),
        is_chebi(CHEBI),
        combo_color(both,C).

user:graphviz_ontol_param(node(X),fillcolor=C):-
        class_in(X,goche),
        \+((class_in(X,CHEBI),
            is_chebi(CHEBI))),
        combo_color(goche,C).

user:graphviz_ontol_param(node(X),fillcolor=C):-
        class_in(X,chebi_lite),
        \+class_in(X,goche),
        combo_color(chebi,C).

user:graphviz_ontol_param(edge(_,_,is_a,_),color=white).

%user:graphviz_ontol_param(edge(_,_,R,_),penwidth=3):-
%        R\=is_a.

user:graphviz_ontol_param(edge(_,_,'ChEBI',_),color=C):-
        combo_color(chebi,C).

user:graphviz_ontol_param(edge(_,_,'GO',_),color=C):-
        combo_color(goche,C).

user:graphviz_ontol_param(edge(_,_,_,_),override(label='')).

user:graphviz_ontol_param(edge(X,Y,_,_),weigth=1005) :-
        restriction(X,'ChEBI',Y),
        restriction(X,'GO',Y).

user:graphviz_ontol_param(edge(X,Y,_,_),penwidth=4) :-
        restriction(X,'ChEBI',Y),
        restriction(X,'GO',Y).

