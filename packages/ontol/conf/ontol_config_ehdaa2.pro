:- multifile user:graphviz_ontol_param/2.

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).

:- multifile user:focus_node/1.
:- dynamic user:focus_node/1.


:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(containment_relation(_),tissue_part_of).

user:graphviz_ontol_param(edge(_,_,is_a,_),arrowhead=empty).
user:graphviz_ontol_param(edge(_,_,is_a,_),color=green).
user:graphviz_ontol_param(edge(_,_,po,_),color=blue).
user:graphviz_ontol_param(edge(_,_,'part_of',_),color=blue).
user:graphviz_ontol_param(edge(_,_,'part_of',_),label='PO').
user:graphviz_ontol_param(edge(_,_,df,_),arrowhead=open).
user:graphviz_ontol_param(edge(_,_,df,_),color=red).

user:graphviz_ontol_param(node(X),style=filled) :- focus_node(X).
user:graphviz_ontol_param(node(X),fillcolor=yellow) :- focus_node(X).

:- multifile ontol_db:restriction/3.

ontol_db:restriction(X,tissue_part_of,Y) :-
        restriction(X,part_of,Y),
        class(X,XN),
        class(Y,YN),
        atom_concat(YN,_,XN).

        
        
