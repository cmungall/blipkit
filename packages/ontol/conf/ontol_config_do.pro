:- multifile user:graphviz_ontol_param/2.
:- use_module(bio(metadata_db)).

:- dynamic dyn_ont_color/2.


ont_color(doid,steelblue2).
ont_color(dc,hotpink).
ont_color(orphanet,yellow).
ont_color(omim,peru).

user:graphviz_ontol_param(edge(_,_,_,_),override(label='')).
user:graphviz_ontol_param(edge(_,_,_,_),color=blue).
user:graphviz_ontol_param(edge(_,Obj,_,_),penwidth=3) :- id_idspace(Obj,'DOID').
user:graphviz_ontol_param(edge(_,Obj,_,_),weight=3) :- id_idspace(Obj,'DOID').



%user:graphviz_ontol_param(node(X),style=filled) :- focus_node(X).
user:graphviz_ontol_param(node(_X),style=filled).
user:graphviz_ontol_param(node(X),fillcolor=Col):-
        id_idspace(X,Ont),downcase_atom(Ont,S),ont_color(S,Col).
/*
user:graphviz_ontol_param(node(X),fillcolor=Col):-
        \+ focus_node(X),
        id_idspace(X,Ont),downcase_atom(Ont,S),ont_color(S,Col).
user:graphviz_ontol_param(node(X),fillcolor=Col):-
        \+ \+ focus_node(X),
        id_idspace(X,Ont),downcase_atom(Ont,S),ont_color(S,Col1),
        concat_atom(['white;0.2:',Col1,';0.6:white;0.2'],Col).
*/

%user:graphviz_ontol_param(node(X),override(shape=doubleoctagon)) :- focus_node(X).
%user:graphviz_ontol_param(node(X),style=bold) :- focus_node(X).
%user:graphviz_ontol_param(node(X),style=striped) :- focus_node(X).
%user:graphviz_ontol_param(node(X),style=striped) :- focus_node(X).
user:graphviz_ontol_param(node(X),fontsize=18) :- focus_node(X).
%user:graphviz_ontol_param(node(X),gradientangle=90) :- focus_node(X).
user:graphviz_ontol_param(node(X),penwidth=10) :- focus_node(X).

/*
user:graphviz_ontol_param(node(X),override(label=Label)) :-
        focus_node(X),
        entity_label(X,N),
        concat_atom(['*** ',N,' ***'],Label).
*/



