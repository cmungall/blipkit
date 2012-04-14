:- module(cytoscape_bridge_from_ontol,
          []).

:- use_module(cytoscape_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

cytoscape_db:node(X) :- class(X).
cytoscape_db:node_attribute_value(X,label,N) :- class(X,N).
cytoscape_db:node_attribute_value(X,idspace,Y) :- class(X),id_idspace(X,Y).
cytoscape_db:edge_attribute_value(edge(S,R,T),label,R) :- edge(S,R,T).
cytoscape_db:edge(S,R,T) :- parent(S,R,T).
