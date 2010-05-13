:- module(kegg_bridge_to_pathway,[]).

:- use_module(bio(pathway_db)).
:- use_module(bio(kegg_db)).
:- use_module(bio(metadata_db)).

pathway_db:event_input(E,C,1) :- ksubstrate(E,C).
pathway_db:event_input(kedge(T,A,B),A,1) :- kedge(T,A,B).

pathway_db:event_output(E,C,1) :- kproduct(E,C).
pathway_db:event_output(kedge(T,A,B),B,1) :- kedge(T,A,B).

metadata_db:entity_label(kedge(T,A,B),L) :- concat_atom([A,' ',T,' ',B],L).

% TODO - edge type. map to interaction_db

/** <module> bridges between the native blip prolog pathway model and a generic ontology model

---+ Synopsis

---+ Details


*/
