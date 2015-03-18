
:- module(curation_bridge_to_example,
	  [
           example/5
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).

:- use_module(bio(ontol_bridge_to_owl2_and_iao)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

example(A,Gene,Genus,R,Y) :-
        curation_subject_property_value(A,Genus,R,Y),
        curation_statement(A,Gene,_,Genus).

ontol_db:class(A) :-
        example(A,_,_,_,_).

ontol_db:def(A,'TODO') :-
        example(A,_,_,_,_).

ontol_db:def_xref(A,X) :-
        example(A,_,_,_,_),
        entity_source(A,X).

ontol_db:restriction(A,has_participant,Gene) :-
        example(A,Gene,_,_,_).

metadata_db:entity_label(A,N) :-
        example(A,_,Genus,R,Y),
        class(Genus,N1),
        class(Y,N2),
        atomic_list_concat([N1,R,N2],' ',N).

metadata_db:entity_resource(A,N) :-
        metadata_db:entity_publisher(A,N).

ontol_db:genus(A,G) :-
        example(A,_,G,_,_).

ontol_db:differentium(A,R,Y) :-
        example(A,_,_,R,Y).

