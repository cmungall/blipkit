:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(library(porter_stem),[]).

ontol_db:class_xref(Class,Xref):-
        var(Xref),
        entity_xref(Class,Xref1),
        concat_atom([taxon,Local],':',Xref1),
        concat_atom(['NCBITaxon',Local],':',Xref),
        class(Class).
ontol_db:class_xref(Class,Xref):-
        nonvar(Xref),
        concat_atom(['NCBITaxon',Local],':',Xref),
        concat_atom([taxon,Local],':',Xref1),
        entity_xref(Class,Xref1),
        class(Class).
