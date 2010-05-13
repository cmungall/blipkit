:- module(rdf_id_util,
          [rdfid_oboid/2,
           literal_value_type/3]).

:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).
:- use_module(library('semweb/rdf_db')).

:- multifile rdfid_oboid_hook/2.

:- mode rdfid_oboid(?,?) is det.
rdfid_oboid(RdfID,OboID):-
        nonvar(RdfID),
        rdf_global_id(NS:Local,RdfID),
        !,
        concat_atom([NS,Local],':',OboID).
rdfid_oboid(RdfID,OboID):-   
        nonvar(OboID),
        (   atomic(OboID)          % tolerate skolem IDs
        ->  concat_atom([NS,Local],':',OboID),
            rdf_db:ns(NS,_),
            rdf_global_id(NS:Local,RdfID)
        ;   rdf_bnode(RdfID)),
        !.
rdfid_oboid(RdfID,OboID):-
        var(RdfID),
        var(OboID),
        freeze(RdfID,rdfid_oboid(RdfID,OboID)),
        !.
%rdfid_oboid(RdfID,OboID):-
%        rdfid_oboid_hook(RdfID,OboID),
%        !.
rdfid_oboid(X,X):- !.
%rdfid_oboid(X,Y):-
%        atomic(Y),
%        !,
%        X=Y.



% cut-n-pasted:
literal_value_type(literal(lang(en,X)),X,string):- !.
literal_value_type(literal(lang(_,X)),X,string):- !.
literal_value_type(literal(type(T1,X)),X,T):- !, convert_xsd_type(T1,T).
literal_value_type(literal(X),X,string):- !.

% shorten xsd url to prefix
convert_xsd_type(In,Out):-
        rdf_global_id(Prefix:Local,In),
        concat_atom([Prefix,Local],':',Out),
        !.
convert_xsd_type(X,X).
