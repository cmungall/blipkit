/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_owlfull_fma3,
          [
           assertall/0
          ]).

:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

%:- rdf_register_ns(dl,'http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#').
:- rdf_register_ns(dl,'http://sig.biostr.washington.edu/fma3.0#').

literal_value_type(literal(lang(en,X)),X,string):- !.
literal_value_type(literal(lang(_,X)),X,string):- !.
literal_value_type(literal(type(T1,X)),X,T):- !, convert_xsd_type(T1,T).
literal_value_type(literal(X),X,string):- !.

%rdfid_oboid(X,X):- !.    % BASIC
rdfid_oboid(RdfID,OboID):-
        rdf(RdfID,dl:'FMAID',IdLiteral),
        literal_to_native(IdLiteral,Num),
        atom_concat('FMA:',Num,OboID).

% TODO - use owl_util
literal_to_native(X,Y):-
        literal_to_native1(X,Y),
        atom(Y).
literal_to_native1(literal(lang(en,X)),X):- !.
literal_to_native1(literal(lang(_,X)),X):- !.
literal_to_native1(literal(type(_,X)),X):- !.
literal_to_native1(literal(X),X):- !.
literal_to_native1(X,X):- !.

% shorten xsd url to prefix
convert_xsd_type(In,Out):-
        rdf_global_id(Prefix:Local,In),
        concat_atom([Prefix,Local],':',Out),
        !.
convert_xsd_type(X,X).

assertall:-
        forall(ontol_frame(L),
               maplist(assert,L)).

ontol_frame([metadata_db:entity_resource(X,fma),ontol_db:class(X),metadata_db:entity_label(X,Name)|L]):-
        %rdf(XI,rdf:type,owl:'Class'),
        %rdf(XI,dl:'Preferred_name',NI),
        %rdf(NI,dl:name,NameLit),
        rdf(XI,rdfs:label,NameLit),
        rdfid_oboid(XI,X),
        \+ rdfs_subclass_of(XI,dl:'Non-physical_anatomical_entity_template'),
        \+ rdfs_individual_of(XI,dl:'Non-physical_anatomical_entity_template'),
        literal_to_native(NameLit,Name),

        (   rdf(XI,dl:definition,DefLit),
            literal_to_native(DefLit,Def)
        ->  Defs=[ontol_db:def(X,Def)]
        ;   Defs=[]),
        
        findall([metadata_db:entity_synonym(X,Syn),metadata_db:entity_synonym_scope(X,Syn,exact)],
                (   rdf(XI,dl:'Synonym',NI),
                    rdf(NI,dl:name,SynLit),
                    literal_to_native(SynLit,Syn)),
                Syns),

        findall(ontol_db:subclass(X,ParentX),
                (   rdf(XI,rdfs:subClassOf,ParentXI),
                    rdfid_oboid(ParentXI,ParentX)),
                SubClasses),
        findall(ontol_db:restriction(X,R,Y),
                (   rdf(XI,RI,YI),
                    allowed_relation(RI,R),
                    %%%rdf(YI,rdf:type,owl:'Class'),                    
                    rdfid_oboid(YI,Y)),
                Links),
        flatten([SubClasses,Syns,Links,Defs],L).


ontol_frame([metadata_db:entity_resource(X,fma),ontol_db:property(X),metadata_db:entity_label(X,X)|L]):-
        allowed_relation(URI,X,RelProps),
        findall(Term,
                (   member(RelProp,RelProps),
                    prop_term(RelProp,X,Term)),
                L1),
        findall(P,relation_prop(URI,X,P),L2),
        append(L1,L2,L).


relation_prop(URI,X,metadata_db:entity_alternate_identifier(X,AltID)) :-
        rdfid_oboid(URI,AltID).
relation_prop(URI,X,ontol_db:def(X,Def)) :-
        rdf(URI,rdfs:comment,Lit),
        literal_to_native(Lit,Def).
%relation_prop(URI,X,ontol_db:inverse_of_on_instance_level(X,Def)) :-
%        rdf(URI,owl:inverseOf,Lit),
%        literal_to_native(Lit,Def).

prop_term(is_transitive,X,is_transitive(X)).
prop_term(subrelation_of(Y),X,subclass(X,Y)).
                
allowed_relation(RI,R):- allowed_relation(RI,R,_).

%allowed_relation('http://sig.biostr.washington.edu/fma3.0#regional_part',has_regional_part,[subrelation_of(has_part)]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#regional_part_of',regional_part_of,[subrelation_of(part_of)]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#constitutional_part_of',constitutional_part_of,[subrelation_of(part_of)]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#systemic_part_of',systemic_part_of,[subrelation_of(part_of)]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#part_of',part_of,[is_transitive]).

%allowed_relation('http://sig.biostr.washington.edu/fma3.0#regional_part_of',part_of,[is_transitive]).
%allowed_relation('http://sig.biostr.washington.edu/fma3.0#constitutional_part_of',part_of,[is_transitive]).
%allowed_relation('http://sig.biostr.washington.edu/fma3.0#systemic_part_of',part_of,[is_transitive]).

allowed_relation('http://sig.biostr.washington.edu/fma3.0#bounded_by',bounded_by,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#surrounded_by',surrounded_by,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#branch_of',branch_of,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#attaches_to',attaches_to,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#connected_to',connected_to,[]). 

% new with 3

allowed_relation('http://sig.biostr.washington.edu/fma3.0#adheres_to',adheres_to,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#develops_from',develops_from,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#arterial_supply_of',arterial_supply_of,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#receives_input_from',receives_input_from,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#tributary_of',tributary_of,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#efferent_to',efferent_to,[]).
allowed_relation('http://sig.biostr.washington.edu/fma3.0#attaches_to',attaches_to,[]).

/*
rdf_has(R,rdf:type,owl:'ObjectProperty'),aggregate(max(T),X,aggregate(count,Y,rdf_has(X,R,Y),T),Num),rdf_has(R,owl:inverseOf,RI),aggregate(max(T),X,aggregate(count,Y,rdf_has(X,RI,Y),T),NumI),format('~w MAX: ~w  <-- INVERSE --> ~w MAX: ~w ~n',[R,Num,RI,NumI]),fail.
http://sig.biostr.washington.edu/fma3.0#arterial_supply MAX: 14  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#arterial_supply_of MAX: 18
http://sig.biostr.washington.edu/fma3.0#branch_of MAX: 4  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#branch MAX: 80
http://sig.biostr.washington.edu/fma3.0#constitutional_part MAX: 100  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#constitutional_part_of MAX: 99
http://sig.biostr.washington.edu/fma3.0#bounded_by MAX: 5  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#bounds MAX: 150
http://sig.biostr.washington.edu/fma3.0#bounds MAX: 150  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#bounded_by MAX: 5
http://sig.biostr.washington.edu/fma3.0#arterial_supply_of MAX: 18  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#arterial_supply MAX: 14
http://sig.biostr.washington.edu/fma3.0#continuous_with MAX: 54  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#continuous_with MAX: 54
http://sig.biostr.washington.edu/fma3.0#constitutional_part_of MAX: 99  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#constitutional_part MAX: 100
http://sig.biostr.washington.edu/fma3.0#branch MAX: 80  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#branch_of MAX: 4
http://sig.biostr.washington.edu/fma3.0#tributary MAX: 26  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#tributary_of MAX: 3
http://sig.biostr.washington.edu/fma3.0#tributary_of MAX: 3  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#tributary MAX: 26
http://sig.biostr.washington.edu/fma3.0#receives_input_from MAX: 26  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#sends_output_to MAX: 26
http://sig.biostr.washington.edu/fma3.0#sends_output_to MAX: 26  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#receives_input_from MAX: 26
http://sig.biostr.washington.edu/fma3.0#regional_part_of MAX: 7  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#regional_part MAX: 100
http://sig.biostr.washington.edu/fma3.0#regional_part MAX: 100  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#regional_part_of MAX: 7
http://sig.biostr.washington.edu/fma3.0#efferent_to MAX: 7  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#afferent_to MAX: 21
http://sig.biostr.washington.edu/fma3.0#afferent_to MAX: 21  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#efferent_to MAX: 7
http://sig.biostr.washington.edu/fma3.0#Homonym_of MAX: 1  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#homonym_for MAX: 8
http://sig.biostr.washington.edu/fma3.0#homonym_for MAX: 8  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#Homonym_of MAX: 1
http://sig.biostr.washington.edu/fma3.0#segmental_composition_of MAX: 1  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#segmental_composition MAX: 2
http://sig.biostr.washington.edu/fma3.0#segmental_composition MAX: 2  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#segmental_composition_of MAX: 1
http://sig.biostr.washington.edu/fma3.0#nerve_supply_of MAX: 13  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#nerve_supply MAX: 29
http://sig.biostr.washington.edu/fma3.0#nerve_supply MAX: 29  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#nerve_supply_of MAX: 13
http://sig.biostr.washington.edu/fma3.0#attaches_to MAX: 6  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#receives_attachment MAX: 7
http://sig.biostr.washington.edu/fma3.0#receives_attachment MAX: 7  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#attaches_to MAX: 6
http://sig.biostr.washington.edu/fma3.0#gives_rise_to MAX: 7  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#develops_from MAX: 2
http://sig.biostr.washington.edu/fma3.0#develops_from MAX: 2  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#gives_rise_to MAX: 7
http://sig.biostr.washington.edu/fma3.0#venous_drainage_of MAX: 14  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#venous_drainage MAX: 10
http://sig.biostr.washington.edu/fma3.0#venous_drainage MAX: 10  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#venous_drainage_of MAX: 14
http://sig.biostr.washington.edu/fma3.0#surrounds MAX: 5  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#surrounded_by MAX: 5
http://sig.biostr.washington.edu/fma3.0#surrounded_by MAX: 5  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#surrounds MAX: 5
http://sig.biostr.washington.edu/fma3.0#part MAX: 2  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#part_of MAX: 1
http://sig.biostr.washington.edu/fma3.0#part_of MAX: 1  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#part MAX: 2
http://sig.biostr.washington.edu/fma3.0#continuous_with_distally MAX: 52  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#continuous_with_proximally MAX: 51
http://sig.biostr.washington.edu/fma3.0#continuous_with_proximally MAX: 51  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#continuous_with_distally MAX: 52
http://sig.biostr.washington.edu/fma3.0#segmental_supply_of MAX: 56  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#segmental_supply MAX: 29
http://sig.biostr.washington.edu/fma3.0#segmental_supply MAX: 29  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#segmental_supply_of MAX: 56
http://sig.biostr.washington.edu/fma3.0#member MAX: 56  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#member_of MAX: 5
http://sig.biostr.washington.edu/fma3.0#member_of MAX: 5  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#member MAX: 56
http://sig.biostr.washington.edu/fma3.0#secondary_segmental_supply MAX: 2  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#secondary_segmental_supply_of MAX: 31
http://sig.biostr.washington.edu/fma3.0#secondary_segmental_supply_of MAX: 31  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#secondary_segmental_supply MAX: 2
http://sig.biostr.washington.edu/fma3.0#contained_in MAX: 65  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#contains MAX: 11
http://sig.biostr.washington.edu/fma3.0#contains MAX: 11  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#contained_in MAX: 65
http://sig.biostr.washington.edu/fma3.0#lymphatic_drainage MAX: 8  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#lymphatic_drainage_of MAX: 10
http://sig.biostr.washington.edu/fma3.0#lymphatic_drainage_of MAX: 10  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#lymphatic_drainage MAX: 8
http://sig.biostr.washington.edu/fma3.0#primary_segmental_supply_of MAX: 20  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#primary_segmental_supply MAX: 2
http://sig.biostr.washington.edu/fma3.0#primary_segmental_supply MAX: 2  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#primary_segmental_supply_of MAX: 20
http://sig.biostr.washington.edu/fma3.0#systemic_part MAX: 97  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#systemic_part_of MAX: 6
http://sig.biostr.washington.edu/fma3.0#systemic_part_of MAX: 6  <-- INVERSE --> http://sig.biostr.washington.edu/fma3.0#systemic_part MAX: 97
*/
