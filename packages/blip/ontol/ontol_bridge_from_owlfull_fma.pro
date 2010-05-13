/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_owlfull_fma,
          [
           assertall/0
          ]).

:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns(dl,'http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#').

literal_value_type(literal(lang(en,X)),X,string):- !.
literal_value_type(literal(lang(_,X)),X,string):- !.
literal_value_type(literal(type(T1,X)),X,T):- !, convert_xsd_type(T1,T).
literal_value_type(literal(X),X,string):- !.

%rdfid_oboid(X,X):- !.    % BASIC
rdfid_oboid(RdfID,OboID):-
        rdf_has(RdfID,dl:'FMAID',IdLiteral),
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
        rdf_has(XI,rdf:type,owl:'Class'),
        rdfid_oboid(XI,X),
        %rdf_has(XI,dl:'Preferred_name',NI),
        %rdf_has(NI,dl:name,NameLit),
        rdf_has(XI,rdfs:label,NameLit),
        literal_to_native(NameLit,Name),

        (   rdf_has(XI,dl:definition,DefLit),
            literal_to_native(DefLit,Def)
        ->  Defs=[ontol_db:def(X,Def)]
        ;   Defs=[]),
        
        findall([metadata_db:entity_synonym(X,Syn),metadata_db:entity_synonym_scope(X,Syn,exact)],
                (   rdf_has(XI,dl:'Synonym',NI),
                    rdf_has(NI,dl:name,SynLit),
                    literal_to_native(SynLit,Syn)),
                Syns),

        findall(ontol_db:subclass(X,ParentX),
                (   rdf_has(XI,rdfs:subClassOf,ParentXI),
                    rdfid_oboid(ParentXI,ParentX)),
                SubClasses),
        findall(ontol_db:restriction(X,R,Y),
                (   rdf_has(XI,RI,YI),
                    allowed_relation(RI,R),
                    rdf_has(YI,rdf:type,owl:'Class'),                    
                    %rdfid_oboid(RI,R),
                    rdfid_oboid(YI,Y)),
                Links),
        flatten([SubClasses,Syns,Links,Defs],L).


ontol_frame([metadata_db:entity_resource(X,fma),ontol_db:property(X),metadata_db:entity_label(X,X)|L]):-
        allowed_relation(_,X,Props),
        findall(Term,
                (   member(Prop,Props),
                    prop_term(Prop,X,Term)),
                L).

prop_term(is_transitive,X,is_transitive(X)).
prop_term(subrelation_of(Y),X,subclass(X,Y)).
                

allowed_relation(RI,R):- allowed_relation(RI,R,_).

%allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#regional_part',has_regional_part,[subrelation_of(has_part)]).
allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#regional_part_of',regional_part_of,[subrelation_of(part_of)]).
allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#constitutional_part_of',constitutional_part_of,[subrelation_of(part_of)]).
allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#systemic_part_of',systemic_part_of,[subrelation_of(part_of)]).
allowed_relation('',part_of,[is_transitive]).

%allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#regional_part_of',part_of,[is_transitive]).
%allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#constitutional_part_of',part_of,[is_transitive]).
%allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#systemic_part_of',part_of,[is_transitive]).

allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#bounds',bounds,[]).
allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#branch_of',branch_of,[]).
allowed_relation('http://bioontology.org/projects/ontologies/fma/fmaOwlDlComponent_2_0#attaches_to',attaches_to,[]).


