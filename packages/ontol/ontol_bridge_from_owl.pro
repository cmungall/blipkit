/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_owl,
          [
           rdfid_oboid/2,
           literal_to_native/2
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).
:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

literal_value_type(literal(lang(en,X)),X,string):- !.
literal_value_type(literal(lang(_,X)),X,string):- !.
literal_value_type(literal(type(T1,X)),X,T):- !, convert_xsd_type(T1,T).
literal_value_type(literal(X),X,string):- !.

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

owl_equivalentClass(X,Y):-               rdf_has(X,owl:equivalentClass,Y).
owl_equivalentClass(X,X).

suppress_ontology('http://www.w3.org/2002/07/owl#').
suppress_ontology('http://www.w3.org/1999/02/22-rdf-syntax-ns#').
suppress_ontology('http://www.w3.org/2000/01/rdf-schema#').

:- multifile suppress_resource/1.
%suppress_resource(X):-
%	debug(ontol,'testing for suppression of ~w',[X]),
%	suppress_ontology(Ont),
%	rdfs_individual_of(X,Type),
%	rdf_split_url(Ont,_,Type),
%	debug(ontol,'suppressed: ~w reason: type=~w',[X,Type]).
suppress_resource(X):-
	debug(ontol_owl,'testing for suppression of ~w',[X]),
	suppress_ontology(Ont),
	rdf_split_url(Ont,_,X).
suppress_resource(X):-
        rdf_has(X,rdf:type,owl:'Restriction').

% deprecated
suppress_class(ID):-            % gets treated in intersectionOf statement
        rdf_has(_,owl:equivalentClass,ID),
        rdf_is_bnode(ID).

rdfs_or_owl_class(Res):-            rdf_has(Res, rdf:type, owl:'Class').
rdfs_or_owl_class(Res):-            rdf_has(Res, rdf:type, rdfs:'Class').

sc(X,Y):- rdf_has(X,rdfs:subClassOf,Y).
sc(X,Y):- rdf_has(X,owl:subClassOf,Y).

:- multifile reserved_property/1.
reserved_property(P):-
	rdf_split_url(Ont,_,P),
	suppress_ontology(Ont).

% allow other modules to declare APs
% (reason: APs may not be explicitly defined as such)
:- multifile allowed_inst_sv/3.

% do not show Restriction triples etc as inst_svs for Classes
allowed_inst_sv(R,P,_):-
        (   rdf_has(R,rdf:type,owl:'Class')
        ->  rdf_has(P,rdf:type,owl:'AnnotationProperty')
        ;   true).
        

% (+,?) sd
node_obo_name(ID,N):-
        rdfid_oboid(ID_RDF,ID),
        rdf_has(ID_RDF, rdfs:label, Label),
        literal_to_native(Label,N).
%node_obo_name(ID,N):-
%        rdfid_oboid(ID_RDF,ID),
%        (   rdf_has(ID_RDF, rdfs:label, Label)
%        ->  literal_to_native(Label,N)
%        ;   rdf_split_url(_,N,ID_RDF)).


% deprec?
is_named_class(ID):-
        rdf_has(ID, rdf:type, owl:'Class'),
        rdf_split_url(_,LocalID,ID),
        not(sub_atom(LocalID,0,2,_,'__')).

% -- MAPPINGS --
metadata_db:entity_label(E,N):-
        node_obo_name(E,N).
metadata_db:idspace_uri(Local,Global):-    rdf_db:ns(Local,Global).

% experiment - what happens when we suppress:
metadata_db:entity_resource(C,Ont):-
        rdfid_oboid(C_RDF,C),
        rdfs_or_owl_class(C_RDF),
        rdf_split_url(Ont,_,C),
	\+ suppress_resource(C_RDF),
        \+ suppress_class(C).
metadata_db:entity_comment(C,Cmt):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF, rdfs:comment, Literal),
	\+ suppress_resource(C_RDF),
        \+ rdf_is_bnode(Literal),
        literal_to_native(Literal,Cmt).

ontol_db:import_directive(URI):-
        rdf_has(Ont,owl:imports,URI),
        debug(ontol_imports,'~w imports ~w',[Ont,URI]).

ontol_db:ontology(ID,N,Desc):-
        rdf_has(ID1, rdf:type, owl:'Ontology'),
        literal_to_native(ID1,ID2),
        concat_atom([ID2,'#'],ID),
        (rdfs_label(ID2,N)
        ->  true
        ;   N=ID2),
        (rdf_has(ID2,rdfs:comment,Desc1)
        ->  literal_to_native(Desc1,Desc)
        ;   Desc=''). 
        
ontol_db:class(C):-
        rdfid_oboid(Res,C),
        rdfs_or_owl_class(Res),
	\+ suppress_resource(Res).
        %\+ suppress_class(C).

ontol_db:subclass(C,PC):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(PC_RDF,PC),
	sc(C_RDF,PC_RDF),
	\+ suppress_resource(C_RDF),
	\+ suppress_resource(PC_RDF).  % e.g. no subClassOf Restrictions

/*
        (   var(C)
        ->  is_named_class(C_RDF),
            sc(C_RDF,PC_RDF),
            is_named_class(PC_RDF)
        ;   is_named_class(PC_RDF),
            sc(C_RDF,PC_RDF),
            is_named_class(C_RDF)),
        \+ C=PC.
*/

% subprop treated as subclass on ontol
ontol_db:subclass(C,PC):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(PC_RDF,PC),
        rdf_has(C_RDF, owl:subPropertyOf, PC_RDF),
	\+ suppress_resource(C_RDF),
	\+ suppress_resource(PC_RDF).

% subclass-of-restriction idiom
ontol_db:restriction(C,T,PC):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(T_RDF,T),
        rdfid_oboid(PC_RDF,PC),
	%debug(ontol,'  R ~w ~w ~w',[C_RDF,T_RDF,PC_RDF]),
	sc(C_RDF,Restr),
        rdf_has(Restr, rdf:type, owl:'Restriction'),
        rdf_has(Restr,owl:onProperty,T_RDF),
	debug(ontol_owl,'   checking prop [ALL-SOME]~w',[T_RDF]),
        \+ reserved_property(T_RDF),
	debug(ontol_owl,'   testing has [ALL-SOME] ~w',[Restr]),
        rdf_has(Restr,owl:someValuesFrom,PC_RDF),
	\+ suppress_resource(C_RDF),
	\+ suppress_resource(PC_RDF),
	debug(ontol,'  ALL-SOME ~w ~w ~w',[C,T,PC]).

% all-only
ontol_db:restriction(C,T,PC):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(T_RDF,TI),
        rdfid_oboid(PC_RDF,PC),
	%debug(ontol,'  R ~w ~w ~w',[C_RDF,T_RDF,PC_RDF]),
	sc(C_RDF,Restr),
        rdf_has(Restr, rdf:type, owl:'Restriction'),
        rdf_has(Restr,owl:onProperty,T_RDF),
	debug(ontol_owl,'   checking prop [ALL-ONLY]~w',[T_RDF]),
        \+ reserved_property(T_RDF),
	debug(ontol_owl,'   testing has [ALL-ONLY] ~w',[Restr]),
        rdf_has(Restr,owl:onlyValuesFrom,PC_RDF),
	property_relationship(T,all_only,TI),
	debug(ontol,'  ALL-ONLY ~w ~w ~w',[C,T,PC]).

ontol_db:property_relationship(RT,all_only,RI):-
        rdfid_oboid(RI_RDF,RI),
        rdf_has(Restr,owl:allValuesFrom,_),
        rdf_has(Restr,owl:onProperty,RI_RDF),
        atom_concat(RI,'_only',RT).

ontol_db:property(R):-
        property_relationship(R,_,_).


% TODO: owl:oneOf
%ontol_db:restriction(C,owl:oneOf,PC):-
%        rdfid_oboid(C_RDF,C),
%        rdfid_oboid(PC_RDF,PC),
%        rdf_has(C_RDF,owl:oneOf,RdfsList),
%        rdfs_list_to_prolog_list(RdfsList,L),
%        member(PC_RDF,L).

ontol_db:property(P):-
        rdfid_oboid(Res,P),
        (   rdf_has(Res, rdf:type,owl:'ObjectProperty')
        ;   rdf_has(Res, rdf:type,owl:'AnnotationProperty')
        ;   rdf_has(Res, rdf:type,owl:'DatatypeProperty')
        ;   rdf_has(Res, rdf:type,owl:'TransitiveProperty')
        ;   rdf_has(Res, rdf:type,rdf:'Property')),
	\+ reserved_property(P).

ontol_db:is_metadata_tag(P):-
        rdfid_oboid(Res,P),
        rdf_has(Res,rdf:type,owl:'AnnotationProperty').

ontol_db:is_transitive(P):-
        rdfid_oboid(Res,P),
        rdf_has(Res,rdf:type,owl:'TransitiveProperty').

ontol_db:property_domain(P,C):-
        rdfid_oboid(P_RDF,P),
        rdfid_oboid(C_RDF,C),
        rdf_has(P_RDF,rdfs:domain,C_RDF).

ontol_db:property_range(P,C):-
        rdfid_oboid(P_RDF,P),
        rdfid_oboid(C_RDF,C),
        \+ rdf_has(C_RDF,rdf:type,owl:'DataRange'),
        rdf_has(P_RDF,rdfs:range,C_RDF).

ontol_db:inst_of(I,C):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(I_RDF,I),
        rdf_has(I_RDF,rdf:type,C_RDF),
	\+ suppress_resource(I_RDF),
	\+ suppress_resource(C_RDF).
%        \+ rdf_has(I_RDF,rdf:type,owl:'Class'),
%        C_RDF \= owl:'Class'.
        %is_named_class(C_RDF).

ontol_db:inst_sv(I,S,V,T):-
        rdfid_oboid(I_RDF,I),
        rdfid_oboid(S_RDF,S),
        rdf_has(I_RDF,S_RDF,V1),
        \+ reserved_property(S_RDF),  % check both - weird expansion magic...
%        \+ (reserved_property(SX:SY),
%            concat_atom([SX,SY],':',S)),
        allowed_inst_sv(I_RDF,S_RDF,S),
        literal_value_type(V1,V,T).
        %not(rdf_has(ID,rdf:type,V1)),
        %not(rdf_has(ID,rdfs:label,V1)).
ontol_db:inst_rel(I,S,To):-
        rdfid_oboid(I_RDF,I),
        rdfid_oboid(S_RDF,S),
        rdfid_oboid(To_RDF,To),
        rdf_has(I_RDF,S_RDF,To_RDF),
        \+ reserved_property(S_RDF),  % check both - weird expansion magic...
 %       \+ (reserved_property(SX:SY),
 %          concat_atom([SX,SY],':',S)),
        not(class(I)),
        not(property(I)),
        not(literal_value_type(To_RDF,_,_)),
        not(rdf_has(I_RDF,rdf:type,To_RDF)).
ontol_db:inst(I):-              % todo - not all instances are typed
        inst_of(I,_).

/*
ontol_db:xxxgenus(C,G):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(G_RDF,G),
        rdf_has(C_RDF, rdf:type, owl:'Class'),
        owl_equivalentClass(C_RDF,Def),
        rdf_has(Def,owl:intersectionOf,RdfsList),
        rdfs_list_to_prolog_list(RdfsList,L),
        member(G_RDF,L),
        \+rdf_has(G_RDF,rdf:type,owl:'Restriction').
ontol_db:xxxdifferentium(C,R,To):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(R_RDF,R),
        rdfid_oboid(To_RDF,To),
        rdf_has(C_RDF, rdf:type, owl:'Class'),
        owl_equivalentClass(C_RDF,EquivC_RDF),
        rdf_has(EquivC_RDF,owl:intersectionOf,RdfsList),
        rdfs_list_to_prolog_list(RdfsList,L),
        member(Restr,L),
        rdf_has(Restr,rdf:type,owl:'Restriction'),
        rdf_has(Restr,owl:onProperty,R_RDF),
        rdf_has(Restr,owl:someValuesFrom,To_RDF).
*/

ontol_db:genus(C,G):-
        genus_differentia(C,G,_).
ontol_db:differentium(C,R,To):-
        genus_differentia(C,_,Diffs),
        member(R=To,Diffs).

plist_to_genii_differentia([],[],[]):- !.
plist_to_genii_differentia([G_RDF|T],[G|Gs],Ds):-
        \+rdf_has(G_RDF,rdf:type,owl:'Restriction'),
	!,
        rdfid_oboid(G_RDF,G),
	plist_to_genii_differentia(T,Gs,Ds).
plist_to_genii_differentia([Restr|T],Gs,[R=To|Ds]):-
	someValuesFrom(Restr,R,To),
	!,
	plist_to_genii_differentia(T,Gs,Ds).
plist_to_genii_differentia([Restr|T],Gs,[RU=To|Ds]):-
	allValuesFrom(Restr,R,To),
	property_relationship(R,all_only,RU),
	!,
	plist_to_genii_differentia(T,Gs,Ds).

genus_differentia(C,G,Diffs):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF, rdf:type, owl:'Class'),
        owl_equivalentClass(C_RDF,EquivC_RDF),
        rdf_has(EquivC_RDF,owl:intersectionOf,RdfsList),
        rdfs_list_to_prolog_list(RdfsList,L),
	plist_to_genii_differentia(L,Gs,Diffs),
	Diffs\=[],
	(   Gs=[G]
	->  true
	;   Gs=[]
	->  G='bfo:Entity'
	;   fail). % TODO

genus_differentia(C,'owl:Thing',[RU=To]):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF, rdf:type, owl:'Class'),
        owl_equivalentClass(C_RDF,Restr),
	allValuesFrom(Restr,R,To),
	property_relationship(RU,all_only,R).

old___genus_differentia(C,G,Diffs):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(G_RDF,G),
        rdf_has(C_RDF, rdf:type, owl:'Class'),
        owl_equivalentClass(C_RDF,EquivC_RDF),
        rdf_has(EquivC_RDF,owl:intersectionOf,RdfsList),
        rdfs_list_to_prolog_list(RdfsList,L),
        select(G_RDF,L,L2),
        \+rdf_has(G_RDF,rdf:type,owl:'Restriction'),
        solutions(R=To,Restr^(member(Restr,L2),someValuesFrom(Restr,R,To)),Diffs1),
        solutions(RU=To,Restr^(member(Restr,L2),allValuesFrom(Restr,R,To),property_relationship(R,all_only,RU)),Diffs2),
	append(Diffs1,Diffs2,Diffs),
	Diffs\=[].


someValuesFrom(Restr,R,To):-
        rdfid_oboid(R_RDF,R),
        rdfid_oboid(To_RDF,To),
        rdf_has(Restr,rdf:type,owl:'Restriction'),
        rdf_has(Restr,owl:onProperty,R_RDF),
        rdf_has(Restr,owl:someValuesFrom,To_RDF).

allValuesFrom(Restr,R,To):-
        rdfid_oboid(R_RDF,R),
        rdfid_oboid(To_RDF,To),
        rdf_has(Restr,rdf:type,owl:'Restriction'),
        rdf_has(Restr,owl:onProperty,R_RDF),
        rdf_has(Restr,owl:allValuesFrom,To_RDF).


ontol_db:class_union_element(C,U):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(U_RDF,U),
        owl_equivalentClass(C_RDF,EquivC_RDF),
        rdf_has(EquivC_RDF,owl:unionOf,RDFList),
        rdfs_list_to_prolog_list(RDFList,List),
        member(U_RDF,List).
ontol_db:disjoint_from(C,U):-
        rdfid_oboid(C_RDF,C),
        rdfid_oboid(U_RDF,U),
        rdf_has(C_RDF,owl:disjointWith,U_RDF).

ontol_db:is_anonymous(C):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF,rdf:type,owl:'Class'),
        rdf_is_bnode(C).
ontol_db:lexical_category(id,categ):- fail.


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(load_owl,
            [],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_bridge_from_owl)),
                load_bioresource(rdfs),
                load_bioresource(owl),
                load_biofile(owl,'sofa.owl'),
                class(ID,mRNA),
                class(PID,transcript),
                subclassT(ID,PID),
                class(_,exon)),
            true)).

unittest(test(genus_diff,
            [],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_bridge_from_owl)),
                load_bioresource(rdfs),
                load_bioresource(owl),
                load_biofile(owl,'llm.owl'),
                forall(genus(ID,GID),
                       format('~w genus:~w~n',[ID,GID])),
                forall(differentium(ID,R,DID),
                       format('diff ~w == ~w ~w~n',[ID,R,DID])),
                nl),
            true)).

unittest(test(wine_test,
            [],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_bridge_from_owl)),
                load_bioresource(rdfs),
                load_bioresource(owl),
                load_biofile(owl,'wine.owl'),
                write_biofile(obo,'wine.obo')),
            true)).

/** <module> maps from RDF/OWL to the ontol_db model

  ==
  :- use_module(bio(io),[load_biofile/2]).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_bridge_from_owl)).
  
  % access biopax OWL model using ontol_db predicates
  demo:-
    load_biofile(owl,'biopax-level1.owl'),
    class(ID,rna),
    format('In biopax, rna is a subclass of the following:~n',[]),
    forall(subclass(ID,PID),
           showclass(PID)).

  showclass(ID):-
    class(ID,N),
    format('Class ID:~w~nClass name:~w~n',[ID,N]).
  ==

  ---+ Description

  This module is a wrapper for the standard SWI-Prolog RDF and OWL
modules, which allows data in rdf_db to be accessed as if they were
data predicates in the ontol_db module, such as class/2,
subclass/2, parent/3, etc

  See <http://www.swi-prolog.org/packages/SemWeb> SemWeb

  ---+ See Also

  * ontol_bridge_to_owl.pro -- for the reverse transformation
  * ontol_bridge_from_owl2.pro -- mapping from OWL2 (via Thea2)

  ---+ TODO

  allow for mapping of namespace to shortened form
  
  */
