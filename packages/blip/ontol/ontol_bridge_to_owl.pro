/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl,[
                               force_ns/1,
                               get_triple/3,
                               owl_assertall/0
                               ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- dynamic term_bnodeid/2.

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% materialize
owl_assertall:-
        get_triple(Sbj,Pred,Obj),
        debug(ontol_bridge_to_owl,'~w',[triple(Sbj,Pred,Obj)]),
        rdf_assert(Sbj,Pred,Obj),
        fail.
owl_assertall.

get_bnodeid_by_term(T,N):-
        term_bnodeid(T,N),
        !.
get_bnodeid_by_term(T,N):-
        rdf_bnode(N),
        assert(term_bnodeid(T,N)).

derived_triple(Sbj,rdf:type,Obj):-
        inst_of(Sbj,Obj).
derived_triple(R,rdf:type,owl:'FunctionalProperty'):-
        is_functional(R).
derived_triple(Sbj,rdfs:label,Literal):-
        inst(Sbj,N),
        native_to_literal(N,Literal).
derived_triple(Sbj,Pred,Obj):-
        inst_sv(Sbj,Pred,Obj1,DT),
        Obj=literal(Obj1,DT).
derived_triple(Sbj,Pred,Obj):-
        inst_rel(Sbj,Pred,Obj).
derived_triple(Sbj,rdfs:subClassOf,Obj):-
        subclass(Sbj,Obj),
        \+ property(Sbj,_).
derived_triple(Sbj,rdfs:subPropertyOf,Obj):-
        property(Sbj,_),
        subclass(Sbj,Obj).
derived_triple(R,owl:inverseOf,S):-
        inverse_of(R,S).
derived_triple(Sbj,oban:has_resource,Obj):-
        entity_resource(Sbj,Obj).
derived_triple(Sbj,rdf:type,owl:'Class'):-
        class(Sbj).
derived_triple(Sbj,rdfs:label,Literal):-
        entity_label(Sbj,N),
        native_to_literal(N,Literal).        
derived_triple(Sbj,rdfs:comment,Literal):-
        class_comment(Sbj,N),
        native_to_literal(N,Literal).        
derived_triple(Sbj,rdf:type,owl:'ObjectProperty'):-
        property(Sbj).
derived_triple(Sbj,rdfs:domain,Obj):-
        property_domain(Sbj,Obj).
derived_triple(Sbj,rdfs:range,Obj):-
        property_range(Sbj,Obj).

derived_triple(Sbj,Pred,Obj):-
        derived_triples(Ts),
        member(triple(Sbj,Pred,Obj),Ts).

% restrictions: treated as subClassOf(restriction..)
derived_triples(Trips):-
        restriction(ID,T,PID),
        get_bnodeid_by_term(restriction(ID,T,PID),BID),
        Trips=[triple(ID,rdfs:subClassOf,BID),
               triple(BID,rdf:type,owl:'Restriction'),
               triple(BID,owl:onProperty,T),
               triple(BID,owl:someValuesFrom,PID)].

% synonyms: simple model
derived_triples(Trips):-
        synonym(ID,_Type,Synonym),
        native_to_literal(Synonym,Literal),
        Trips=[triple(ID,oboInOwl:hasSynonym,Literal)].

% NCBO oboInOwl model
%derived_triples(Trips):-
%        synonym(ID,Type,Synonym),
%        native_to_literal(Synonym,Literal),
%        get_bnodeid_by_term(synonym(ID,Type,Synonym),BID),
%        Trips=[triple(ID,oboInOwl:hasSynonym,BID),
%               triple(BID,rdf:type,rdf:'Description'),
%               triple(BID,rdfs:label,Literal)].

% anonymous classes
derived_triples(Trips):-
        inst_rel_anon(ID,Rel,ToClass),
        rdf_bnode(ToInstID),
        Trips=[triple(ID,Rel,ToInstID),
               triple(ToInstID,rdf:type,ToClass)].

% reification
derived_triples(Trips):-
        reification_quad(Statement,Subj,Pred,Obj),
        Trips=[triple(Statement,rdf:type,rdf:'Statement'),
                triple(Statement,rdf:subject,Subj),
                triple(Statement,rdf:predicate,Pred),
                triple(Statement,rdf:object,Obj)].

% intersection
derived_triples(Triples):-
        genus(Class,Genus0),
        rdfify(Genus0,Genus),
        findall(Element-Triples,
                (   differentium(Class,Rel,To),
                    get_bnodeid_by_term(differentium(Class,Rel,To),Element),
                    differentium_triples(Element,Rel,To,Triples)),
                ElTriplePairs),
        findall(Element,member(Element-_,ElTriplePairs),Elements),
        findall(Triple,(member(_-Triples1,ElTriplePairs),member(Triple,Triples1)),TriplesInList),
        rdfs_assert_list([Genus|Elements],RdfsList),
        Triples=[triple(Class,owl:intersectionOf,RdfsList)|TriplesInList].

% intersection, no genus
derived_triples(Triples):-
        class(Class),
        \+ genus(Class,_),
        \+ \+ differentium(Class,_,_),
        findall(Element-Triples,
                (   differentium(Class,Rel,To),
                    get_bnodeid_by_term(differentium(Class,Rel,To),Element),
                    differentium_triples(Element,Rel,To,Triples)),
                ElTriplePairs),
        findall(Element,member(Element-_,ElTriplePairs),Elements),
        findall(Triple,(member(_-Triples1,ElTriplePairs),member(Triple,Triples1)),TriplesInList),
        rdfs_assert_list(Elements,RdfsList),
        Triples=[triple(Class,owl:intersectionOf,RdfsList)|TriplesInList].


differentium_triples(Element,card(Rel,X,X),To,
                     [triple(Element,rdf:type,owl:'Restriction'),
                      triple(Element,owl:onProperty,Rel),
                      triple(Element,owl:cardinality,literal(X)),
                      triple(Element,owl:onClass,To)]):- !.
differentium_triples(Element,Rel,To,
                     [triple(Element,rdf:type,owl:'Restriction'),
                      triple(Element,owl:onProperty,Rel),
                      triple(Element,owl:someValuesFrom,To)]).


% todo - instances, subclass...
reification_quad(Statement,Subj,rdfs:subClassOf,Restriction):-
        reification(Statement,restriction(Subj,Pred,Obj)),
        get_bnodeid_by_term(restriction(Subj,Pred,Obj),Restriction).

% this works, but can be inefficient when S/P/O nonvar; try freeze...
get_triple(Sbj,Pred,Obj):-
        derived_triple(Sbj1,Pred1,Obj1),
        rdfify(Sbj1,Sbj),
        rdfify(Obj1,Obj),
        rdfify(Pred1,Pred).

% TODO: check; this appears to work..
% need to unify id mapping rules..
% also: reordering with derived_triples(L)...
experimental____get_triple(Sbj,Pred,Obj):-
        rdfid_oboid(SbjURI,Sbj),
        rdfid_oboid(ObjURI,Obj),
        rdfid_oboid(PredURI,Pred),
        trace,
        derived_triple(SbjURI,PredURI,ObjURI).



% (+,?) d
rdfify(literal(X),literal(X)):-
        !.
% obo ID may already be in NS:Local form
rdfify(ID,ID):-
        rdf_is_bnode(ID),
        !.
rdfify(NS:Local,ID):-
	sub_atom(Local,0,1,_,A),
	(   A='_'
	;   A@>'a',A@<'z'
	;   A@>'A',A@<'Z'),
        !,
        escape_ns(NS,NS2),
        force_ns(NS2),
        rdf_global_id(NS2:Local,ID).
rdfify(NS:Local,ID):-
        !,
        escape_ns(NS,NS2),
        force_ns(NS2),
	concat_atom([NS2,Local],'_',SafeLocal),
        rdf_global_id(NS2:SafeLocal,ID).
rdfify(OboID,ID):-
        rdfify_oboid(OboID,ID),
        !.
rdfify(Local,ID):-
        entity_label(OboID,Local),        % use name to lookup ID; WARN?
        rdfify_oboid(OboID,ID),
        !.
rdfify(Local,ID):-
        !,
        rdfify(orphan:Local,ID).

rdfify_oboid(skolem(Term),ID):-
        !,
        Term=..[Pred|Args],
        concat_atom(Args,'__',ArgsAtom),
        concat_atom([ArgsAtom,Pred],'-',OboID),
        rdfify_oboid(OboID,ID).
rdfify_oboid(OboID,ID):-
        sub_atom(OboID,Bef,_,After,':'),
        sub_atom(OboID,0,Bef,_,NS),
        \+ sub_atom(NS,_,_,_,'-'),  % todo - fix for all invalid chars
        !,
        Bef1 is Bef+1,
        sub_atom(OboID,Bef1,After,_,Local),
        %concat_atom([NS,'_',Local],PrefixedLocal),
        %rdfify(NS:PrefixedLocal,ID).
        rdfify(NS:Local,ID).

force_ns(NS):-
        rdf_db:ns(NS,_URI),
        !.
%        register_ns(NS,URI).        
force_ns(NS):-
        concat_atom(['http://purl.org/obo/owl/',NS,'#'],URI),
        %concat_atom(['urn:lsid:bioontology.org:',NS,':'],URI),
        %concat_atom(['http://www.bioontology.org/2006/02/',NS,'/'],URI),
        register_ns(NS,URI).

register_ns(NS,URI):-
        debug(owl,'registering ~w ~w',[NS,URI]),
        rdf_register_ns(NS,URI).
        

%inst_rel(A,'dc:source',X):-  % same as dc:source, more specific
%        entity_source(A,X).
%inst_rel(A,'oban:has_resource',X):-
%        entity_resource(A,X).
%inst_rel(A,'oban:creator',X):-
%        entity_creator(A,X).
%inst_rel(A,'oban:contributor',X):-
%        entity_contributor(A,X).


%escape_ns(V,V):- !.
% URI chars muse be escaped
escape_ns(V,V2):-
        atom_codes(V,CL),
        escape_codes_to_atoms(CL,AL),
        concat_atom(AL,V2),
        !.
escape_ns(V,_):-
        throw(escape_ns(V)).

% this is a misnomer we're just making it safe, not escaping
escape_codes_to_atoms([],[]).
escape_codes_to_atoms([C|CL],[C2|CL2]):-
        escape_code_to_atom(C,C2),
        escape_codes_to_atoms(CL,CL2).

% preserve alphanumerics
escape_code_to_atom(C,A):-
        ( (C >= 97, C =< 122)
        ; (C >= 65, C =< 90)
        ; (C >= 48, C =< 57)
        ; C=95
        ; C=46
        ; C=45),
        !,
        atom_codes(A,[C]).
% turn non-alphanumerics into underscores (do we need to roundtrip?)
escape_code_to_atom(_,'_').

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.11 $
  @date  $Date: 2006/03/18 03:35:06 $
  @license LGPL


  you should not need this module directly - handled by module io

  */
