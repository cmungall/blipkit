/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_rdfs,
          [
          ]).

:- use_module(bio(ontol_db)).
:- use_module(triple20(owl)).   % ? just use direct rdf?
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

literal_value_type(literal(lang(en,X)),X,string):- !.
literal_value_type(literal(lang(_,X)),X,string):- !.
literal_value_type(literal(type(T,X)),X,T):- !.
literal_value_type(literal(X),X,string):- !.

literal_to_native(literal(lang(en,X)),X):- !.
literal_to_native(literal(lang(_,X)),X):- !.
literal_to_native(literal(type(_,X)),X):- !.
literal_to_native(literal(X),X):- !.
literal_to_native(X,X):- !.

is_named_class(ID):-
%%        rdf_has(ID, rdf:type, owl:'Class'),
        rdf_has(ID, rdf:type, rdfs:'Class'),
        rdf_split_url(_,LocalID,ID),
        not(sub_atom(LocalID,0,2,_,'__')).

ontol_db:ontology(ID,N,Desc):-
        rdf_has(ID1, rdf:type, owl:'Ontology'),
        literal_to_native(ID1,ID2),
        concat_atom([ID2,'#'],ID),
%        (rdf_has(ID2,rdfs:label,N1)
%        ->  literal_to_native(N1,N)
        (rdfs_label(ID2,N)
        ->  true
        ;   N=ID2),
        (rdf_has(ID2,rdfs:comment,Desc1)
        ->  literal_to_native(Desc1,Desc)
        ;   Desc=''). 
        
ontol_db:property(ID,N):-
        (rdf_has(ID, rdf:type,owl:'ObjectProperty')
        ;rdf_has(ID, rdf:type,owl:'AnnotationProperty')
        ;rdf_has(ID, rdf:type,owl:'DatatypeProperty')),
        (rdf_has(ID, rdfs:label, literal(lang(en,N))) % TODO: lang prefs
        ->  true
        ;   rdf_split_url(_,N,ID)).

ontol_db:property_domain(ID,C):-
        rdf_has(ID,rdfs:domain,C).

ontol_db:property_range(ID,C):-
        rdf_has(ID,rdfs:range,C).

ontol_db:class(ID,N):-
%%        rdf_has(ID, rdf:type, owl:'Class'), % ? rdfs:'Class'
        rdf_has(ID, rdf:type, rdfs:'Class'), % ? owl:'Class'
        is_named_class(ID),
        node_obo_name(ID,N).
ontol_db:subclass(ID,PID):-
        (   var(ID)
        ->  is_named_class(ID),
            %owl_subclass_of(ID,PID),
            rdf_has(ID,rdfs:subClassOf,PID),
            is_named_class(PID)
        ;   is_named_class(PID),
%            owl_subclass_of(ID,PID),
            rdf_has(ID,rdfs:subClassOf,PID),
            is_named_class(ID)),
        \+ ID=PID.

% subprop treated as subclass on ontol
ontol_db:subclass(ID,PID):-
        rdf_has(ID, owl:'subPropertyOf', PID).
% subclass-of-restriction idiom
ontol_db:restriction(ID,T,PID):-
        is_named_class(ID),
        owl_subclass_of(ID,RID),
        rdf_has(RID, rdf:type, owl:'Restriction'),
        owl_restriction_on(RID,Restr),
        (Restr=restriction(T,all_values_from(PID))
        ;Restr=restriction(T,some_values_from(PID))
%        ;Restr=restriction(T,has_value(PID))).
        ),
        is_named_class(PID).
ontol_db:belongs(ID,Ont):-
%%        rdf_has(ID, rdf:type, owl:'Class'),
        rdf_has(ID, rdf:type, rdfs:'Class'),
        rdf_split_url(Ont,_,ID).
%        rdf_split_url(NS,_,ID),
%        (rdf_db:ns(Ont,NS)
%        ->  true
%        ;   Ont=NS).
ontol_db:belongs(ID,Ont):-
        property(ID,_),
        rdf_split_url(Ont,_,ID).
ontol_db:inst_of(ID,ClassID):-
        rdf_has(ID,rdf:type,ClassID),
        is_named_class(ClassID).        
ontol_db:inst_sv(ID,S,V,T):-
        rdf_has(ID,S,V1),
        not(class(ID,_)),
        not(property(ID,_)),
        literal_value_type(V1,V,T),
        not(rdf_has(ID,rdf:type,V1)),
        not(rdf_has(ID,rdfs:label,V1)).
ontol_db:inst_rel(ID,S,V):-
        rdf_has(ID,S,V),
        not(class(ID,_)),
        not(property(ID,_)),
        not(literal_value_type(V,_,_)),
        not(rdf_has(ID,rdf:type,V)).
ontol_db:inst(ID,N):-
        inst_of(ID,_),
        node_obo_name(ID,N).
%ontol_db:essence(ID,essence(GenusID,DiffL)):-
%        owl_description(ID,intersection_of(DescL)).
%ontol_db:def(ID,Def):-
%        rdf_has(ID, rdfs:comment, literal(lang(en,Def))). % TODO: lang prefs
ontol_db:class_comment(ID,Cmt):-
        rdf_has(ID, rdfs:comment, Literal),
        literal_to_native(Literal,Cmt).
ontol_db:differentium(id,type,to):- fail.
ontol_db:is_anonymous(id):- fail.
ontol_db:lexical_category(id,categ):- fail.

% (+,?) sd
node_obo_name(ID,N):-
        (   rdf_has(ID, rdfs:label, Label)
        ->  (    Label=literal(lang(en,N)) % TODO: lang prefs
            ->   true
            ;    Label=literal(N))
        ;   rdf_split_url(_,N,ID)).


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
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/11/23 20:27:41 $
  @license LGPL

  ---+ Name
%  ontol_bridge_from_owl

  ---+ Synopsis

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

  The ontol_bridge_to_owl module, for the reverse transformation

  */
