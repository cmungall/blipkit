/* -*- Mode: Prolog -*- */

:- module(sb_bridge_from_biopax_level2,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(bio(ontol_db)).
:- use_module(bio(sb_db)).

% duplicated from ontol_bridge_from_owl
literal_to_native(literal(lang(en,X)),X):- !.
literal_to_native(literal(lang(_,X)),X):- !.
literal_to_native(literal(type(_,X)),X):- !.
literal_to_native(literal(X),X):- !.
literal_to_native(X,X):- !.

biopax_ns('http://www.biopax.org/release/biopax-level2.owl#').

setup_ns :-
	ensure_loaded(library(semweb/rdf_db)),
        biopax_ns(NSFull),
        (   rdf_db:ns(bp,NSFull)
        ->  true
        ;   rdf_register_ns(bp,NSFull)).

:- initialization(setup_ns,now).


sb_db:reaction(ID,N):-
        rdfs_individual_of(ID,bp:interaction),
        rdfs_label_nullable(ID,N).

rdfs_label_nullable(ID,N):-
        (   rdfs_label(ID,N)
        ->  true
        ;   N=null).

sb_db:species(ID,N,CID):-
        rdfs_individual_of(ID,bp:physicalEntityParticipant),
        rdfs_label_nullable(ID,N),
        (   rdf_has(ID,bp:'CELLULAR-LOCATION',CID)
        ->  true
        ;   CID=null).

sb_db:compartment(ID,N):-
        rdf_has(_,bp:'CELLULAR-LOCATION',ID),
        rdfs_label_nullable(ID,N).

sb_db:component_of(Comp,Cont):-
        rdf_has(Cont,bp:'COMPONENTS',Comp).

sb_db:reaction_reactant(RID,SID):-
        rdf_has(RID,bp:'LEFT',SID).

sb_db:reaction_product(RID,SID):-
        rdf_has(RID,bp:'RIGHT',SID).

sb_db:reaction_modifier(RID,SID):-
        rdf_has(RID,bp:'COFACTOR',SID).

sb_db:annotation(ID,URI,Base,ExtID):-
        PropertyID='http://www.biopax.org/release/biopax-level2.owl#PHYSICAL-ENTITY',
        inst_sv(ID,PropertyID,URI,_),
        rdf_split_url(URI,Base,ExtID).
sb_db:annotation(ID,ExtID,'EC',ExtID):-
        PropertyID='http://www.biopax.org/release/biopax-level2.owl#EC-NUMBER',
        inst_sv(ID,PropertyID,LocalID),
        concat_atom(['EC',LocalID],':',ExtID).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(hiv)=
      (   load_biofile(owl,'biopax-level2.owl'),
          load_biofile(owl,'hiv1-bp.owl'))/[]).

unittest(test(basic,
            [_=load(hiv)],
            (   ensure_loaded(bio(sb_db)),
                ensure_loaded(bio(sb_bridge_from_biopax)),
                ensure_loaded(bio(graph)),
                forall(reaction(ID,N),
                       (   reaction(ID,N),
                           format('Reaction: ~w [~w]~n',[ID,N]),
                           forall(sbentity_class(ID,CID),
                                  format('  Annotation: ~w~n',[CID])),
                           setof(D-ID2,
                                 closure_dist(sb_db:reaction_link,D,ID,ID2),
                                 DIDs),
                           forall(member(D-ID2,DIDs),
                                  (   reaction(ID2,N2),
                                      format('  Distance ~w from ~w [~w]~n',[D,ID2,N2])))))),
            true)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/11/23 20:26:50 $
  @license LGPL

  ---+ Name
%  sb_bridge_from_biopax_level2

  ---+ Synopsis

  ==
  :- use_module(bio(sb_db),[reaction/2,sbentity_class/2,reaction_link/3]).
  :- use_module(bio(io),[load_biofile/2]).
  :- use_module(bio(graph),[closure_dist/4]).
  
  % load OWL class and instance data and access via sb_db views
  demo:-
    load_biofile(owl,'biopax-level1.owl'),
    load_biofile(owl,'biopax-example-ecocyc-glycolysis.owl'),
    ensure_loaded(bio(sb_bridge_from_biopax_level2)),
    reaction(ID,'6PFRUCTPHOS-RXN'),
    show_reaction(ID).

  show_reaction(ID):-
    reaction(ID,N),
    format('Reaction: ~w [~w]~n',[ID,N]),
    forall(sbentity_class(ID,CID),
           format('  Annotation: ~w~n',[CID])),
    forall(closure_dist(sb_db:reaction_link,D,ID,ID2),
           (   reaction(ID2,N2),
               format('  Distance ~w from ~w [~w]~n',[D,ID2,N2]))).
  ==

  ---+ Description

  This module provides access to biopax_level2 OWL instance data using data
predicates and rules from the sb_db module. Examples of sb_db data
predicates are species/3, reaction_reactant/2,
annotation/4 etc. Examples of rules are reaction_link/3

  ---++ How it works
  
  When you use this module, you are actually using the SWI
<http://www.swi-prolog.org/packages/SemWeb> SemWeb
library, and the rdf_db module. You can access the biopax instance
data more directly if you bypass this bridge module and use rdf_db
directly. You can also access it using

  
    * inst_of/2
    * inst_sv/3
    * inst_rel/3
    * inst/2
  

  and so on, in the ontol_db module

  
  */