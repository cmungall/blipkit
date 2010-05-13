/* -*- Mode: Prolog -*- */


:- module(sb_bridge_from_biopax,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(sb_db)).

biopax_ns('http://www.biopax.org/release/biopax-level1.owl#').

%  ---+++ reaction/2
%
%  reaction/2 is equivalent to instance of bp:interaction
sb_db:reaction(ID,N):-
        ClassID='http://www.biopax.org/release/biopax-level1.owl#interaction',
        inst_ofRT(ID,ClassID),
        (   inst_sv(ID,
                    'http://www.biopax.org/release/biopax-level1.owl#NAME',
                    N,
                    _)
        ->  true
        ;   inst(ID,N)).


%  ---+++ species/3
%
%  species/3 is equivalent to bp:physicalEntityParticipant
%
%  the species compartment is the object of the CELLULAR-LOCATION slot
sb_db:species(ID,N,CID):-
        ClassID='http://www.biopax.org/release/biopax-level1.owl#physicalEntityParticipant',
        inst_ofRT(ID,ClassID),
        inst(ID,N),
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#CELLULAR-LOCATION',
        (   inst_rel(ID,PropertyID,CID)
        ->  true
        ;   CID=null).

%  ---+++ compartment/2
%  
%  biopax does not explicitly model compartment/2: we find these by
%looking for anything that is the range of a CELLULAR-LOCATION this is
%represented as an instance of a bp:openControlledVocabulary
sb_db:compartment(ID,N):-
        MetaClassID='http://www.biopax.org/release/biopax-level1.owl#openControlledVocabulary',
        inst_of(ID,MetaClassID),
        RevPropertyID='http://www.biopax.org/release/biopax-level1.owl#CELLULAR-LOCATION',
        inst_rel(_,RevPropertyID,ID),
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#TERM',
        inst_sv(MetaClassID,PropertyID,N).

%  ---+++ reaction_reactant/2
%
%  reaction_reactant/2 from the object of the bp:LEFT slot
%
%  ???also controlled/controller???
sb_db:reaction_reactant(RID,SID):-
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#LEFT',
        inst_rel(RID,PropertyID,SID).

%  ---+++ reaction_product/2
%
%  reaction_product/2 from the object of the bp:RIGHT slot
%
%  ???also controlled/controller???
sb_db:reaction_product(RID,SID):-
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#RIGHT',
        inst_rel(RID,PropertyID,SID).

%  ---+++ reaction_modifier/2
%
%  reaction_modifier/2 from the object of the bp:COFACTOR slot
sb_db:reaction_modifier(RID,SID):-
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#COFACTOR',
        inst_rel(RID,PropertyID,SID).

%  ---+++ annotation/4
%
%  annotation/4 - currently incomplete TODO
sb_db:annotation(ID,URI,Base,ExtID):-
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#PHYSICAL-ENTITY',
        inst_sv(ID,PropertyID,URI,_),
        rdf_split_url(URI,Base,ExtID).
sb_db:annotation(ID,ExtID,'EC',ExtID):-
        PropertyID='http://www.biopax.org/release/biopax-level1.owl#EC-NUMBER',
        inst_sv(ID,PropertyID,LocalID),
        concat_atom(['EC',LocalID],':',ExtID).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(glyco)=
      (   load_biofile(owl,'biopax-level1.owl'),
          load_biofile(owl,'biopax-example-ecocyc-glycolysis.owl'))/[]).

unittest(test(basic,
            [_=load(glyco)],
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
%  sb_bridge_from_biopax

  ---+ Synopsis

  ==
  :- use_module(bio(sb_db),[reaction/2,sbentity_class/2,reaction_link/3]).
  :- use_module(bio(io),[load_biofile/2]).
  :- use_module(bio(graph),[closure_dist/4]).
  
  % load OWL class and instance data and access via sb_db views
  demo:-
    load_biofile(owl,'biopax-level1.owl'),
    load_biofile(owl,'biopax-example-ecocyc-glycolysis.owl'),
    ensure_loaded(bio(sb_bridge_from_biopax)),
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

  This module provides access to biopax OWL instance data using data
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