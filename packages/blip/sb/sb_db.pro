/* -*- Prolog -*- */


:- module(sb_db,
          [
           model/2,
           unitdef/2,
           unit/4,
           kinetic_law/2,
           assignment_rule/2,
           compartment/2,
           species/3,
           reaction/2,
           sbentity_param/3,
           annotation/4,
           parameter/4,
           reaction_reactant/2,
           reaction_product/2,
           reaction_modifier/2,
           component_of/2,
           
           reaction_participant/2,
           reaction_participant/3,
           sbentity/2,
           sbentity/3,
           
           model_class/2,
           species_class/2,
           reaction_class/2,
           compartment_class/2,
           sbentity_class/2,
           
           reaction_link/2,
           reaction_link/3,
           species_path/3
          ]).


% should we prepend model ID to each? Do at parse time?
% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).

%% model(Model,Name)
:- extensional(model/2).

%% unitdef(Unit,Name)
:- extensional(unitdef/2).

%% unit(Unit,Exp,Kind,Scale)
:- extensional(unit/4).

%% kinetic_law(KinteticLaw,MathTerm)
:- extensional(kinetic_law/2).

%% assignment_rule(Rule,MathTerm)
:- extensional(assignment_rule/2).

%% compartment(Compartment,Name)
:- extensional(compartment/2).

%% species(Species,Name,Compartment)
:- extensional(species/3).

%% reaction(Reaction,Name)
:- extensional(reaction/2).

%% sbentity_param(Entity,Type,Val)
:- extensional(sbentity_param/3).

%% parameter(Param,MetaId,Name,Val)
:- extensional(parameter/4).

%% annotation(Id,Uri,BaseUri,ExtId)
:- extensional(annotation/4).

%% component_of(Component,Container)
:- extensional(component_of/2).

%% reaction_product(?R,?P)
% relation between a biochemical reaction and a molecular constituent produced in the reaction
:- extensional(reaction_product/2).

%% reaction_modifier(?R,?P)
% relation between a biochemical reaction and a molecular constituent that plays a role in the process but is unmodified 
:- extensional(reaction_modifier/2).

%% reaction_reactant(?R,?P)
% relation between a biochemical reaction and a molecular constituent that is consumed in the reaction
:- extensional(reaction_reactant/2).

%% sbentity(?ID,?ClassID)
sbentity(ID,N):- sbentity(ID,N,_).

%% sbentity(?ID,?ClassID,?Type)
%  mode: nondet
%   any sbentity: species, compartment or reaction
sbentity(ID,N,species):- species(ID,N,_).
sbentity(ID,N,compartment):- compartment(ID,N).
sbentity(ID,N,reaction):- reaction(ID,N).

%% species_class(?SID,?ClassID) is  nondet
%  ID for an ontol class for this species; eg a ChEBI ID or a KEGG ID
%  
species_class(SID,ClassID):-
        annotation(SID,_,_,ClassID).

%% reaction_class/2
%  reaction_class(?SID,?ClassID)
%  mode: nondet
%   ID for an ontol class for this reaction; eg a GO ID or a KEGG ID
%  
reaction_class(SID,ClassID):-
        annotation(SID,_,_,ClassID).

%% compartment_class(?SID,?ClassID)
%  mode: nondet
%   ID for an ontol class for this compartment; eg a GO ID or a KEGG ID
%  
compartment_class(SID,ClassID):-
        annotation(SID,_,_,ClassID).

%% model_class(?SID,?ClassID)
%  mode: nondet
%   ID for an ontol class for this model; eg a GO ID or a KEGG ID
%  
model_class(SID,ClassID):-
        annotation(SID,_,_,ClassID).

%% sbentity_class(?SID,?ClassID)
%  mode: nondet
%   ID for an ontol class for this sbentity;
%  sbentity can be a species, model, compartment or reaction
sbentity_class(SID,ClassID):-
        annotation(SID,_,_,ClassID).



%% reaction_participant(?Reaction,?Role,?Participant)
% @param Reaction A reaction is a biochemical process with product, reactant and modifier contituents
% @param Role product | reactant | modifier
% @param Participant molecular constituent which plays some role in a reaction
% union of reaction_product/2, reaction_reactant/2 and reaction_modifier/2
reaction_participant(ID,product,CID):-
        reaction_product(ID,CID).
reaction_participant(ID,reactant,CID):-
        reaction_reactant(ID,CID).
reaction_participant(ID,modifier,CID):-
        reaction_modifier(ID,CID).

%% reaction_participant(?ID,?CID)
% contracted form of reaction_participant/2
reaction_participant(ID,CID):-
        reaction_participant(ID,_,CID).

%% reaction_link(?ReactionID1,?ReactionID2)
reaction_link(RID1,RID2):-
        reaction_link(RID1,RID2,_).

%% reaction_link(?ReactionID1,?ReactionID2,?ViaSpeciesID)
%  
%  two reactions directly linked via a connecting species
%  (excludes modifiers)
reaction_link(RID1,RID2,SID):-
        reaction_product(RID1,SID),
        reaction_reactant(RID2,SID).

%% species_link(?InputID,?OutputID)
species_link(InputID,OutputID):-
        species_link(InputID,OutputID,_).

%% species_link(?InputID,?OutputID,?ReactionID)
%  two species directly linked via a connecting reaction
%  (excludes modifiers)
species_link(InputID,OutputID,RID):-
        reaction_reactant(RID,InputID),
        reaction_product(RID,OutputID).


%% species_path(+InputSpeciesID,?OutputSpeciesID,?Path)
%  finds path between an input species and an output species
%
%  @param Path
%       List of SpeciesID
%  
species_path(InputID,OutputID,Path):-
        species_pathset([path(InputID,[])],
                path(OutputID,Path),
                []).

% (+,?,+)
species_pathset(SrcPaths,IDPath,VisitedIDs):-
        setof(NextPath,
              member_species_link(SrcPaths,NextPath,VisitedIDs),
              NextPaths),
        % each SinkID is a possible solution
        % recursively call to get other possible solutions
        (member(IDPath,NextPaths)
        ;   setof(NextID,X^member(path(NextID,X),NextPaths),NextIDs),
            append(VisitedIDs,NextIDs,NextVisitedIDs),
            species_pathset(NextPaths,IDPath,NextVisitedIDs)).

% nondet; when RID is in path, highly nondet, because of
% promiscuous intermediates
member_species_link(SrcPaths,path(NextID,[ID|RL]),VisitedIDs):-
        member(path(ID,RL),SrcPaths),
        species_link(ID,NextID),
        not(member(NextID,VisitedIDs)).




/** <module> Systems biology SBML-style model


  ---+ Synopsis
  
  ==
  :- use_module(bio(sb_db)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(io)).
  
  summarize_species(SID):-
    species(SID,Name,Comp),
    format('Species: ~w ~w in:~w~n',[SID,Name,Comp]).
  
  demo:-
    load_biofile(sbml,'BIOMD0000000018.xml'),
    load_bioresource(chebi),    % EBI Chemical otnology
    
    forall( (class(CID1,'amino acids'),
             parentRT(CID,CID1),
             species_class(SID,CID)),
           summarize_species(SID)).
  
  ==

  ---+ Description
  
  Models pathways and interactions. Currently the model is very SBML-y
(level 2) but it may change. Tested with EBI BioModels data. No
support for quantitative data as yet.

  ---++ Future Directions

  Although non-quantitative representation of pathways is extremely
useful in itself, the additional of qualitative data would be useful
and the ability to perform simulations extremely useful

  Currently SBML uses MathML to represent equations. It would seem to
be trivial to convert the compound terms derived from the SWI XML
parser and transform them directly into prolog herbrand terms
representing the equations. Functional or constraint-prologramming
(clp) techniques could be used from here

  ---++ Examples

  ==
  :- use_module(bio(sb_db)).
  :- use_module(bio(io)).

  % find reactions spanning distinct compartments
  reactions_spanning_compartments(R1,C1,R2,C2):-
    species(S1,_,C1),
    reaction_reactant(R1,S1),
    reaction_link(R1,R2),
    reaction_product(R2,S2),
    species(S2,_,C2),
    C1 \= C2.
  
  demo:-
    load_biofile(sbml,'BIOMD0000000018.xml'),
    forall(reactions_spanning_compartments(R1,C1,R2,C2),
           format('Reaction ~w in ~w and ~w in ~w~n',[R1,C1,R2,C2])).
  ==

  
  ---++ MathTerms

  A prolog term representation of a mathematical equation, equivalent
in expressive power to MathML. They are used for kinetic_law/2 and
assignment_rule/2

  A mathterm is a recursive prolog term:

*  =|MathTerm = Func(MathTerms) ; ci(Var) ; cn(Const)|=
  
*  =|Func = times ; divide ; ...|=

  ---++ Importing and exporting data

  This is a data module. Facts can be imported and exported from both
prolog fact databases and other formats
  
  ---+++ Import

  The following file formats can be read in using load_biofile/2
and load_bioresource/1

  
  * sbml - via sb_xmlmap_dbml
  * biopax (level 1) - with sb_bridge_from_biopax
  

  ---+++ Export

  The following file formats can be written using write_biofile/2

  
  * chadoxml - via io_chadoxml
  * dot - via sb_bridge_to_dot
  

  ---++ BioPax

  sb_db is not yet fully integrated with biopax.
  However, you can use the auto-generated biopax1_db and biopax2_db schemas:

==
  :- use_module(bio(biopax2_db)).

  % find reactions spanning distinct compartments
  reactions_spanning_compartments(R1,C1,R2,C2):-
    species(S1,_,C1),
    reaction_reactant(R1,S1),
    reaction_link(R1,R2),
    reaction_product(R2,S2),
    species(S2,_,C2),
    C1 \= C2.
  
  demo:-
    load_biofile(owl,'Homo sapiens.owl'),
    Name='Purine metabolism',
    
    forall(reactions_spanning_compartments(R1,C1,R2,C2),
           format('Reaction ~w in ~w and ~w in ~w~n',[R1,C1,R2,C2])).
==


  */
