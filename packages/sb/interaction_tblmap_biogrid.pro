:- module(interaction_tblmap_biogrid,[]).

:- use_module(interaction_db).
:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(seqfeature_db)).

%% interaction(INTERACTOR_A, INTERACTOR_B, OFFICIAL_SYMBOL_A, OFFICIAL_SYMBOL_B, ALIASES_FOR_A, ALIASES_FOR_B, EXPERIMENTAL_SYSTEM, SOURCE, PUBMED_ID, ORGANISM_A_ID, ORGANISM_B_ID)
% this replicates the biogrid table
%:- extensional(interaction/11).

interspecies_interaction(G1,G2,Tax1,Tax2) :-
        interaction(_,_,G1,G2,_,_,_,_,_,Tax1,Tax2),
        Tax1\=Tax2.

intraspecies_interaction(G1,G2,Tax,Tax) :-
        interaction(_,_,G1,G2,_,_,_,_,_,Tax,Tax).

map_interaction(X1,X2,Tax) :-
        interaction(_,_,G1,G2,_,_,_,_,_,Tax,Tax),
        entity_label(X1,G1),
        entity_label(X2,G2).

/** <module> 

  biogrid tbl file can be downloaded from here
http://www.thebiogrid.org/downloadfile.php?type=current&file=1  

  this module has utility query predicates
  
*/

