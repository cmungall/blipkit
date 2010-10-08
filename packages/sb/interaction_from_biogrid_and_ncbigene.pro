:- module(interaction_from_biogrid_and_reactome,
          [
           index_ncbigene/2
          ]).

:- use_module(bio(interaction_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(metadata_db)).

interaction_from_biogrid(X1,X2,Sys,Src,PMID,Tax) :-
        interaction(_,_,G1,G2,_,_,Sys,Src,PMID,Tax,Tax),
        entity_label_or_synonym(X1,G1),
        entity_label_or_synonym(X2,G2).

% blip io-convert -r reactome/Homo_sapiens  -debug dot -r biogrid -u interaction_from_biogrid_and_reactome -to interaction_db:pro
interaction_db:interacts_with(X1,X2) :-
        interaction_from_biogrid(X1,X2,_Sys,_Src,_PMID,_).

index_ncbigene :-
        ensure_loaded(bio(index_util)),
        materialize_index(metadata_db:entity_label_or_synonym/2).

