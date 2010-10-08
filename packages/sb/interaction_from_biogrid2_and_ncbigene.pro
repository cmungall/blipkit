:- module(interaction_from_biogrid_and_reactome,
          [
           index_ncbigene/2
          ]).

:- use_module(bio(interaction_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(metadata_db)).

interaction_from_biogrid(X1,X2) :-
        functor(Goal,interaction,24),
        arg(Goal,2,G1),
        arg(Goal,3,G2),
        Goal,
        entity_xref(X1,G1),
        entity_xref(X2,G2).

% blip io-convert -r reactome/Homo_sapiens  -debug dot -r biogrid -u interaction_from_biogrid_and_reactome -to interaction_db:pro
interaction_db:interacts_with(X1,X2) :-
        interaction_from_biogrid(X1,X2).

index_ncbigene :-
        ensure_loaded(bio(index_util)),
        materialize_index(metadata_db:entity_xref/2).

