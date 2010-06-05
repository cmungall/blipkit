:- module(interaction_from_biogrid_and_reactome,[]).

:- use_module(bio(interaction_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(metadata_db)).

interaction_from_biogrid(X1,X2,Sys,Src,PMID,Tax) :-
        interaction(_,_,G1,G2,_,_,Sys,Src,PMID,Tax,Tax),
        entity_label(E1,G1),
        entity_xref(E1,X1),
        id_idspace(X1,'UniProtKB'),
        entity_label(E2,G2),
        entity_xref(E2,X2),
        id_idspace(X2,'UniProtKB').

% blip io-convert -r reactome/Homo_sapiens  -debug dot -r biogrid -u interaction_from_biogrid_and_reactome -to interaction_db:pro
interaction_db:interacts_with(X1,X2) :-
        interaction_from_biogrid(X1,X2,_Sys,_Src,_PMID,9606).

