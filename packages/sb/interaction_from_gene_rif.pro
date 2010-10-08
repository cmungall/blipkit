:- module(interaction_from_gene_rif,
          [
           interaction_pub_rif/5,
           index_gene_rif/0
           ]).

:- use_module(bio(interaction_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(index_util)).



interaction_pub_rif(T,G1,G2,Pub,Rif) :-
        gene_rif(T,G1,Pub,_,Rif),
        gene_rif(T,G2,Pub,_,_).
interaction_db:interacts_with(G1,G2) :-
        interaction_pub_rif(_,G1,G2,_,_).


index_gene_rif :-
        materialize_index(user:gene_rif(1,1,1,0,0)).


/*

  Examples:
  
  blip-findall -r gene/9606 -i generifs_basic.pro -u interaction_from_gene_rif -goal index_gene_rif "interaction_pub_rif('NCBITaxon:9606',G1,G2,P,Rif)" 

*/