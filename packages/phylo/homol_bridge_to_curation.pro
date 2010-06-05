:- module(homol_bridge_to_curation,[]).

:- use_module(bio(curation_db)).
:- use_module(bio(homol_db)).

EXPERIMENTAL!!

JUST USE _to_ontol INSTEAD!!



hset_gene(HSet,AncGene):- homologset(HSet),atom_concat('OBD:ancestral_gene-',HSet,AncGene).
ancgene(AncGene):- hset_gene(_,AncGene).
% ancestral relation

% we treat each homology set as a curation with about an ancestral gene,
% each statement stating derivation from that gene
curation_db:curation(HSet):- homologset(HSet).
curation_db:curation_statement(HSet,Gene,'oban:evolutionary_ancestor_of',AncGene):- homologset_member(HSet,Gene,_),hset_gene(HSet,AncGene).
metadata_db:entity_resource(HSet,homologene):- homologset(HSet).% should be there already??

% extra info - may move from homologene somewhere else - annotation in its own right
curation_db:curation_statement(HSet,Gene,'oban:has_role',Class):- homologset_member_phenotype(HSet,Gene,Class).

/** <module> <module>

  we treat every putative homologous set as corresponding to some putative ancestral gene type
  */