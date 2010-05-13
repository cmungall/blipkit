:- module(homol_bridge_to_ontol,[]).

:- use_module(bio(curation_db)).
%:- use_module(bio(homol_bridge_to_curation)). % too abstract..?
%:- use_module(bio(curation_bridge_to_ontol)).
:- use_module(bio(homol_db)).
:- use_module(bio(ontol_db)).
%:- use_module(bio(taxon_db)).
:- use_module(bio(bioprolog_util)).

hset_gene(HSet,AncGene):- homologset(HSet),atom_concat('OBD:homologene-',HSet,AncGene).
ancgene(AncGene):- hset_gene(_,AncGene).
% ancestral relation

% we treat each homology set as a curation with about an ancestral gene,
% each statement stating derivation from that gene
% (todo: there is something a little closed-world about this. perhaps we need a stronger relation?)
curation_db:curation(HSet):- homologset(HSet).
curation_db:curation_statement(HSet,Gene,'OBO_REL:descended_from',AncGene):- homologset_member(HSet,Gene,_),hset_gene(HSet,AncGene).
metadata_db:entity_resource(HSet,homologene):- homologset(HSet). % should be there already??
metadata_db:entity_publisher(HSet,homologene):- homologset(HSet). % should be there already??

ontol_db:inst(AncGene):- ancgene(AncGene).
ontol_db:inst_of(AncGene,'SO:0000704'):- ancgene(AncGene).
metadata_db:entity_resource(AncGene,homologene):- ancgene(AncGene).
metadata_db:entity_publisher(AncGene,homologene):- ancgene(AncGene).
metadata_db:entity_comment(AncGene,'putative gene implied by homolgy grouping'):- ancgene(AncGene).
ontol_db:inst_rel(AncGene,'OBO_REL:inheres_in',Tax):- homologset_taxid(HSet,Tax),hset_gene(HSet,AncGene).

% leads to duplicates? required so we know that reified statement is a restriction, not inst_rel
ontol_db:inst(Gene):- homologset_member(_,Gene,_).
ontol_db:inst_of(Gene,'SO:0000704'):- homologset_member(_,Gene,_). % dupes?
%taxon_db:entity_taxon(Gene,Taxon):- homologset_member(_,Gene,Taxon).
ontol_db:inst_rel(Gene,'OBO_REL:inheres_in',Taxon):- homologset_member(_,Gene,Taxon).

metadata_db:entity_label(Gene,Symbol):- homologset_member(_,Gene,_,Symbol).
metadata_db:entity_resource(Gene,'NCBIGene'):- homologset_member(_,Gene,_).
metadata_db:entity_publisher(Gene,'NCBIGene'):- homologset_member(_,Gene,_).
%ontol_db:restriction(Gene,'oban:evolutionary_ancestor_of',AncGene):- anc(Gene,AncGene).

% extra info - may move from homologene somewhere else - annotation in its own right
curation_db:curation_statement(HSet,Gene,'oban:has_role',Class):- homologset_member_phenotype(HSet,Gene,Class).

% owl full as not an annot prop?
% ontol_db:inst_rel(HSet,'OBO_REL:has_quality','oban:Homologous'):- homologset(HSet).

/** <module> <module>

  we treat every putative homologous set as corresponding to some putative ancestral gene type
  */
