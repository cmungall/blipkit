:- module(pkb_from_obo,[]).

:- use_module(pkb_db).
%:- use_module(phenotype_db).
:- use_module(bio(tabling)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(genome_db)).

:- multifile metadata_db:entity_label/2.

% ASSUMED FACTS:
% g2p/2
feature_phenotype(F,P) :- g2p(F,P).

% ----------------------------------------
% SPECIES
% ----------------------------------------
% harcoded metadata for now

pkb_db:species(S) :- species_label(S,_).
pkb_db:species_label('NCBITaxon:7955',zebrafish). 
pkb_db:species_label('NCBITaxon:9606',human).
pkb_db:species_label('NCBITaxon:10090',mouse). 
foo('0').

metadata_db:entity_label(S,L) :- species_label(S,L).

% ----------------------------------------
% GENES
% ----------------------------------------
% assumes ontol_db representation of gene set using SO
% (e.g. gene/9606)
% only genes with phenotypes

genome_db:gene(G) :-
	inst_ofRT(G,'SO:0000704'), entity_label(G,_), \+ \+ feature_phenotype(G,_).
genome_db:gene_symbol(G,L) :-
	inst_ofRT(G,'SO:0000704'), entity_label(G,L), \+ \+ feature_phenotype(G,_).

% ----------------------------------------
% ORGANISMS
% ----------------------------------------
% MODs only, not OMIM
% for every gene, manifest a representative organism that bears the mutated version of that gene

pkb_db:organism_variant_gene(Org,G) :-
	gene(G),
	atom_concat(G,'-mutant',Org).

% simple - treat each gene/genotype as organism for now (must have annotation)
% TODO - transgenes
pkb_db:organism(Org) :- organism_variant_gene(Org,_).

pkb_db:organism_species(Org,Species) :-
	organism_variant_gene(Org,G),
	inst_rel(G,encoded_by,Species).

% this is for MODs - need a separate mapping for OMIM?
pkb_db:organism_label(Org,L) :-
	organism_variant_gene(Org,G),
	entity_label(G,X),
	atom_concat(X,' mutant',L).

pkb_db:organism_type(Org,Type) :-
	organism_species(Org,Type). % TODO - transgene construct etc

pkb_db:organism_role(Org,model) :-
	organism(Org).		% TODO

pkb_db:organism_description(Org,Desc) :-
        organism(Org),
        entity_comment(Org,Desc).

pkb_db:organism_phenotype(Org,P) :-
	organism_variant_gene(Org,G),
        feature_phenotype(G,P1),
        iduri(P,P1).
pkb_db:phenotype_quad(P,P) :-
	feature_phenotype(_,P),
	P=(_,_,_,_).

% ----------------------------------------
% DISEASES
% ----------------------------------------
% use ontol_db representation of omim

pkb_db:disease_gene_variant(O,G,any) :-
	restriction(O,associated_with,OG),
	entity_xref(G,OG).


iduri(-,-) :- !.
iduri(thing,-) :- !.
%iduri(URI,URI) :- var(URI),!.
%iduri(ID,URI) :-
%        concat_atom([S,Loc],':',ID),
%        !,
%        concat_atom(['http://purl.org/obo/owl/',S,'#',S,'_',Loc],URI).
iduri(URI,URI) :- !.



