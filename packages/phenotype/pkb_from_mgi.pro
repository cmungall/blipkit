:- module(pkb_from_mgi,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
:- use_module(bio(tabling)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(genome_db)).

:- multifile metadata_db:entity_label/2.

pkb_db:species(S) :- species_label(S,_).
pkb_db:species_label('NCBITaxon:7955',zebrafish). 
pkb_db:species_label('NCBITaxon:9606',human).
pkb_db:species_label('NCBITaxon:10090',mouse). 
foo('0').

% TODO : reuse pkb_from_obo

metadata_db:entity_label(S,L) :- species_label(S,L).

genome_db:gene(G) :-
	inst_ofRT(G,'SO:0000704'), entity_label(G,_), \+ \+ gene_mp(G,_).
genome_db:gene_symbol(G,L) :-
	inst_ofRT(G,'SO:0000704'), entity_label(G,L), \+ \+ gene_mp(G,_).

% MODs only...
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
	gene_mp(G,P).
pkb_db:phenotype_quad(P,PQ) :-
	class(P),
	\+ \+ gene_mp(_,P),
	mp_nr_subsumed_by_phenotype(P,PQ).

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




mp_phenotype(MP,(E,Q,D,W)) :-
        genus(MP,Q),
        belongs(Q,quality),
        (   differentium(MP,'OBO_REL:inheres_in',E)
        ->  true
        ;   E=(-)),
        (   differentium(MP,'OBO_REL:inheres_in_part_of',W)
        ->  true
        ;   W=(-)),
        (   differentium(MP,'OBO_REL:towards',D)
        ->  true
        ;   D=(-)).

mp_subsumed_by_phenotype(MP,P) :-
        subclassRT(MP,MP1),
        mp_phenotype(MP1,P).

mp_nr_subsumed_by_phenotype(MP,P) :-
        subclassRT(MP,MP1),
        mp_phenotype(MP1,P),
        \+ ((subclassRT(MP,MP2),
             mp_phenotype(MP2,_),
             subclassT(MP2,MP1))).
