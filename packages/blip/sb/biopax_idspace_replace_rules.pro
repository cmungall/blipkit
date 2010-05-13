
% pathwaycommons and reactome all use different IDspaces from the standard ones in http://www.geneontology.org/cgi-bin/xrefs.cgi

%% Rewrite whole ID:

:- multifile user:replace_id/2.

% EXAMPLE Makefile:
%: chebi-replace_id.pro:
%:	blip -r chebi findall "entity_xref(A,B)" -select "user:replace_id(B,A)" -write_prolog > $@
%: %-pathway_db.pro: %.owl
%:	blip -i chebi-replace_id.pro -u biopax2_bridge_to_pathway io-convert -to pathway_db:pro -i $< -o $@

% humancyc
db_fix('KEGG LIGAND',ID,'CHEBI',ID2) :-
	concat_atom(['KEGG_COMPOUND',ID],':',KeggXrefInCHEBI),
	user:replace_id(KeggXrefInCHEBI,CHEBI_ID),
	%entity_xref(CHEBI_ID,KeggXrefInCHEBI), CYCLES
	!,
	concat_atom(['CHEBI',ID2],':',CHEBI_ID).
db_fix(DB,ID,DB2,ID) :-
	!,
	db_fix(DB,DB2).

%% Rewrite IDspace only


%db_fix(X,X) :- var(X) 
db_fix('ChEBI','CHEBI') :- !.
db_fix('UniProt','UniProtKB') :- !.
db_fix('UNIPROT','UniProtKB') :- !.
db_fix('REACTOME','Reactome') :- !.
db_fix('GENE_ONTOLOGY','GO') :- !.
db_fix('PUBMED','PMID') :- !.
db_fix('ENTREZ_GENE','NCBI_Gene') :- !.
db_fix(X,X).
