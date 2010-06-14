% ----------------------------------------
% ontol_biosources_shared
% ----------------------------------------
% common to both local and remote

:- multifile bioresource/2,bioresource/3,bioresource/4.

% ----------------------------------------
% GO
% ----------------------------------------

% EXPANSION RULES FOR GO XPS
user:bioresource(goxp(N),Path,obo):-
	nonvar(N),
	absolute_file_name(go('scratch/xps/'),P1),
	concat_atom([P1,N,'.obo'],Path).
user:bioresource(obolr(N),Path,obo):-
	nonvar(N),
	absolute_file_name(go('scratch/obol_results/'),P1),
	concat_atom([P1,N,'-obol.obo'],Path).

% EXPANSION RULES FOR GENE ASSOCIATIONS
user:bioresource(go_assoc_local(N),go_gene_associations(Path),gzip(go_assoc)):- nonvar(N),concat_atom(['gene_association.',N,'.gz'],Path).
user:bioresource(go_assoc_submit(N),go_gene_associations(Path),gzip(go_assoc)):- nonvar(N),concat_atom(['submission/gene_association.',N,'.gz'],Path).
user:bioresource(go_assoc(N),url(URL),gzip(go_assoc)):- nonvar(N),concat_atom(['http://www.geneontology.org/gene-associations/gene_association.',N,'.gz'],URL).
user:bioresource(go_assoc_version(N,V),url(URL),gzip(go_assoc)):-
        nonvar(N),
        concat_atom(['http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/~checkout~/go/gene-associations/gene_association.',N,'.gz?rev=',V,';content-type=application%2Fx-gzip.'],URL).

% ONTOLOGIES
user:bioresource(go,go('ontology/editors/gene_ontology_write.obo'),obo).
user:bioresource(go_public,go('ontology/gene_ontology_edit.obo'),obo).

user:bioresource(obo_download(N),obo_download(Path),obo):- nonvar(N),concat_atom([N,'/',N,'.obo'],Path).
user:bioresource(obo(N),url(Path),obo):- nonvar(N),concat_atom(['http://purl.org/obo/obo-all/',N,'/',N,'.obo'],Path).
user:bioresource(obo2(N),url(Path),obo):- nonvar(N),concat_atom(['http://purl.org/obo/obo/',N,'.obo'],Path).
user:bioresource(obop(N),url(Path),ontol_db:pro):- nonvar(N),concat_atom(['http://purl.org/obo/obo-all/',N,'/',N,'.pro'],Path).
