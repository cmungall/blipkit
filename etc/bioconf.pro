%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bioconf.pro - bio configuration file for prolog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:file_search_path(home, Home) :-
	getenv('HOME',Home).

% TODO:
%  make this more configurable, such that the
%  this core conf file can be used without all
%  packages installed
:- [bio(conf/ontol_bioresources_local)].
:- [bio(conf/sb_bioresources_local)].


%% ---++ bioconf
%% This file contains the local blip configuration

:- multifile bioresource/2,bioresource/3,bioresource/4.
:- multifile user:uri_resolution/2.

user:file_search_path(data_cache,home('.blip/data_cache')).

user:prolog_file_type(pro,prolog).
user:prolog_file_type('P',prolog).

% --Triple20--
user:file_search_path(triple20, '/users/cjm/cvs/Triple20/src').

% amigo: core
user:file_search_path(amigo_src,'/users/cjm/cvs/bioprolog/apps/amigo/src').
user:file_search_path(amigo_conf,'/users/cjm/cvs/bioprolog/apps/amigo/conf').

% amigo: obol
user:file_search_path(amigo_src,'/users/cjm/cvs/bioprolog/apps/obol/src').
user:file_search_path(amigo_conf,'/users/cjm/obol2/conf').

user:file_search_path(biowarehouse,'/users/cjm/cvs/biowarehouse').
user:file_search_path(phenotype_commons,'/users/cjm/cvs/phenotype-commons/annotations').


:- discontiguous user:bioresource/2, user:bioresource/3, user:bioresource/4.

user:bioresource(rdb(go),odbc_connect(go,[user(cjm),alias(go),open(once)]),go).
user:bioresource(rdb(go2),plsql_connect(go,[driver(mysql),args('-h 127.0.0.1')]),go).
user:bioresource(rdb(go_latest),odbc_connect(go_latest,[user(cjm),alias(go_latest),open(once)]),go).
user:bioresource(rdb(go_load),odbc_connect(go_load,[user(cjm),alias(go_load),open(once)]),go).
user:bioresource(rdb(reactome),odbc_connect(reactome,[user(cjm),alias(hs),open(once)]),reactome).
user:bioresource(rdb(hs),odbc_connect(hs,[user(cjm),alias(hs),open(once)]),enscore).
user:bioresource(rdb(cv),odbc_connect(cv,[user(cjm),alias(cv),password(''),open(once)]),chado).
user:bioresource(rdb(cjmchado),odbc_connect(cjmchado,[user(cjm),alias(cjmchado),password(''),open(once)]),chado).
user:bioresource(rdb(chado),odbc_connect(chado,[user(cjm),alias(chado),password(''),open(once),silent(false)]),chado).
user:bioresource(rdb(obd),odbc_connect(obd,[user(cjm),alias(obd),password(''),open(once),silent(true)]),obd).
user:bioresource(rdb(obd2),odbc_connect(obd2,[user(cjm),alias(obd2),password(''),open(once),silent(false)]),obd).
user:bioresource(rdb(obdtest),odbc_connect(obdtest,[user(cjm),alias(obdtest),password(''),open(once),silent(false)]),obd).
user:bioresource(rdb(cjmtest),odbc_connect(cjmtest,[user(cjm),alias(cjmtest),password(''),open(once)]),cjmtest).
user:bioresource(rdb(flybase),odbc_connect(flybase,[user(flybase),alias(flybase),password(''),open(once)]),chado).
user:bioresource(obd(DB),odbc_connect(DB,[user(cjm),alias(DB),password(''),open(once),silent(true)]),obd):- nonvar(DB).
user:bioresource(chado(DB),odbc_connect(DB,[user(cjm),alias(DB),password(''),open(once),silent(true)]),chado):- nonvar(DB).
user:bioresource(enscore(DB),odbc_connect(DB,[user(anonymous),alias(DB),password(''),open(once),silent(true)]),enscore):- nonvar(DB).
user:bioresource(ucsc(DB),odbc_connect(DB,[user(genome),alias(DB),open(once),silent(true)]),ucsc):- nonvar(DB).
user:bioresource(godb(DB),odbc_connect(DB,[user(cjm),alias(DB),open(once)]),go):- nonvar(DB).
user:bioresource(ebi_go,odbc_connect(ebi_go,[user(go_select),alias(ebi_go),password('amigo'),open(once),silent(true)]),go).
user:bioresource(obdphenoscape,odbc_connect(obdphenoscape,[user(cjm),alias(obdphenoscape),password(''),open(once),silent(true)]),obd).
user:bioresource(owlgres,odbc_connect(owlgres,[user(cjm),alias(owlgres),password(''),open(once),silent(true)]),owlgres).

user:file_search_path(serval_conf, '/users/cjm/cvs/bioprolog/conf').
user:file_search_path(localncbigene, datadir('NCBI/Gene')).
user:file_search_path(datadir, '/users/cjm/Data').

% --Sesame/SeRQL Servers--
user:bioresource(sesame(local),connect([host(localhost),
                                        port(3020),
                                        path(''),
                                        repository('mem-rdfs-db')])).

% --GO.xrf_abbs--
user:bioresource(xrf,'http://www.berkeleybop.org/ontologies/obo-all/go_xrf_metadata/go_xrf_metadata.obo',obo).

% --WordNet--
user:file_search_path(wn, ontdir('wn')).
user:bioresource(ontol_wn,wn('ontol_wn.pro'),pro,ontol_db).
user:bioresource(wn2ubo,wn('wn2ubo.obo'),obo).
user:bioresource(wn,wn('all_wn.pl'),pro,wn_db).
user:bioresource(wn_s,wn('wn_s.pl'),pro,wn_db).
user:bioresource(wn_hyp,wn('wn_hyp.pl'),pro,wn_db).
user:bioresource(wn_mm,wn('wn_mm.pl'),pro,wn_db).
user:bioresource(wn_ms,wn('wn_ms.pl'),pro,wn_db).
user:bioresource(wn_mp,wn('wn_mp.pl'),pro,wn_db).
user:bioresource(wn_der,wn('wn_der.pl'),pro,wn_db).


% --Proteomics--
user:bioresource(interpro,datadir('interpro.obo-xml'),obo_xml).

% --Comparative--
user:bioresource(inparanoid,datadir('inparanoid/all.tbl'),inparanoid_tbl).

% http://jura.wi.mit.edu/young_public/regulatory_network/binding_by_gene.tsv

% --Base Ontologies--
user:bioresource(rdfs,'/users/cjm/cvs/Triple20/Ontologies/Base/rdfs.rdfs',rdfs).
user:bioresource(owl,'/users/cjm/cvs/Triple20/Ontologies/Base/owl.owl',owl).
user:bioresource(dc,url('http://purl.org/dc/terms'),rdfs).
user:bioresource(protege_dc,url('http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl'),owl).
user:bioresource(skos,url('http://www.w3.org/2004/02/skos/core/history/2006-04-18.rdf'),owl).

% --Models--
%user:bioresource(phenmodel,obo_cvs('phenotype/phenmodel.obo'),obo).
%user:bioresource(sox,song('ontology/sox.obo'),obo).



user:file_search_path(ncbigene, 'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/').
user:file_search_path(ncbigene(Family,Species),url(X),gzip(gene_info)) :-
        sformat(X,'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/~w/~w.gene_info.gz',[Family,Species]).
user:bioresource(ncbi_gene_info,url('ftp://ftp.ncbi.nih.gov/gene/DATA/gene_info.gz'),gzip(gene_info)).
user:bioresource(ncbi_generifs,url('ftp://ftp.ncbi.nih.gov/gene/GeneRIF/generifs_basic.gz'),gzip(gene_rif)).
user:bioresource(homologene,url('ftp://ftp.ncbi.nih.gov/pub/HomoloGene/current/homologene.data'),homoltbl).

user:bioresource(iproclass,url('ftp://ftp.pir.georgetown.edu/databases/iproclass/iproclass.tb.gz'),iproclass).

user:bioresource(seq2pthr,url('ftp://ftp.pantherdb.org/genome/pthr7.0/seq2pthr.gz'),gzip(tbl(seq2pthr))).

% e.g. 10116_rattus_norvegicus
%user:bioresource(gp2p(N),url(Path),tbl(gp2p)):- nonvar(N),concat_atom(['ftp://ftp.ebi.ac.uk/pub/contrib/qfo/gp2protein.',N],Path).
user:bioresource(gp2p(N),url(Path),gzip(tbl(gp2p))):- nonvar(N),concat_atom(['ftp://ftp.ebi.ac.uk/pub/databases/reference_proteomes/gp2protein.',N,'.gz'],Path).
user:bioresource(qfo(N),url(Path),gzip(fasta)):- nonvar(N),concat_atom(['ftp://ftp.ebi.ac.uk/pub/databases/reference_proteomes/',N,'.fasta.gz'],Path).

% e.g. 10000
user:bioresource(pthr(N),url(Path),nhx):- nonvar(N),concat_atom(['http://amigo.berkeleybop.org/amigo/panther/PTHR',N,'.tree'],Path).
user:bioresource(paint(N),Path,nhx):- nonvar(N),concat_atom(['/users/cjm/cvs/go/gene-associations/submission/paint/PTHR',N,'/PTHR',N,'.save.tree'],Path).

% --OBDPatoData--

% NEW
user:bioresource(mgi_genotype_phenotype,phenotype_commons('MGI/genotype_phenotype.rpt'),tbl(g2p)).
user:bioresource(mgi_gene_phenotype,phenotype_commons('MGI/gene_phenotype.txt'),tbl(g2p)).
user:bioresource(omim_phenotype,phenotype_commons('Human/disorder_phenotype.txt'),tbl(g2p)).
user:bioresource(mgi_genotype_gene,phenotype_commons('MGI/genotype_gene.rpt'),tbl(genotype_gene)).
user:bioresource(zfin_gene_phenotype,phenotype_commons('ZFIN/gene_phenotype.txt'),tbl(g2p)).

user:bioresource(omim2gene,biowarehouse('omim/disorder2ncbigene.txt'),txt).


% --NCBI--
user:bioresource(taxnames,home('Data/ncbitax/names.dmp'),ncbitaxname).
user:bioresource(taxnodes,home('Data/ncbitax/nodes.dmp'),ncbitaxnode).
%user:bioresource(taxonomy,ontdir('NCBITaxonomy.pro'),pro,ontol_db).
user:bioresource(taxonomy,obo_download('ncbi_taxonomy/ncbi_taxonomy.pro'),pro,ontol_db).
user:bioresource(taxonomy_stemmed,ontdir('ncbi_taxonomy_stemmed.obo'),obo).
%user:bioresource(gotax,'/users/cjm/cvs/go/scratch/go-taxon/TaxonGOLinksFile.obo',obo).
%user:bioresource(taxslim,'/users/cjm/cvs/go/scratch/go-taxon/ncbitax-slim.obo',obo).
user:bioresource(gotax,'/users/cjm/cvs/go/quality_control/annotation_checks/taxon_checks/taxon_go_triggers.obo',obo).
user:bioresource(taxslim,'/users/cjm/cvs/go/quality_control/annotation_checks/taxon_checks/ncbi_taxon_slim.obo',obo).

% Genome-wide association catalog
user:bioresource(gwas,url('http://www.genome.gov/admin/gwascatalog.txt'),tbl(gwas)).


user:bioresource(gi_taxid_nucl,home('Data/NCBI/gi_taxid_nucl.dmp.gz'),gzip(gi_taxid)).

user:bioresource(shape,url('http://www.aimatshape.net/resources/aas-ontologies/shapecommonontology.owl'),owl).

% --Other/NLP--
user:bioresource(words,datadir('words.pro'),pro,user).


