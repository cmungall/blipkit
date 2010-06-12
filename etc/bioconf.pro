%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bioconf.pro - bio configuration file for prolog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO - normalize some of this file, make it usable for others

% done this so far for ontologies, need to split out others
:- [bio(conf/ontol_bioresources_local)].


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
user:bioresource(rdb(wn),odbc_connect(wn,[user(cjm),alias(wn),open(once)]),wn).
user:bioresource(rdb(crm),odbc_connect(crm,[user(cjm),alias(srm),password(''),open(once)]),chado).
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
%user:bioresource(xrf,'/users/cjm/obd/conf/go_xrf_abbs.obo',obo).
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

% --Genomics--
user:bioresource(testcg,'/users/cjm/chaos-xml/sample-data/Rab1.chaos-xml',chaos).
user:bioresource(testsox,ontdir('testsox.owl'),owl).
user:bioresource(testobd,ontdir('obdtest.owl'),owl).

% --Pathways--
user:bioresource(rhea,datadir('rhea/rhea-pathway_db.pro'),pathway_db:pro).
%user:bioresource(reactome,datadir('Pathways/reactome.sbml'),sbml).
user:bioresource(biopax1,url('http://www.biopax.org/release/biopax-level1.owl'),owl).
user:bioresource(biopax2,url('http://www.biopax.org/release/biopax-level2.owl'),owl).
user:bioresource(biopax3,url('http://www.biopax.org/release/biopax-level3.owl'),owl).

user:bioresource(biopax_glycolysis,datadir('Pathways/biopax-level1/biopax-example-ecocyc-glycolysis'),owl).
% blip -r reactome_biopax/Drosophila\ melanogaster
user:bioresource(reactome_biopax(Sp),Path,owl) :-
        nonvar(Sp),
        sformat(Path,'/Users/cjm/cvs/biowarehouse/reactome/~w.owl',[Sp]).
%        sformat(Path,'/Users/cjm/data/reactome-bp3/~w.owl',[Sp]).
user:bioresource(reactome(Sp),Path,pathway_db:pro) :-
        nonvar(Sp),
        sformat(Path,'/Users/cjm/cvs/biowarehouse/reactome/~w-pathway_db.pro',[Sp]).
%        sformat(Path,'/Users/cjm/data/reactome-bp3/~w-pathway_db.pro',[Sp]).

user:bioresource(pathway_commons(Sp),url(Path),gzip(owl)) :-
        nonvar(Sp),
        sformat(Path,'http://www.pathwaycommons.org/pc-snapshot/biopax/by_species/~w.owl.zip',[Sp]).



% --Disease--
%user:bioresource(disease,obo_download('disease_ontology/disease_ontology.obo'),obo).
user:bioresource(disease_dn,ontdir('diseaseontology/HumanDO_downcase.obo'),obo).
user:bioresource(disease,cvs('diseaseontology/HumanDO.obo'),obo).
user:bioresource(disease_stemmed,ontdir('disease/DO_stemmed.pro'),pro,ontol_db).
user:bioresource(disease_xp,'/Users/cjm/cvs/obo/ontology/phenotype/disease_xp/disease_xp_all-merged.obo',obo).
user:bioresource(disease2gene,url('http://django.nubic.northwestern.edu/fundo/media/data/do_lite.txt'),txt).
user:bioresource(do_rif,url('http://projects.bioinformatics.northwestern.edu/do_rif/do_rif.human.txt'),do_rif).
user:bioresource(omim,biowarehouse('omim/omim.obo'),obo).
user:bioresource(omim2gene,biowarehouse('omim/disorder2ncbigene.txt'),txt).
user:bioresource(generif,'/users/cjm/cvs/obo-database/build/build-ncbi-gene/generifs_basic.gz',gzip(gene_rif)).
user:bioresource(ido,obo_cvs('phenotype/infectious_disease.obo'),obo).
user:bioresource(mgip,'/users/cjm/obd/data/phenotype_annotation/MGI/source_files/gene_mp-curation_db.pro',curation_db:pro).
user:bioresource(mgi_gene,'/users/cjm/obd/data/phenotype_annotation/MGI/source_files/gene.obo',obo).
user:bioresource(ogms,cvs('ogms-read-only/src/ontology/ogms.obo'),obo).

% --Proteomics--
user:bioresource(interpro,datadir('interpro.obo-xml'),obo_xml).

% --Comparative--
user:bioresource(inparanoid,datadir('inparanoid/all.tbl'),inparanoid_tbl).

% --Interaction--
% TODO - new URL?
%user:bioresource(biogrid,url('http://www.thebiogrid.org/downloadfile.php?type=current&file=1'),gzip(tbl(interaction))).
user:bioresource(biogrid,'/Users/cjm/src/biogrid/biogrid.tab.txt',tbl(interaction)).
user:bioresource(biogrid_hs,'/Users/cjm/src/biogrid/biogrid-hs.pro',interaction_db:pro).
user:bioresource(biogrid_hs_gene,'/Users/cjm/src/biogrid/biogrid-hs-gene.pro',interaction_db:pro).
% Reactome convention: genus_species (all lowercase)
user:bioresource(reactome_interactions(Sp),url(URL),gzip(tbl(reactome_interaction))) :-
        nonvar(Sp),
        sformat(URL,'http://www.reactome.org/download/current/~w.interactions.txt.gz',[Sp]).
% hs only?
user:bioresource(reactome_mitab(Sp),url(URL),gzip(tbl(reactome_interaction))) :-
        nonvar(Sp),
        sformat(URL,'http://www.reactome.org/download/current/~w.mitab.interactions.txt.gz',[Sp]).

% http://jura.wi.mit.edu/young_public/regulatory_network/binding_by_gene.tsv

% --Base Ontologies--
user:bioresource(rdfs,'/users/cjm/cvs/Triple20/Ontologies/Base/rdfs.rdfs',rdfs).
user:bioresource(owl,'/users/cjm/cvs/Triple20/Ontologies/Base/owl.owl',owl).
user:bioresource(dc,url('http://purl.org/dc/terms'),rdfs).
user:bioresource(protege_dc,url('http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl'),owl).
user:bioresource(skos,url('http://www.w3.org/2004/02/skos/core/history/2006-04-18.rdf'),owl).

% --Upper Ontologies--
user:bioresource(ubo,obo_cvs('upper_bio_ontology/ubo.obo'),obo).
%user:bioresource(bfo,obo_cvs('upper_bio_ontology/bfo.obo'),obo).
user:bioresource(bfo,[obo(bfo)]).
user:bioresource(bfo2_obo,home('cvs/bfo/src/ontology/bfo2-classes.obo'),obo).
user:bioresource(bfo2_rel,home('cvs/bfo/src/ontology/bfo2-relations.obo'),obo).
user:bioresource(dolcelite,ontdir('upper/DolceLite/DOLCE-Lite_397.owl'),owl).
user:bioresource(sumo,ontdir('upper/SUMO/SUMO.owl'),owl).
user:bioresource(sumo_pro,ontdir('upper/SUMO/SUMO.pro'),pro,ontol_db).
user:bioresource(opencyc,ontdir('upper/opencyc.owl'),owl).
user:bioresource(biotop,url('http://www.ifomis.org/biotop/biotop.owl'),owl).

% --Non-Bio Ontologies--
user:bioresource(wine,ontdir('wine.owl'),owl).
user:bioresource(bible,ontdir('NTNames.owl'),owl).
user:bioresource(food,ontdir('food.owl'),owl).
user:bioresource(koala,ontdir('koala.owl'),owl).

% --Other Bio-Ontologies--
user:bioresource(rex,obo_download('rex/rex.obo'),obo).
user:bioresource(ncit,obo_download('ncithesaurus/ncithesaurus.pro'),pro).
%user:bioresource(ncit_dn,obo_download('ncithesaurus/ncithesaurus.pro'),pro).
user:bioresource(pir,pir('PIRSF_ontology-02082005.dag'),dag).
user:bioresource(pir_uniprot,pir('PIRSF_UniProt_ontology-02082005.dag'),dag).
user:bioresource(evoc,ontdir('evoc.pro'),pro,ontol_db).
%user:bioresource(cco,ontdir('cco.owl'),owl).
user:bioresource(cco,url('http://www.cellcycleontology.org/ontology/cco.obo'),obo).
%user:bioresource(acgt,url('http://www.ifomis.org/acgt/1.0' ),owl). % 1.0

% -- Obol --
user:bioresource(obol_av,obol2('vocab/vocab_obo.pro'),pro,av_db).

% --Chemistry--
user:bioresource(biochem_prim,url('http://ontology.dumontierlab.com/biochemistry-primitive'),owl).
user:bioresource(biochem_complex,url('http://ontology.dumontierlab.com/biochemistry-complex'),owl).
user:bioresource(xchebi,obolr('xchebi.obo'),obo).

% --Models--
%user:bioresource(phenmodel,obo_cvs('phenotype/phenmodel.obo'),obo).
%user:bioresource(sox,song('ontology/sox.obo'),obo).



user:file_search_path(ncbigene, 'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/').
user:file_search_path(ncbigene(Family,Species),url(X),gzip(gene_info)) :-
        sformat(X,'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/~w/~w.gene_info.gz',[Family,Species]).
user:bioresource(ncbi_gene_info,url('ftp://ftp.ncbi.nih.gov/gene/DATA/gene_info.gz'),gzip(gene_info)).
user:bioresource(ncbi_generifs,url('ftp://ftp.ncbi.nih.gov/gene/GeneRIF/generifs_basic.gz'),gzip(gene_rif)).
user:bioresource(homologene,url('ftp://ftp.ncbi.nih.gov/pub/HomoloGene/current/homologene.data'),homoltbl).
user:bioresource(gene(Tax),obo_cvs(Path),obo) :- sformat(Path,'genomic-proteomic/gene/genes-~w.obo',[Tax]).

user:bioresource(iproclass,url('ftp://ftp.pir.georgetown.edu/databases/iproclass/iproclass.tb.gz'),iproclass).

user:bioresource(seq2pthr,url('ftp://ftp.pantherdb.org/genome/pthr7.0/seq2pthr.gz'),gzip(tbl(seq2pthr))).

% e.g. 10116_rattus_norvegicus
user:bioresource(gp2p(N),url(Path),tbl(gp2p)):- nonvar(N),concat_atom(['ftp://ftp.ebi.ac.uk/pub/contrib/qfo/gp2protein.',N],Path).

% e.g. 10000
user:bioresource(pthr(N),url(Path),nhx):- nonvar(N),concat_atom(['http://amigo.berkeleybop.org/amigo/panther/PTHR',N,'.tree'],Path).

% --OBDPatoData--

% old..
user:bioresource(fly_instances,datadir('phenotype/fb_inst.pro'),pro,ontol_db).
user:bioresource(pheno_fly,datadir('phenotype/fb_inst.pro'),pro,ontol_db).
user:bioresource(zfin_instances,datadir('phenotype/zfin_inst.pro'),pro,ontol_db).
user:bioresource(pheno_zfin,datadir('phenotype/zfin_inst.pro'),pro,ontol_db).
user:bioresource(organism,datadir('phenotype/organism.pro'),pro,ontol_db).

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

user:bioresource(gi_taxid_nucl,home('Data/NCBI/gi_taxid_nucl.dmp.gz'),gzip(gi_taxid)).

user:bioresource(shape,url('http://www.aimatshape.net/resources/aas-ontologies/shapecommonontology.owl'),owl).

% --Other/NLP--
user:bioresource(words,datadir('words.pro'),pro,user).


