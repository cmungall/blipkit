%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bioconf.pro - bio configuration file for prolog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%user:rebuild_all_qlf.

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

% amipath
user:file_search_path(amipath_src,'/users/cjm/cvs/bioprolog/apps/amipath/src').
user:file_search_path(amipath_conf,'/users/cjm/cvs/bioprolog/apps/amipath/conf').

user:file_search_path(obo_cvs, OBO) :-
	(getenv('OBO_HOME', OBO_HOME)
	->  true
        ;   getenv('HOME',HOME),
            concat_atom([HOME,'/','cvs/obo'],OBO_HOME)),
        concat_atom([OBO_HOME,'/','ontology'],OBO).
user:file_search_path(go, GO) :-
	(getenv('GO_HOME', GO_HOME)
	->  true
        ;   getenv('HOME',HOME),
            concat_atom([HOME,'/','cvs/go'],GO_HOME)),
        concat_atom([GO_HOME,'/','ontology'],GO).
user:file_search_path(gene_assoc, cvs('go/gene-associations')).
user:file_search_path(song, cvs(song)).
user:file_search_path(poc, cvs('Poc')).
user:file_search_path(datadir, '/users/cjm/Data').
user:file_search_path(ontdir, '/users/cjm/ontologies').
user:file_search_path(obo_download, '/users/cjm/cvs/obo/website/utils/obo-all').
user:file_search_path(obo_metadata_local, '/users/cjm/cvs/obo/website/cgi-bin').
user:file_search_path(obo_remote, 'http://purl.org/obo').
user:file_search_path(pir, ontdir(pir)).
user:file_search_path(sweet_dir, ontdir('SWEET')).
user:file_search_path(obol, '/users/cjm/obol').
user:file_search_path(obol2, '/users/cjm/obol2/results').
user:file_search_path(go_xp_dir, '/users/cjm/cvs/go/scratch/obol_results').
user:file_search_path(obol_ont, obol('ontologies')).
user:file_search_path(obol_out, obol('export')).
user:file_search_path(serval_conf, '/users/cjm/cvs/bioprolog/conf').
user:file_search_path(localncbigene, datadir('NCBI/Gene')).


%user:file_search_path(anatxp, obo_cvs('anatomy/anatomy_xp')).
user:file_search_path(anatxp, '/users/cjm/cvs/uberon').

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
        sformat(Path,'/Users/cjm/data/reactome-bp3/~w.owl',[Sp]).
user:bioresource(reactome(Sp),Path,pathway_db:pro) :-
        nonvar(Sp),
        sformat(Path,'/Users/cjm/data/reactome-bp3/~w-pathway_db.pro',[Sp]).

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
user:bioresource(omim,'/users/cjm/cvs/obo-database/build/build-omim/omim.obo',obo).
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
user:bioresource(obol_av,obol('data/vocab_obo.P'),pro,av_db).
%user:bioresource(obo,ontdir('obo.pro'),pro,ontol_db).

% --Chemistry--
user:bioresource(biochem_prim,url('http://ontology.dumontierlab.com/biochemistry-primitive'),owl).
user:bioresource(biochem_complex,url('http://ontology.dumontierlab.com/biochemistry-complex'),owl).
user:bioresource(xchebi,obol('ontologies/xchebi.obo'),obo).

% --Models--
%user:bioresource(phenmodel,obo_cvs('phenotype/phenmodel.obo'),obo).
%user:bioresource(sox,song('ontology/sox.obo'),obo).

% --OBO Ontologies--
user:bioresource(caro,obo_cvs('anatomy/caro/caro.obo'),obo).
user:bioresource(spatial,obo_cvs('anatomy/caro/spatial.obo'),obo).
user:bioresource(caro_extra,obo_cvs('anatomy/caro/caro_extra.obo'),obo).
user:bioresource(relationship,obo_cvs('OBO_REL/ro.obo'),obo).
user:bioresource(ro_proposed,obo_cvs('OBO_REL/ro_proposed_edit.obo'),obo).
user:bioresource(biological_role,'/users/cjm/cvs/go/scratch/obol_results/biological_role.obo',obo).
user:bioresource(goche,'/users/cjm/cvs/go/scratch/obol_results/goche.obo',obo).
user:bioresource(transitive_over,obo_cvs('OBO_REL/transitive_over.obo'),obo).
user:bioresource(test_transitive_over,ontdir('test_transitive_over.obo'),obo).
user:bioresource(chebi,obo_cvs('chemical/chebi.obo'),obo).
user:bioresource(so,song('ontology/so.obo'),obo).
user:bioresource(sequence,song('ontology/so.obo'),obo). % synonym for SO
user:bioresource(soxp,song('ontology/so-xp.obo'),obo).
user:bioresource(so2,song('ontology/working_draft.obo'),obo).
user:bioresource(fpo,song('ontology/fpo/feature_property.obo'),obo).
user:bioresource(genbank_fpo,song('ontology/fpo/genbank_fpo.obo'),obo).
user:bioresource(sofa,song('ontology/sofa.obo'),obo).
user:bioresource(go,go('editors/gene_ontology_write.obo'),obo).
user:bioresource(go_public,go('gene_ontology_edit.obo'),obo).
user:bioresource(biological_process,obo_download('biological_process/biological_process.obo'),obo).
user:bioresource(go_synonyms,'/users/cjm/obol2/conf/go_synonyms.obo',obo).

user:bioresource(bp_xp_uberon,'/users/cjm/cvs/go/scratch/xps/biological_process_xp_uber_anatomy-imports.obo',obo).
user:bioresource(chebi_slim,'/users/cjm/cvs/go/scratch/xps/chebi_relslim.obo',obo).
user:bioresource(chebi_with_formula,'/users/cjm/cvs/go/scratch/obol_results/chebi_with_formula.obo',obo).
user:bioresource(chego,'/users/cjm/cvs/go/scratch/obol_results/chego.obo',obo).


user:bioresource(cell,obo_cvs('anatomy/cell_type/cell.obo'),obo).
user:bioresource(cdo,obo_cvs('anatomy/cell_type/cdo.obo'),obo).
user:bioresource(cell2,obo_cvs('anatomy/cell_type/cell_cjm.obo'),obo).
user:bioresource(evoc_cell,obo_cvs('anatomy/cell_type/evoc_cell.obo'),obo).
%user:bioresource(plant,poc('ontology/OBO_format/plant_ontology.obo'),obo).
user:bioresource(plant_anatomy,poc('ontology/OBO_format/po_anatomy.obo'),obo).
user:bioresource(plant_anatomy_xp,poc('ontology/OBO_format/po_anatomy_xp.obo'),obo).
user:bioresource(plant_development,poc('ontology/OBO_format/po_temporal.obo'),obo).
%user:bioresource(plant_environment,obo_download('plant_environment/plant_environment.obo'),obo).
user:bioresource(plant_environment,obo_cvs('phenotype/environment/environment_ontology.obo'),obo).
user:bioresource(plant_trait,obo_cvs('phenotype/plant_traits/plant_trait.obo'),obo).
user:bioresource(pato,obo_cvs('phenotype/quality.obo'),obo).
user:bioresource(pato2,obo_cvs('phenotype/quality-revised.obo'),obo).
user:bioresource(miro,obo_cvs('phenotype/mosquito_insecticide_resistance.obo'),obo).
user:bioresource(unit,obo_cvs('phenotype/unit.obo'),obo).
user:bioresource(mpath,obo_cvs('phenotype/mouse_pathology/mouse_pathology.obo'),obo).
user:bioresource(plant_trait_xp,obo_cvs('phenotype/plant_traits/plant_trait_xp.obo'),obo).
user:bioresource(mammalian_phenotype,obo_cvs('phenotype/mammalian_phenotype.obo'),obo).
user:bioresource(ascomycete_phenotype,obo_cvs('phenotype/ascomycete_phenotype.obo'),obo).
user:bioresource(human_phenotype,'/Users/cjm/cvs/hpo/human-phenotype-ontology.obo',obo).
user:bioresource(human_phenotype_xp,'/Users/cjm/cvs/hpo/human-phenotype-ontology_xp.obo',obo).
user:bioresource(human_phenotype_xp_nif,obo_cvs('phenotype/human_phenotype_xp/human_phenotype_xp_nif.obo'),obo).
user:bioresource(human_phenotype_xp_uberon,obo_cvs('phenotype/human_phenotype_xp/human-phenotype-ontology_xp_uberon.obo'),obo).
user:bioresource(hp_xp_all,obo_cvs('phenotype/human_phenotype_xp/human-phenotype-ontology_xp-merged.obo'),obo).
user:bioresource(genetic_context,obo_cvs('phenotype/genetic_context.obo'),obo).
user:bioresource(rkc,obo_cvs('phenotype/phenotype_xp/rkc.obo'),obo).
user:bioresource(yeast_phenotype,obo_cvs('phenotype/yeast_phenotype.obo'),obo).
user:bioresource(evidence_code,obo_cvs('evidence_code.obo'),obo).
user:bioresource(obi,url('http://purl.obofoundry.org/obo/obi.owl'),owl).
user:bioresource(brenda,url('http://purl.obofoundry.org/obo/obo-all/brenda/brenda.obo'),obo).

user:bioresource(gro,url('http://www.ebi.ac.uk/Rebholz-srv/GRO/GRO_v0.2_20Dec2007.owl'),owl).


% XP
user:bioresource(mammalian_phenotype_xp,obo_cvs('phenotype/mammalian_phenotype_xp.obo'),obo).
user:bioresource(mammalian_phenotype_xp_nif,obo_cvs('phenotype/mammalian_phenotype_xp/mammalian_phenotype_xp_nif.obo'),obo).
user:bioresource(mammalian_phenotype_xp_uberon,obo_cvs('phenotype/mammalian_phenotype_xp/mammalian_phenotype_xp_uberon.obo'),obo).
user:bioresource(mp_xp_all,obo_cvs('phenotype/mammalian_phenotype_xp/mammalian_phenotype_xp-merged.obo'),obo).
user:bioresource(worm_phenotype_xp,obo_cvs('phenotype/worm_phenotype_xp.obo'),obo).
%user:bioresource(go_xp_chebi,obo_cvs('cross_products/go_chebi_xp/GO_to_ChEBI.obo'),obo).
%user:bioresource(ro_ucdhsc,obo_cvs('cross_products/go_chebi_xp/ro_ucdhsc.obo'),obo).

user:bioresource(go_xp_all,'/users/cjm/cvs/go/scratch/xps/go_xp_all-merged.obo',obo).
user:bioresource(cc_xp_self,'/users/cjm/cvs/go/scratch/xps/cellular_component_xp_self-imports.obo',obo).

% --Relations hack--
%   part_of etc are in their own idspace
user:bioresource(flat_relations,ontdir('flat_relations.obo'),obo).

user:bioresource(cfg,url('http://ontology.dumontierlab.com/cfg'),owl).

% --Anatomical Ontologies--
%user:bioresource(fma,ontdir('FMA/fma_obo.obo'),obo).
user:bioresource(fma_with_has_part,cvs('fma-conversion/fma_obo.obo'),obo).
user:bioresource(fma,cvs('fma-conversion/fma2.obo'),obo).
user:bioresource(fma_simple,cvs('fma-conversion/fma2-simple.obo'),obo).
user:bioresource(fma2,cvs('fma-conversion/fma2.obo'),obo). % NOW DEFAULT
user:bioresource(fma3,cvs('fma-conversion/fma3.obo'),obo).
user:bioresource(fma1,cvs('fma-conversion/fma-part-slim.obo'),obo).
%user:bioresource(fma,cvs('obo-database/conf/fma-part-slim.obo'),obo).
user:bioresource(fma_downcase,cvs('fma-conversion/fma_downcase.obo'),obo).
user:bioresource(fma_stemmed,ontdir('FMA/fma_obo_stemmed.obo'),obo).
user:bioresource(efo,'/users/cjm/tmp/efo.obo',obo).
user:bioresource(hao,'/users/cjm/tmp/hao.obo',obo).
user:bioresource(hog,url('http://bgee.unil.ch/bgee/download/HOG.obo'),obo).
user:bioresource(hog_stages,url('http://bgee.unil.ch/bgee/download/stages.obo'),obo).

user:bioresource(fungal_anatomy,obo_cvs('anatomy/gross_anatomy/microbial_gross_anatomy/fungi/fungal_anatomy.obo'),obo).
user:bioresource(tick_anatomy,[obo(tick_anatomy)]).
user:bioresource(flytest,'/Users/cjm/tmp/NB_CARO_dev.obo',obo).
user:bioresource(fly_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fly/fly_anatomy.obo'),obo).
user:bioresource(fly_anatomy_xp,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fly/fly_anatomy_XP.obo'),obo).
user:bioresource(fly_development,obo_cvs('developmental/animal_development/fly/fly_development.obo'),obo).
user:bioresource(worm_development,obo_cvs('developmental/animal_development/worm/worm_development.obo'),obo).
user:bioresource(mosquito_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/mosquito_anatomy.obo'),obo).
user:bioresource(ehdaa,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/human/human-dev-anat-abstract.obo'),obo).
user:bioresource(adult_mouse,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/mouse/adult_mouse_anatomy.obo'),obo). % synonym
user:bioresource(mouse_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/mouse/adult_mouse_anatomy.obo'),obo).
user:bioresource(emap,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/mouse/EMAP.obo'),obo).
user:bioresource(emapa,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/mouse/EMAPA.obo'),obo).
user:bioresource(zebrafish_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fish/zebrafish_anatomy.obo'),obo).
user:bioresource(zebrafish_anatomy_pre,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fish/preversion.zfish.obo'),obo).
user:bioresource(zebrafish_stages,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fish/zebrafishstages.obo'),obo).
user:bioresource(teleost_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fish/teleost_anatomy.obo'),obo).
user:bioresource(teleost_taxonomy,obo_cvs('taxonomy/teleost_taxonomy.obo'),obo).
user:bioresource(xenopus_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/frog/xenopus_anatomy.obo'),obo).
user:bioresource(bao,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fish/BAO_BTO.obo'),obo).
user:bioresource(medaka_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/fish/medaka_ontology.obo'),obo).

user:bioresource(full_galen,url('http://www.co-ode.org/galen/full-galen.owl'),owl).
%user:bioresource(galen,url('http://www.cs.man.ac.uk/~horrocks/OWL/Ontologies/galen.owl'),owl).
%user:bioresource(galen,obo_cvs('scratch/full-galen-with-names.obo'),obo).
user:bioresource(galen,anatxp('galen.obo'),obo).

user:bioresource(worm_anatomy,obo_cvs('anatomy/gross_anatomy/animal_gross_anatomy/worm/worm_anatomy/WBbt.obo'),obo).
%user:bioresource(worm_anatomy,[obo('worm_anatomy')]).
user:bioresource(worm_phenotype,url('http://tazendra.caltech.edu/~azurebrd/cgi-bin/forms/phenotype_ontology_obo.cgi'),obo).

user:bioresource(dicty_anatomy,obo_cvs('anatomy/gross_anatomy/microbial_gross_anatomy/dictyostelium/dictyostelium_anatomy.obo'),obo).
user:bioresource(fungal_anatomy,obo_cvs('anatomy/gross_anatomy/microbial_gross_anatomy/dictyostelium/dictyostelium_anatomy.obo'),obo).
user:bioresource(cyc,anatxp('opencyc2.obo'),obo).
user:bioresource(sao_obo,anatxp('sao.obo'),obo).
user:bioresource(sao,url('http://ccdb.ucsd.edu/SAO/1.2/SAO.owl'),owl).
user:bioresource(birndo,url('http://ccdb.ucsd.edu/SAO/PDPO/2.0/HumanPDPO.owl'),owl).
user:bioresource(birndpo,url('http://ccdb.ucsd.edu/SAO/DPO/1.0/DPO.owl'),owl).
user:bioresource(birnimg,url('http://ccdb.ucsd.edu/SAO/DPO/1.0/ImagePhenotype.owl'),owl).
%user:bioresource(nif,url('http://purl.org/nif/ontology/nif.owl'),owl).
user:bioresource(birnall,home('OBDAPI/conf/obd-birn/PKB_all.obo'),obo).
user:bioresource(pkb,home('cvs/OBD-PKB/PKB.obo'),obo).
user:bioresource(nif_downcase,home('cvs/OBD-PKB/PKB_dn.obo'),obo).

user:bioresource(birnlex_anatomy,url('http://birnlex.nbirn.net/ontology/BIRNLex-Anatomy.owl'),owl).
user:bioresource(birnlex_anatomy_obo,anatxp('birnlex_anatomy.obo'),obo).
user:bioresource(birnlex,url('http://purl.org/nbirn/birnlex'),owl).

user:bioresource(nif_anatomy,url('http://nif.nbirn.net/ontology/NIF-Anatomy.owl'),owl).
user:bioresource(nif_anatomy_obo,anatxp('nif_anatomy.obo'),obo).

user:bioresource(fly2fma,anatxp('fly-to-fma-homology.obo'),obo).
user:bioresource(zf2fma,anatxp('zfa-to-fma-homology.obo'),obo).
user:bioresource(mouse2fma,anatxp('ma-to-fma-homology.obo'),obo).
user:bioresource(miaa,anatxp('MIAA.obo'),obo).
user:bioresource(bila,X,obo):-
        bioresource(obo(bilateria_mrca),X,obo).
user:bioresource(amphibian_anatomy,X,obo):- bioresource(obo(amphibian_anatomy),X,obo).
user:bioresource(uberon,anatxp('uberon_edit.obo'),obo).
user:bioresource(uberonp,anatxp('uberon.obo'),obo).
user:bioresource(uberon_with_isa,anatxp('uberon_edit-with-isa.obo'),obo).
user:bioresource(uberonp_with_isa,anatxp('uberon-with-isa.obo'),obo).
user:bioresource(fma_xp,anatxp('fma_xp.obo'),obo).
user:bioresource(wpanat,anatxp('dbpedia_ontol.obo'),obo).

user:bioresource(gemina_anatomy,url('http://gemina.svn.sourceforge.net/viewvc/gemina/trunk/Gemina/ontologies/gemina_anatomy.obo'),obo).


% --Protein--
%user:bioresource(protein,[obo(protein)]).
user:bioresource(protein,obo_cvs('genomic-proteomic/pro.obo'),obo).
user:bioresource(psimod,obo_cvs('genomic-proteomic/protein/psi-mod.obo'),obo).
user:bioresource(psimi,obo_cvs('genomic-proteomic/protein/psi-mi.obo'),obo).
user:bioresource(pro2uniprot_tbl,url('ftp://ftp.pir.georgetown.edu/databases/ontology/pro_obo/PRO_mappings/uniprotmapping.txt'),tbl).
user:bioresource(pro2uniprot,'/users/cjm/cvs/go/scratch/xps/pro2uniprot.obo',obo).


%user:bioresource(opb,url('http://rest.bioontology.org/bioportal/ontologies/download/38990'),owl).


user:bioresource(ant,'/Users/cjm/tmp/Ant.obo',obo).

% --Env--
user:bioresource(envo,obo_cvs('environmental/envo.obo'),obo).
user:bioresource(envo_xp,obo_cvs('environmental/envo_xp.obo'),obo).
%user:bioresource(gaz,[obo2('GAZ')]).
user:bioresource(gaz,obo_cvs('environmental/gaz.obo'),obo).
user:bioresource(gaz2,obo_cvs('environmental/gaz2.obo'),obo).

% --GeneAssociations--
user:bioresource(uniprot_ga,gene_assoc('gene_association.goa_uniprot.lite.pro'),pro,ontol_db).
user:bioresource(tair_ga_xp,datadir('phenotype/tair_ga_xp.pro'),pro,ontol_db).
user:bioresource(fly_ga,gene_assoc('gene_association.fb.gz'),gzip(go_assoc)).

% mouse: special; includes CL,MA
user:bioresource(mgi_ga,datadir('phenotype/mgi_ga2.pro'),pro,goa_db).
user:bioresource(mgi_ga_xp,datadir('phenotype/mgi_ga2_xp.pro'),pro,goa_db).
user:bioresource(fly_ga_xp,datadir('phenotype/fb_ga_xp.pro'),pro,ontol_db).
user:bioresource(tair_ga,gene_assoc('gene_association.tair.pro',pro,ontol_db)).

% --PlantPhenAssocs--
user:bioresource(maize_pa,poc('associations/maize_ga.pro',pro,ontol_db)).
user:bioresource(gramene_pa,poc('associations/maize_ga.pro',pro,ontol_db)).
user:bioresource(worm_pa,'/users/cjm/obd/data/phenotype_annotation/WB/test_data/phenotype_association.WS186.wb',go_assoc).
user:bioresource(worm_ga,'/users/cjm/cvs/go/gene-associations/gene_association.wb.gz',gzip(go_assoc)).

user:bioresource(idmapping,'ftp://ftp.pir.georgetown.edu/databases/idmapping/idmapping.tb.gz',gzip(idmap)).

user:file_search_path(ncbigene, 'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/').
user:file_search_path(ncbigene(Family,Species),url(X),gzip(gene_info)) :-
        sformat(X,'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/~w/~w.gene_info.gz',[Family,Species]).
user:bioresource(ncbi_gene_info,url('ftp://ftp.ncbi.nih.gov/gene/DATA/gene_info.gz'),gzip(gene_info)).
user:bioresource(ncbi_generifs,url('ftp://ftp.ncbi.nih.gov/gene/GeneRIF/generifs_basic.gz'),gzip(gene_rif)).
user:bioresource(homologene,url('ftp://ftp.ncbi.nih.gov/pub/HomoloGene/current/homologene.data'),homoltbl).
user:bioresource(gene(Tax),obo_cvs(Path),obo) :- sformat(Path,'genomic-proteomic/gene/genes-~w.obo',[Tax]).
%user:bioresource(ncbi_gene_ontol,ncbigene('gene_info_ontol.pro'),pro,ontol_db).
%user:bioresource(gene_mouse,ncbigene('genes-10090.gene_info_ontol.pro'),pro,ontol_db).
%user:bioresource(gene_fly,ncbigene('genes-7227.gene_info_ontol.pro'),pro,ontol_db).
%user:bioresource(gene_fish,ncbigene('genes-7955.gene_info_ontol.pro'),pro,ontol_db).
%user:bioresource(gene_human,ncbigene('genes-9606.gene_info_ontol.pro'),pro,ontol_db).
%user:bioresource(obd_genes,[gene_mouse,gene_fly,gene_fish,gene_human]).

user:bioresource(iproclass,url('ftp://ftp.pir.georgetown.edu/databases/iproclass/iproclass.tb.gz'),iproclass).

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


% --SWEET--
% from NASA JPL
user:bioresource(sweet_biosphere,sweet_dir('biosphere.owl'),owl).
user:bioresource(sweet_data,sweet_dir('data.owl'),owl).
user:bioresource(sweet_data_center,sweet_dir('data_center.owl'),owl).
user:bioresource(sweet_earthrealm,sweet_dir('earthrealm.owl'),owl).
user:bioresource(sweet_human_activities,sweet_dir('human_activities.owl'),owl).
user:bioresource(sweet_material_thing,sweet_dir('material_thing.owl'),owl).
user:bioresource(sweet_numerics,sweet_dir('numerics.owl'),owl).
user:bioresource(sweet_phenomena,sweet_dir('phenomena.owl'),owl).
user:bioresource(sweet_process,sweet_dir('process.owl'),owl).
user:bioresource(sweet_property,sweet_dir('property.owl'),owl).
user:bioresource(sweet_sensor,sweet_dir('sensor.owl'),owl).
user:bioresource(sweet_space,sweet_dir('space.owl'),owl).
user:bioresource(sweet_substance,sweet_dir('substance.owl'),owl).
user:bioresource(sweet_sunrealm,sweet_dir('sunrealm.owl'),owl).
user:bioresource(sweet_time,sweet_dir('time.owl'),owl).
user:bioresource(sweet_units,sweet_dir('units.owl'),owl).          
user:bioresource(sweet(X),sweet_dir(File),owl):- atom_concat(X,'.owl',File).

user:bioresource(hydrology,url('http://www.ordnancesurvey.co.uk/ontology/Hydrology0.1.owl'),owl).
user:bioresource(ecological_concepts,url('http://wow.sfsu.edu/ontology/rich/EcologicalConcepts.owl'),owl).

% http://obo.cvs.sourceforge.net/viewvc/*checkout*/obo/obo/website/cgi-bin/ontologies.txt
user:bioresource(obo_meta,obo_metadata_local('ontologies.txt'),tagval).
user:bioresource(obo_meta_xp,obo_metadata_local('mappings.txt'),tagval).


% --AUTOMATIC--
user:bioresource(obo_local(N),obo_download(Path),obo):- nonvar(N),concat_atom([N,'/',N,'.obo'],Path).
user:bioresource(obo(N),url(Path),obo):- nonvar(N),concat_atom(['http://purl.org/obo/obo-all/',N,'/',N,'.obo'],Path).
user:bioresource(obo2(N),url(Path),obo):- nonvar(N),concat_atom(['http://purl.org/obo/obo/',N,'.obo'],Path).
user:bioresource(obop(N),url(Path),ontol_db:pro):- nonvar(N),concat_atom(['http://purl.org/obo/obo-all/',N,'/',N,'.pro'],Path).
user:bioresource(go_assoc_local(N),gene_assoc(Path),gzip(go_assoc)):- nonvar(N),concat_atom(['gene_association.',N,'.gz'],Path).
user:bioresource(go_assoc_submit(N),gene_assoc(Path),gzip(go_assoc)):- nonvar(N),concat_atom(['submission/gene_association.',N,'.gz'],Path).
user:bioresource(go_assoc(N),url(URL),gzip(go_assoc)):- nonvar(N),concat_atom(['http://www.geneontology.org/gene-associations/gene_association.',N,'.gz'],URL).
user:bioresource(go_assoc_version(N,V),url(URL),gzip(go_assoc)):-
        nonvar(N),
        concat_atom(['http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/~checkout~/go/gene-associations/gene_association.',N,'.gz?rev=',V,';content-type=application%2Fx-gzip.'],URL).

user:bioresource(obolr(N),Path,obo):- nonvar(N),concat_atom(['/users/cjm/cvs/go/scratch/obol_results/',N,'-obol.obo'],Path).
user:bioresource(goxp(N),Path,obo):- nonvar(N),concat_atom(['/users/cjm/cvs/go/scratch/xps/',N,'.obo'],Path).
user:bioresource(anu(N),anatxp(Path),obo):- nonvar(N),concat_atom(['anu-',N,'_obo.obo'],Path).

% post-reasoner results
user:bioresource(implied(Resource),PrologFile,'ontol_db:pro'):-
        nonvar(Resource),
        var(PrologFile),
        user:bioresource(Resource,InputFileTerm,_),
        !,
        expand_file_search_path(InputFileTerm,InputFile),
        concat_atom([InputFile,'-implied.pro'],PrologFile),
	(   exists_file(PrologFile),
	    time_file(PrologFile, PrologTime),
	    time_file(InputFile, InputTime),
            \+ user:recompile_all_biofiles,
            \+ (user:recompile_biofiles_before(Before),
                PrologTime < Before),
	    PrologTime >= InputTime
	->  true
	;   sformat(Cmd,'blip-reasoner -import_all -r ~w -to ontol_db:pro -o ~w.tmp && mv ~w.tmp ~w',[Resource,PrologFile,PrologFile,PrologFile]),
            shell(Cmd)
	->  true
	;   throw(cannot_execute(Cmd))).

user:bioresource(implied(InputFileTerm),PrologFile,'ontol_db:pro'):-
        nonvar(InputFileTerm),
        var(PrologFile),
        expand_file_search_path(InputFileTerm,InputFile),
        concat_atom([InputFile,'-implied.pro'],PrologFile),
	(   exists_file(PrologFile),
	    time_file(PrologFile, PrologTime),
	    time_file(InputFile, InputTime),
            \+ user:recompile_all_biofiles,
            \+ (user:recompile_biofiles_before(Before),
                PrologTime < Before),
	    PrologTime >= InputTime
	->  true
	;   sformat(Cmd,'blip-reasoner -import_all -i ~w -to ontol_db:pro -o ~w.tmp && mv ~w.tmp ~w',[InputFile,PrologFile,PrologFile,PrologFile]),
            shell(Cmd)
	->  true
	;   throw(cannot_execute(Cmd))).
        

user:uri_resolution('http://ccdb.ucsd.edu/SAO/1.2','http://ccdb.ucsd.edu/SAO/1.2.8/SAO.owl').
user:uri_resolution('http://ccdb.ucsd.edu/PDStageOntology/1.0/','http://ccdb.ucsd.edu/SAO/PDSO/1.0/PDSO.owl').
user:uri_resolution('http://ccdb.ucsd.edu/smart_atlas_ontology/sa.owl','http://openccdb.org/wiki/images/Sa.owl').
user:uri_resolution('http://www.owl-ontologies.com/unnamed.owl','http://ccdb.ucsd.edu/SAO/1.2.8/SAO.owl'). % TEMP

