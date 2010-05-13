%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bioconf.pro - bio configuration file for prolog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% semi-automatic setup via Makefile
%  todo: make a data bundle for bio

user:file_search_path(amigo_src,'/users/cjm/cvs/bioprolog/apps/amigo/src').
user:file_search_path(amigo_conf,'/users/cjm/cvs/bioprolog/apps/amigo/conf').

user:file_search_path(amipath_src,'/users/cjm/cvs/bioprolog/apps/amipath/src').
user:file_search_path(amipath_conf,'/users/cjm/cvs/bioprolog/apps/amipath/conf').

user:file_search_path(obo, OBO) :-
        concat_atom(['/Users/cjm/cvs/obo','/','ontology'],OBO).
user:file_search_path(go, GO) :-
	(getenv('GO_HOME', GO_HOME)
	->  true
        ;   GO_HOME='@GO@'),
        concat_atom([GO_HOME,'/','ontology'],GO).
user:file_search_path(gene_assoc, GO_ASSOC):-
	(getenv('GO_HOME', GO_HOME)
	->  true
        ;   GO_HOME='@GO@'),
        concat_atom([GO_HOME,'/','gene-associations'],GO).
user:file_search_path(song, cvs(song)).
user:file_search_path(poc, cvs('Poc')).

user:file_search_path(mydata, '/users/cjm/Data').
user:file_search_path(myont, '/users/cjm/Data/ontologies').
user:file_search_path(obol, '/users/cjm/obol/ontologies').
user:file_search_path(obol_out, '/users/cjm/obol/export').
user:file_search_path(serval_conf, '/users/cjm/cvs/bioprolog/conf').

:- discontiguous bioresource/2, bioresource/3, bioresource/4.

user:bioresource(rdb(go),[odbc_connect(go,[])]).

% --Genomics--
user:bioresource(testcg,'/users/cjm/chaos-xml/sample-data/Rab1.chaos-xml',chaos).
user:bioresource(testsox,myont('testsox.owl'),owl).

% --Pathways--
user:bioresource(reactome,'/users/cjm/Data/pathways/reactome.sbml-xml',sbml).

% --Comparative--
user:bioresource(inparanoid,'/users/cjm/Data/inparanoid/all.tbl',inparanoid_tbl).

% --Base Ontologies--
user:bioresource(owl,'/users/cjm/cvs/Triple20/Ontologies/Base/rdfs.rdfs',rdfs).
user:bioresource(rdfs,'/users/cjm/cvs/Triple20/Ontologies/Base/owl.owl',owl).

% --Non-OBO Ontologies--
user:bioresource(wine,myont('wine.owl'),owl).
user:bioresource(bible,myont('NTNames.owl'),owl).
user:bioresource(food,myont('food.owl'),owl).
user:bioresource(koala,myont('koala.owl'),owl).

% --Other Bio-Ontologies--
user:bioresource(nci,myont('NCIThesaurus.owl'),owl).

% --OBO Ontologies--
user:bioresource(obo,obol('ont_obo.P'),pro,ontol).
user:bioresource(ubo,obol('ubo.obo'),obo).
user:bioresource(spatial,obol('spatial.obo'),obo).
%user:bioresource(so,song('ontology/so.pro')).
user:bioresource(so,song('ontology/so.obo'),obo).
user:bioresource(sox,song('ontology/sox.obo'),obo).
user:bioresource(sofa,song('ontology/sofa.obo'),obo).
user:bioresource(go,go('gene_ontology.obo'),obo).
user:bioresource(dl_go,go('gene_ontology_dl.pro'),pro,ontol).

user:bioresource(cell,'/users/cjm/obol/data/ont_cell.P',pro,ontol).
user:bioresource(dl_bp,obol_out('biological_process.pro'),pro,ontol).
user:bioresource(dl_cell,obol_out('cell.pro'),pro,ontol).
user:bioresource(plant,poc('anatomy/po_anatomy.obo'),obo).
user:bioresource(dl_plant,obol_out('plant_anatomy.pro'),pro,ontol).
user:bioresource(dl_fly,obol_out('fly_anatomy.pro'),pro,ontol).
user:bioresource(pato,obol('attribute_and_value.obo'),obo).

% --Anatomical Ontologies--
user:bioresource(fly_anatomy,obo('anatomy/gross_anatomy/animal_gross_anatomy/fly/fly_anatomy.obo'),obo).
user:bioresource(mouse_anatomy,obo('anatomy/gross_anatomy/animal_gross_anatomy/mouse/MA.obo'),obo).
user:bioresource('EMAP',obo('anatomy/gross_anatomy/animal_gross_anatomy/mouse/EMAP.obo'),obo).
user:bioresource(zebrafish_anatomy,obo('anatomy/gross_anatomy/animal_gross_anatomy/zebrafish/zebrafish_pure_anatomy.obo'),obo).

% --GeneAssociations--
user:bioresource(uniprot_ga,gene_assoc('gene_association.goa_uniprot.lite.pro'),pro,ontol).
user:bioresource(tair_ga_xp,'/users/cjm/Data/phenotype/tair_ga_xp.pro',pro,ontol).
user:bioresource(fly_ga,gene_assoc('gene_association.fb.gz'),gzip(go_assoc)).

% mouse: special; includes CL,MA
user:bioresource(mgi_ga,'/users/cjm/Data/phenotype/mgi_ga2.pro',pro,ontol).
user:bioresource(mgi_ga_xp,'/users/cjm/Data/phenotype/mgi_ga2_xp.pro',pro,ontol).
user:bioresource(fly_ga_xp,'/users/cjm/Data/phenotype/fb_ga_xp.pro',pro,ontol).
user:bioresource(tair_ga,gene_assoc('gene_association.tair.pro',pro,ontol)).

% --PlantPhenAssocs--
user:bioresource(maize_pa,poc('associations/maize_ga.pro',pro,ontol)).
user:bioresource(gramene_pa,poc('associations/maize_ga.pro',pro,ontol)).

% --OBDPatoData--
user:bioresource(fly_instances,'/users/cjm/Data/phenotype/fb_inst.pro',pro,ontol).
user:bioresource(zfin_instances,'/users/cjm/Data/phenotype/zfin_inst.pro',pro,ontol).
user:bioresource(organism,'/users/cjm/Data/phenotype/organism.pro',pro,ontol).

% --NCBI--
user:bioresource(taxnames,home('Data/ncbitax/names.dmp'),ncbitaxname).
user:bioresource(taxnodes,home('Data/ncbitax/nodes.dmp'),ncbitaxnode).
