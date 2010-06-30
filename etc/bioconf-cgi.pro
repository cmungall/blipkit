%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bioconf.pro - bio configuration file for prolog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:trusted_user(false).
user:file_search_path(home,'/tmp').
user:file_search_path(data_cache,'/tmp/.blip/data_cache').
user:file_search_path(path_to_wget,'/opt/local/bin').
user:file_search_path(path_to_dot,'/opt/local/bin').
user:file_search_path(obo_metadata_local, 'http://obo.cvs.sourceforge.net/viewvc/*checkout*/obo/obo/website/cgi-bin/').

%% This is required to run anything using Thea2 (e.g. owl2 conversion in ontol_rest)
% TODO: use git submodules intsead!!
user:file_search_path(library,'/users/cjm/cvs/').


user:max_cached_file_age_seconds(MaxAge):- MaxAge is 3600 * 100. % ~4 days


% allow alternate suffixes on prolog fact files (default is .pl)
user:prolog_file_type(pro,prolog). % -- blip style
%user:prolog_file_type('P',prolog). % -- XSB style

%user:file_search_path(blip_apps,home('cvs/bioprolog/apps')).

user:file_search_path(amigo,blip_apps(amigo)).
user:file_search_path(amigo_src,amigo(src)).

%user:file_search_path(obo,home('cvs/obo/ontology')).
%user:file_search_path(data,home('Data')).

:- discontiguous user:bioresource/2, user:bioresource/3, user:bioresource/4.

% -- SQL Databases --
% these must be downloaded installed, and an ODBC conf set up
user:bioresource(rdb(go),odbc_connect(go,[user(cjm),alias(go),open(once)]),go).
user:bioresource(rdb(hs),odbc_connect(hs,[user(cjm),alias(hs),open(once)]),go).
user:bioresource(rdb(enscore_hs),odbc_connect(homo_sapiens_cdna_30_35c_mysql40_compatible,[user(cjm),open(once)]),enscore).
user:bioresource(rdb(wn),odbc_connect(wn,[user(cjm),alias(wn),open(once)]),wn).

% --Base Ontologies--
%   these must be loaded when reasoning over OWL ontologies
%user:file_search_path(triple20_base,home('cvs/Triple20/Ontologies/Base')).
user:bioresource(owl,triple20_base('rdfs.rdfs'),rdfs).
user:bioresource(rdfs,triple20_base('owl.owl'),owl).

% --OBO Metadata--
user:bioresource(obo_meta,obo_metadata_local('ontologies.txt'),tagval).
user:bioresource(obo_meta_xp,obo_metadata_local('mappings.txt'),tagval).
user:bioresource(xrf,'http://www.berkeleybop.org/ontologies/obo-all/go_xrf_metadata/go_xrf_metadata.obo',obo).


% -- GO and OBO ontologies --
user:bioresource(go,url('http://www.geneontology.org/ontology/gene_ontology.obo'),obo).
user:bioresource(obo(N),url(Path),obo):- nonvar(N),concat_atom(['http://purl.org/obo/obo-all/',N,'/',N,'.obo'],Path).
user:bioresource(obop(N),url(Path),ontol_db:pro):- nonvar(N),concat_atom(['http://purl.org/obo/obo-all/',N,'/',N,'.pro'],Path).


