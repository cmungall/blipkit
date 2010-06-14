% ----------------------------------------
% sb_biosources_local
% ----------------------------------------
% this configuration should be used if you wish to use
% locally download pathway data

:- multifile bioresource/2,bioresource/3,bioresource/4.

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


% --Pathways--
user:bioresource(rhea,datadir('rhea/rhea-pathway_db.pro'),pathway_db:pro).
user:bioresource(biopax1,url('http://www.biopax.org/release/biopax-level1.owl'),owl).
user:bioresource(biopax2,url('http://www.biopax.org/release/biopax-level2.owl'),owl).
user:bioresource(biopax3,url('http://www.biopax.org/release/biopax-level3.owl'),owl).

% blip -r reactome_biopax/Drosophila\ melanogaster
user:bioresource(reactome_biopax(Sp),Path,owl) :-
        nonvar(Sp),
        sformat(Path,'/Users/cjm/cvs/biowarehouse/reactome/~w.owl',[Sp]).
user:bioresource(reactome(Sp),Path,pathway_db:pro) :-
        nonvar(Sp),
        sformat(Path,'/Users/cjm/cvs/biowarehouse/reactome/~w-pathway_db.pro',[Sp]).

user:bioresource(pathway_commons(Sp),url(Path),gzip(owl)) :-
        nonvar(Sp),
        sformat(Path,'http://www.pathwaycommons.org/pc-snapshot/biopax/by_species/~w.owl.zip',[Sp]).
