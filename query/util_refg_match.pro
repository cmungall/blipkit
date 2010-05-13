
:- use_module(bio(curation_db)).
:- use_module(bio(io)).
:- use_module(bio(tabling)).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_compare)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_text)).
:- use_module(bio(ontol_sqlmap_go)).
:- use_module(bio(homol_sqlmap_go)).
:- use_module(bio(homol_db)).
:- use_module(bio(seqfeature_sqlmap_go)).
:- use_module(bio(seqfeature_bridge_to_class)).

initdb(Db,Rdb):-
	load_bioresource(go),
	rdb_connect(Rdb,Db),
	sqlbind(curation_db:class_infocontent/2-Db),
	table_pred(class_infocontent/2), % always table after sqlbind/2
	table_pred(ontol_db:parentT/2).
		   
%% compare_homolset(Db,Gene)
compare_homolset(Db,Gene):-
	initdb(Db,Rdb),
	rdb_findall(Rdb,
		    G-GN-C,
		    (	feature_label(G1,Gene),
			interspecies_reflexive_homologous_to(G1,G),
			feature_label(G,GN),
			curation_statement(_,G,_,C)),
		    GeneClassPairs),
	solutions(G-N,member(G-N-_,GeneClassPairs),Genes),
	forall((member(G1-G1N,Genes),member(G2-G2N,Genes),G1\=G2),
	       show_gene_pair_comparison(G1-G1N,G2-G2N,GeneClassPairs)).

show_gene_pair_comparison(G1-G1N,G2-G2N,GeneClassPairs):-
	format('~w [~w] vs ~w [~w]~n',[G1N,G1,G2N,G2]),
	solutions(C,member(G1-_-C,GeneClassPairs),Classes1),
	solutions(C,member(G2-_-C,GeneClassPairs),Classes2),
	show_class_set_difference(Classes1,Classes2).

%% compare_orgs(Db,Gene)
compare_orgs(Db,Gene):-
	initdb(Db,Rdb),
	rdb_findall(Rdb,
		    G-GN-Org-C,
		    (	feature_label(G1,Gene),
			interspecies_reflexive_homologous_to(G1,G),
			feature_label(G,GN),
			feature_organism(G,Org),
			curation_statement(_,G,_,C)),
		    Annots),
	solutions(Org,member(_-_-Org-_,Annots),Orgs),
	forall(member(Org,Orgs),
	       show_org_vs_rest_comparison(Org,Annots)).

show_org_vs_rest_comparison(Org,Annots):-
	format('Org: [~w]~n',[Org]),
	solutions(C,member(_-_-Org-C,Annots),Classes1),
	solutions(C,(member(_-_-Org2-C,Annots),Org\=Org2),Classes2),
	show_class_set_difference(Classes1,Classes2).

%% compare_dbs(Db1,Db2,Gene)
compare_dbs(Db1,Db2,Gene):-
	initdb(Db1,Rdb1),
	rdb_connect(Rdb2,Db2),
	rdb_findall(Rdb1,C,curation_statement(_,Gene,_,C),Classes1),
	rdb_query(Rdb1,GeneSymbol,feature_label(Gene,GeneSymbol)),
	rdb_query(Rdb1,GeneSpecies,feature_organism(Gene,GeneSpecies)),
	corresponding_gene_id(Rdb2,Gene,Gene2,GeneSymbol,GeneSpecies),
	rdb_findall(Rdb2,C,curation_statement(_,Gene2,_,C),Classes2),
	!,
	format('~w / ~w [~w]~n',[Gene,Gene2,GeneSymbol]),
	format('** ~w~n',[Db1]),
	maplist(show_class,Classes1),
	format('** ~w~n',[Db2]),
	maplist(show_class,Classes2),
	show_class_set_difference(Classes1,Classes2).

%% corresponding_gene_id(Rdb,Gene,Gene2,GeneSymbol,GeneSpecies) is semidet
% ID mapping. Uses symbol+species as key if ID cannot be found
corresponding_gene_id(Rdb,Gene,Gene,_,_):- % exact match by ID
	rdb_query(Rdb,Gene,feature(Gene)),
	!.
corresponding_gene_id(Rdb,_,Gene,GeneSymbol,GeneSpecies):- % find same symbol in same species
	rdb_query(Rdb,Gene,(feature_label(Gene,GeneSymbol),feature_organism(Gene,GeneSpecies))),
	!.

%% compare_dbs(Db1,Db2,Gene)
compare_dbs_for_refg(Db1,Db2,Gene):-
	initdb(Db1,Rdb1),
	rdb_connect(Rdb2,Db2),
	rdb_findall(Rdb1,C,curation_statement(_,Gene,_,C),Classes1),
	rdb_findall(Rdb2,C,curation_statement(_,Gene,_,C),Classes2),
	rdb_query(Rdb1,GeneSymbol,feature_label(Gene,GeneSymbol)),
	!,
	format('~w [~w]~n',[Gene,GeneSymbol]),
	format('** ~w~n',[Db1]),
	maplist(show_class,Classes1),
	format('** ~w~n',[Db2]),
	maplist(show_class,Classes2),
	show_class_set_difference(Classes1,Classes2).

findall_annots(Rdb,G,Annots):-
	rdb_findall(Rdb,
		    G-GN-C,
		    (	feature_label(G,GN),
			curation_statement(_,G,_,C)),
		    Annots).


% ========================================
% Display Predicates
% ========================================

max_ic(Classes,CMaxIC,MaxIC):-
	aggregate(max(IC,C),(member(C,Classes),class_infocontent(C,IC)),max(MaxIC,CMaxIC)).

	
show_class_set_difference(Classes1,Classes2):-
	class_set_difference_with_closest(Classes1,Classes2,M1,M2,[]),
	max_ic(Classes1,CMaxIC1,MaxIC1),
	max_ic(Classes2,CMaxIC2,MaxIC2),
	format('** Uniq1 : Max=~w ~w~n',[MaxIC1,CMaxIC1]),
	maplist(show_class,M1),
	format('** Uniq2 : Max=~w ~w~n',[MaxIC2,CMaxIC2]),
	maplist(show_class,M2),
	class_set_intersection(Classes1,Classes2,ClassesBoth),
	max_ic(ClassesBoth,CMaxICB,MaxICB),
	format('** Intersection : Max=~w ~w~n',[MaxICB,CMaxICB]),
	maplist(show_class,ClassesBoth),
	nl.

simple_show_class_set_difference(Classes1,Classes2):-
	class_set_difference(Classes1,Classes2,Uniq1,Uniq2,[]),
	writeln('** Uniq1'),
	maplist(show_class,Uniq1),
	writeln('** Uniq2'),
	maplist(show_class,Uniq2),
	writeln('** Intersection'),
	class_set_intersection(Classes1,Classes2,ClassesBoth),
	maplist(show_class,ClassesBoth),
	nl.

% class paired with best matches in common set
show_class(C-L):-
	!,
	show_class_basic(C),
	class_infocontent(C,IC),
	format(' [IC=~w]',[IC]),
	format(' CLOSEST: '),
	(   L=[]
	->  writeln('**NONE**')
	;   nl,
	    forall((member(X,L),class_infocontent(X,XIC),XIC > 2.5),
		   (   format('    '),
		       write_class(text,X),
		       format(' [IC=~w]',[XIC]),
		       nl))).

show_class(C):-
	show_class_basic(C),
	class_infocontent(C,IC),
	format(' [IC=~w]',[IC]),
	nl.

show_class_basic(C):-
	write_class(text,C,[ontolmap([biological_process-'P',
				      molecular_function-'F',
				      cellular_component-'C'])]).

