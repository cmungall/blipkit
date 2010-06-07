
:- use_module(bio(simmatrix_multiset)).
:- use_module(bio(owl2_to_sim)).
:- use_module(bio(pkb_to_sim)).

% R6/2 mouse 4 vs  S129D Alpha-Synuclein Drosophila (age: 30 days) 481
tpair('http://ccdb.ucsd.edu/PKB/1.0/PKB.owl#nlx_organ_20090205_109','http://ccdb.ucsd.edu/PKB/1.0/PKB.owl#nlx_organ_090804_76').

% fly < Retina photoreceptor cell
t_feature_nr_attx_1('http://ccdb.ucsd.edu/PKB/1.0/PKB.owl#nlx_organ_090804_76',['http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#sao1233810115','http://purl.org/obo/owl/PATO#PATO_0002001']).

% R6/2 mouse 4 mutated 	Retina amacrine cell
t_feature_nr_attx_2('http://ccdb.ucsd.edu/PKB/1.0/PKB.owl#nlx_organ_20090205_109',['http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#nifext_36','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#nlx_mol_20090302','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Quality.owl#nlx_qual_20090302']).

t :-
	tpair(F1,F2),
	feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC),
	format('~w LCS=~w [ ~w vs ~w]~n',[IC,LCS,S1,S2]).

%% expect 5.67243 LCS=[http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#sao1233810115,http://purl.org/obo/owl/PATO#PATO_0002001] 
% NIF_Cell:sao1233810115 ! Retina photoreceptor cell 
t2 :-
	t_feature_nr_attx_1(F1,S1),
	t_feature_nr_attx_1(F2,S2),
	feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC),
	format('~w LCS=~w ~n',[IC,LCS]).

t3 :-
	ensure_loaded(bio(simmatrix)),
	forall(simmatrix:generate_term_indexes_hook(Hook),
	       debug(sim,'generated: ~w',[Hook])),	
	tpair(F1,F2),
	feature_pair_ci_cu_simj(F1,F2,CI,X,S),
	format('~w ~w ~w~n',[CI,X,S]).
	
	

