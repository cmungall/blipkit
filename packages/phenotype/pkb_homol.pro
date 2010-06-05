:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).

homologous_to(A,B) :- curation_statement(_,A,'OBO_REL:homologous_to',B).

homologous_to(A1,T1,A2,T2) :-
	curation_statement(_,X1,'OBO_REL:homologous_to',X2),
	xp_anat_tax(X1,A1,T1),
	xp_anat_tax(X2,A2,T2).

homologous_to(A1,T1,A2,T2,A3,T3,A3Xs) :-
	homologous_to(A1,T1,A2,T2),
	class_pair_subclass_lca(A1,A2,A3),
	solutions(A3X,class_pair_parent_lca(A1,A2,A3X),A3Xs),
	class_pair_subclass_lca(T1,T2,T3).

xp_anat_tax(X,A,T) :-
	genus(X,A),
	differentium(X,'PHENOSCAPE:in_taxon',T).

class_pair_parent_lca(X,Y,LCA):-
        bf_parentRT(X,LCA),
        bf_parentRT(Y,LCA),
        \+ (( bf_parentRT(X,CA),
	      CA\=LCA,
              bf_parentRT(Y,CA),
              bf_parentRT(CA,LCA))).

	
