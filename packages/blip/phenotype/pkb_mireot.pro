:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(tabling)).
:- use_module(bio(pkb_db)).

mireot_hpo(ID) :-
	(   differentium(X,_,Y);genus(X,Y)),
	id_idspace(X,'HP'),
	\+ id_idspace(Y,'HP'),
	refs(Y,ID).
mireot_mpo(ID) :-
	(   differentium(X,_,Y);genus(X,Y)),
	id_idspace(X,'MP'),
	\+ id_idspace(Y,'MP'),
	refs(Y,ID).

mireot(ID) :-
	% pre-composed
	setof(P,O^(organism_phenotype(O,P),class(P)),Ps),
	member(P,Ps),
	debug(mireot,' prec: ~w',[P]),
	bf_parentRT(P,ID).

mireot(ID) :-
	setof(X,P^PQ^R^(phenotype_quad(P,PQ),
			phenotype_differentium(PQ,X,R)),
	      Xs),
	member(X,Xs),
	debug(mireot,' post: ~w',[P]),
	bf_parentRT(X,ID).

refs(X,Y) :-
	debug(mireot,'mir: ~w',[X]),
	%id_idspace(X,'UBERON'),
	%!,
	bf_parentRT(X,Y).
	%refs_safe(X,Y).
%refs(X,Y) :-
%	\+ id_idspace(X,'UBERON'),
%	parentRT(X,Y).

%:- table_pred(user:refs/2).

% DEPRECATED:
refs_safe(X,Y) :-
	subclassRT(X,Y).
%refs_safe(X,Y) :-
%	parent_overT(part_of,X,Y).
refs_safe(X,Y) :-
	subclassRT(X,Z),restriction(Z,_,Z2),subclassRT(Z2,Y).
refs_safe(X,Y) :-
	subclassRT(X,Z),genus(Z,Z2),subclassRT(Z2,Y).
refs_safe(X,Y) :-
	subclassRT(X,Z),differentium(Z,_,Z2),subclassRT(Z2,Y).



