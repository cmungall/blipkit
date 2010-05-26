:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(index_util)).
:- use_module(bio(tabling)).

seq_family(S,F) :-
	seq2pthr(A,F),
	concat_atom(L,'|',A),
	member(S,L).


setup :-
      load_bioresource(protein),
      load_bioresource(pro2uniprot),
      load_bioresource(seq2pthr),
      table_pred(ontol_db:subclassT/2),
      materialize_index(user:seq_family(1,1)).

pro_unipair(P,U1,U2,F1,F2,Eq) :-
	subclass(U1,P),
	id_idspace(U1,'UniProtKB'),
	subclass(U2,P),
	U1 @< U2,
	id_idspace(U2,'UniProtKB'),
	seq_family(U1,F1),
	seq_family(U2,F2),
	(   F1=F2
	->  Eq=true
	;   Eq=false).

fam_unipair(F,U1,U2,P) :-
	seq_family(U1,F),
	seq_family(U2,F),
	U1 @< U2,
	class_pair_subclass_lca(U1,U2,P).

	

