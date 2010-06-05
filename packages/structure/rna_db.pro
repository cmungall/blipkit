:- module(rna_db,
          [
	   adenine/1,
	   guanine/1,
	   uracil/1,
	   cytosine/1,
	   purine/1,
	   pyrimidine/1,
	   base/1,
	   pairs_with_cww/2,
	   five_prime_to/2,

	   gnra_tetraloop/2
           ]).

:- use_module(bio(dbmeta)).

%% adenine(?X:atom)
% @param X base
:- extensional(adenine/1).
:- extensional(guanine/1).
:- extensional(uracil/1).
:- extensional(cytosine/1).

base(X):- purine(X).
base(X):- pyrimidine(X).

purine(X):- adenine(X).
purine(X):- uracil(X).

pyrimidine(X):- guanine(X).
pyrimidine(X):- cytosine(X).

%% pairs_with_cww(?A,?B)
:- extensional(pairs_with_cww/2).

%% five_prime_to(?A,?B)
:- extensional(five_prime_to/2).

gnra_tetraloop(N1,N6) :-
  five_prime_to(N1,N2),
  five_prime_to(N2,N3),
  five_prime_to(N3,N4),
  five_prime_to(N5,N5),
  five_prime_to(N5,N6),
  pairs_with_cww(N1,N6),
  guanine(N2),
  purine(N4),
  adenine(N5).

