:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).

regrel(regulates).
regrel(negatively_regulates).
regrel(positively_regulates).


rx(A,RoX,RegRel,X):-
	subclass(A,RoX),
	differentium(RoX,RegRel,X),
	regrel(RegRel),
	debug(rx,'~w is_a ~w ~w ~w',[A,RoX,RegRel,X]).

user:suppress_fact(subclass(A,RoX)):-
	rx(A,RoX,_,_).

ontol_db:restriction(A,RegRel,X):-
	rx(A,_,RegRel,X).
