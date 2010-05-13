:- module(sparql_util,
	  [
	   iterative_sparql_query/4,
	   dbpedia_query_links/4,
	   sparql_query_links/4
	   ]).


:- use_module(serql(sparql_client)).
:- use_module(serql(no_entailment)).

iterative_sparql_query(Q,Row,Limit,Opts) :-
	iterative_sparql_query(Q,Rows,0,Limit,Opts),
	member(Row,Rows).

iterative_sparql_query(_,[],Offset,_,Opts) :-
	member(max_offset(Max),Opts),
	Offset >= Max,
	!.
iterative_sparql_query(Q,AllRows,Offset,Limit,Opts) :-
	sformat(Q2,'~w LIMIT ~w OFFSET ~w',[Q,Limit,Offset]),
	debug(sparql,'  query: ~w',[Q2]),
	findall(Row,sparql_query(Q2,Row,Opts),Rows),
	length(Rows,NumRows),
	debug(sparql,'    rows: ~w',[NumRows]),
	% sometimes something less than the full complement is returned.
	% as a heuristic we check if < half of the expected rows are
	% returned, and, if so, we assume this is the last batch
	HalfOfOffset is Offset/2,
	(   NumRows < HalfOfOffset
	->  AllRows=Rows
	;   NextOffset is Offset+Limit,
	    iterative_sparql_query(Q,NextRows,NextOffset,Limit,Opts),
	    append(Rows,NextRows,AllRows)).

dbpedia_query_links(A,Row,Limit,Opts) :-
	(   atom_concat('http://',_,A)
	->  URL=A
	;   atom_concat('http://dbpedia.org/resource/',A,URL)),
	debug(dbpedia,'focus: ~w',[A]),
	sparql_set_server([host('dbpedia.org'),port(80),path('/sparql/')]),
	sparql_query_links(URL,Row,Limit,Opts).

sparql_query_links(A,row(A,P,O),Limit,Opts) :-
	triple_sparql(A,_,_,Q),
	iterative_sparql_query(Q,row(P,O),Limit,Opts).
sparql_query_links(A,row(S,P,A),Limit,Opts) :-
	triple_sparql(_,_,A,Q),
	iterative_sparql_query(Q,row(S,P),Limit,Opts).


triple_sparql(S,P,O,Q) :-
	sparql_term(S,Sx,s),
	sparql_term(P,Px,p),
	sparql_term(O,Ox,o),
	sformat(Q,'SELECT * WHERE { ~w ~w ~w }',[Sx,Px,Ox]).

sparql_term(A,T,X) :-
	var(A),
	!,
	atom_concat('?',X,T).
sparql_term(A,T,_) :-
	sformat(T,'<~w>',[A]).

traverse_over(A,Prop,Row,Limit,Opts) :-
	findall(Row,sparql_query_links(A,Row,Limit,Opts),Rows),
	member(Row,Rows),
	(   Prop=inv(IProp)
	->  member(row(X,IProp,A),Rows),
	    traverse_over(X,Prop,Row,Limit,Opts)
	;   member(row(A,Prop,X),Rows),
	    traverse_over(X,Prop,Row,Limit,Opts)).



            