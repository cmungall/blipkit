:- module(sparql_util,
	  [
	   iterative_sparql_query/4,
	   dbpedia_query_links/4,
	   sparql_query_links/4,
           neurocommons_describe/2,
           neurocommons_query_links/4
	   ]).


:- use_module(semweb(sparql_client)).
%:- use_module(semweb(no_entailment)).

% ----------------------------------------
% SPARQL ENDPOINTS
% ----------------------------------------

%% iterative_sparql_query(+SparqlAtom,?Row,+Limit,+Opts) :-
% some sparql servers impose a LIMIT. this circumvents this by performing iterative queries.
% may not be 100% reliable as order may change..?
iterative_sparql_query(Q,Row,Limit,Opts) :-
	iterative_sparql_query(Q,Rows,0,Limit,Opts),
	member(Row,Rows),
        debug(sparql,'Row: ~w',[Row]).

iterative_sparql_query(_,[],Offset,_,Opts) :-
	member(max_offset(Max),Opts),
	Offset >= Max,
	!.
iterative_sparql_query(Q,AllRows,Offset,Limit,Opts) :-
        get_time(T1),
	sformat(Q2,'~w LIMIT ~w OFFSET ~w',[Q,Limit,Offset]),
	debug(sparql,'  query: ~w',[Q2]),
        safe_sparql_query(Q2,Rows,Opts),
	length(Rows,NumRows),
	debug(sparql,'    rows: ~w',[NumRows]),
	% sometimes something less than the full complement is returned.
	% as a heuristic we check if < half of the expected rows are
	% returned, and, if so, we assume this is the last batch
	HalfOfOffset is Offset/2,
	(   NumRows < HalfOfOffset
	->  AllRows=Rows
	;   T2 is T1+10, % TODO - use robots.txt; make this safe for now
            wait_until(T2),
            NextOffset is Offset+Limit,
	    iterative_sparql_query(Q,NextRows,NextOffset,Limit,Opts),
	    append(Rows,NextRows,AllRows)).

sparql_query_results(Q,Rows,Opts) :-
        findall(Row,sparql_query(Q,Row,Opts),Rows).

safe_sparql_query(Q,Rows,Opts) :-
        catch(sparql_query_results(Q,Rows,Opts),
              E,
              (   print_message(error,E),
                  fail)),
        !.
safe_sparql_query(Q,Rows,Opts) :-
        sleep(10),
        print_message(error,attempt(2)),
        catch(sparql_query_results(Q,Rows,Opts),
              _,
              fail),
        !.
safe_sparql_query(Q,Rows,Opts) :-
        sleep(60),
        print_message(error,attempt(3)),
        catch(sparql_query_results(Q,Rows,Opts),
              _,
              fail),
        !.
safe_sparql_query(_,[],Opts) :-
        member(fallible(true),Opts),
        !.
safe_sparql_query(Q,_,_) :-
        throw(error(sparql(Q))),
        !.

wait_until(T) :-
        repeat,
        get_time(T1),
        debug(sparql,'waiting until ~w >= ~w',[T1,T]),
        (   T1<T
        ->  sleep(1),
            fail
        ;   !).
        
% ----------------------------------------
% DESCRIBE
% ----------------------------------------

sparql_describe(URI,R,Opts) :-
        concat_atom(['DEFINE sql:describe-mode "CBD"\nDESCRIBE',' ','<',URI,'>'],Q),
        sparql_query(Q,R,Opts).

% ----------------------------------------
% NEUROCOMMONS SPECIFIC UTILS
% ----------------------------------------

%% neurocommons_describe(+URI,?Triple) is nondet
% example: neurocommons_describe('http://purl.obolibrary.org/obo/IAO_0000301',X)
neurocommons_describe(URI,R) :-
        sparql_set_server([host('sparql.obodev.neurocommons.org'),port(80),path('/sparql/')]),
        % virtuoso returns n3 by default - todo add support in sparql_client
        sparql_describe(URI,R,[search([format='application/rdf+xml'])]).

%% neurocommons_query_links(+A,?Row,+Limit,+Opts) is nondet
neurocommons_query_links(A,Row,Limit,Opts) :-
	(   A=obo(URL)
	->  true
	;   atom_concat('__',URL,A) % e.g. __nodeID://1000164413
        ->  true
        ;   URL=A),
	debug(neurocommons,'focus: ~w',[A]),
	sparql_set_server([host('sparql.obodev.neurocommons.org'),port(80),path('/sparql/')]),
	sparql_query_links(URL,Row,Limit,Opts).


% ----------------------------------------
% DBPEDIA SPECIFIC UTILS
% ----------------------------------------

%% dbpedia_query_links(+A,?Row,+Limit,+Opts) is nondet
dbpedia_query_links(A,Row,Limit,Opts) :-
	(   atom_concat('http://',_,A)
	->  URL=A
	;   atom_concat('http://dbpedia.org/resource/',A,URL)),
	debug(dbpedia,'focus: ~w',[A]),
	sparql_set_server([host('dbpedia.org'),port(80),path('/sparql/')]),
	sparql_query_links(URL,Row,Limit,[sameAs('http://dbpedia.org/property/redirect')|Opts]).

% ----------------------------------------
% QUERY+REDIRECTS
% ----------------------------------------
% can use owl:sameAs, dbpedia:redirect etc to perform extra queries
% - may be better to do this directly in SPARQL..?
% - ideally entailment on the server would cover this, but its not how most work
sparql_query_links(A,row(A2,P,O),Limit,Opts) :-
        member(sameAs(EqP),Opts),
        % redirect source
	triple_sparql(A,EqP,_,Q),
	iterative_sparql_query(Q,row(A2),Limit,Opts),
        direct_sparql_query_links(A2,row(A2,P,O),Limit,Opts).
sparql_query_links(A,row(A,P,O2),Limit,Opts) :-
        member(sameAs(EqP),Opts),
        % redirect results
        direct_sparql_query_links(A,row(A,P,O),Limit,Opts),
        atom(O), % don't follow labels etc
	triple_sparql(O,EqP,_,Q),
	iterative_sparql_query(Q,row(O2),Limit,Opts).
sparql_query_links(A,row(S,P,O),Limit,Opts) :-
        direct_sparql_query_links(A,row(S,P,O),Limit,Opts).

% ----------------------------------------
% DIRECT QUERIES
% ----------------------------------------

% outward
direct_sparql_query_links(A,row(A,P,O),Limit,Opts) :-
	triple_sparql(A,_,_,Q),
	iterative_sparql_query(Q,row(P,O),Limit,Opts).
% inward
direct_sparql_query_links(A,row(S,P,A),Limit,Opts) :-
	triple_sparql(_,_,A,Q),
	iterative_sparql_query(Q,row(S,P),Limit,Opts).
        

% ----------------------------------------
% PROLOG TO SPARQL
% ----------------------------------------

% simplistic pl2sparql
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

            
