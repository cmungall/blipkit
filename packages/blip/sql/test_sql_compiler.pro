:- module(test_sql_compiler,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(bioprolog_util)).

% --- benchmarking programs -------------------------------------
%
% taken from R. O'Keefe: The Craft of Prolog, MIT Press 1990
%
% Sepia Prolog version

translate(Proj,Goal,Sql):- plterm_to_sqlterm(Proj,Goal,Sql).

% --- printqueries(Code) -----------------------------------------------------

printqueries([Query]):-
   nl,
   print_query(Query),
   write(';'),
   nl,
   nl.

printqueries([Query|Queries]):-
   not (Queries = []),
   nl,
   print_query(Query),
   nl,
   write('UNION'),
   nl,
   printqueries(Queries).



% --- print_query(QueryCode) --------------------------------------------------

print_query(query([agg_query(Function,Select,From,Where,Group)],_,_)):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   print_query(agg_query(Function,Select,From,Where,Group)).

print_query(query(Select,From,Where)):-
   print_clause('SELECT',Select,','),
   nl,
   print_clause('FROM',From,','),
   nl,
   print_clause('WHERE',Where,'AND'),
   nl.

print_query(agg_query(Function,Select,From,Where,Group)):-
   print_clause('SELECT',Function,Select,','),
   nl,
   print_clause('FROM',From,','),
   nl,
   print_clause('WHERE',Where,'AND'),
   nl,
   print_clause('GROUP BY',Group,',').

print_query(negated_existential_subquery(Select,From,Where)):-
   write('NOT EXISTS'),
   nl,
   write('('),
   print_clause('SELECT',Select,','),
   nl,
   print_clause('FROM',From,','),
   nl,
   print_clause('WHERE',Where,'AND'),
   nl,
   write(')').




% --- print_clause(Keyword,ClauseCode,Separator) ---------------------------------
%
% with 
% Keyword    one of SELECT, FROM, WHERE, or GROUP BY, 
% ClauseCode the code corresponding to the appropriate clause of an SQL query, and 
% Separator  indicating the character(s) through which the items of a clause
%            are separated from each other (',' or 'AND').
% 
% ------------------------------------------------------------------------

print_clause(_Keyword,[],_).

print_clause(Keyword,[Column|RestColumns],Separator):-
   write(Keyword),
   write(' '),
   print_clause([Column|RestColumns],Separator).

print_clause(Keyword,Function,[Column],Separator):-
   write(Keyword),
   write(' '),
   write(Function),
   write('('),
   print_clause([Column],Separator),
   write(')').





% --- print_clause(ClauseCode,Separator) ----------------------------------------

print_clause([Item],_):-
   print_column(Item).

print_clause([Item,NextItem|RestItems],Separator):-
   print_column(Item),
   write(' '),
   write(Separator),
   write(' '),
   print_clause([NextItem|RestItems],Separator).




% --- print_column(ColumnCode) ---------------------------

print_column('*'):-
   write('*').

print_column(att(RangeVar,Attribute)):-
   write(RangeVar),
   write('.'),
   write(Attribute).

print_column(rel(Relation,RangeVar)):-
   write(Relation),
   write(' '),
   write(RangeVar).

print_column('$const$'(String)):-
   get_type('$const$'(String),string),
   write('"'),
   write(String),
   write('"').

print_column('$const$'(Number)):-
   get_type('$const$'(Number),NumType),
   type_compatible(NumType,number),
   write(Number).

print_column(comp(LeftArg,Operator,RightArg)):-
   print_column(LeftArg),
   write(' '),
   write(Operator),
   write(' '),
   print_column(RightArg).

print_column(LeftExpr * RightExpr):-
   print_column(LeftExpr),
   write('*'),
   print_column(RightExpr).

print_column(LeftExpr / RightExpr):-
   print_column(LeftExpr),
   write('/'),
   print_column(RightExpr).

print_column(LeftExpr + RightExpr):-
   print_column(LeftExpr),
   write('+'),
   print_column(RightExpr).

print_column(LeftExpr - RightExpr):-
   print_column(LeftExpr),
   write('-'),
   print_column(RightExpr).

print_column(agg_query(Function,Select,From,Where,Group)):-
   nl,
   write('('),
   print_query(agg_query(Function,Select,From,Where,Group)),
   write(')').

print_column(negated_existential_subquery(Select,From,Where)):-
   print_query(negated_existential_subquery(Select,From,Where)).



% --- queries_atom(Queries,QueryAtom) ------------------------ 
%
% queries_atom(Queries,QueryAtom) returns in its second argument
% the SQL query as a Prolog atom. For efficiency reasons, a list
% of ASCII codes is ceated as a difference list, and it is then 
% transformed to an atom by name/2
% ------------------------------------------------------ 


queries_atom(Queries,QueryAtom):-
   queries_atom(Queries,QueryList,[]),
   name(QueryAtom,QueryList).



queries_atom([Query],QueryList,Diff):-
   query_atom(Query,QueryList,Diff).

queries_atom([Query|Queries],QueryList,Diff):-
   Queries \= [],
   query_atom(Query,QueryList,X1),
   column_atom('UNION',X1,X2),
   queries_atom(Queries,X2,Diff).


cpu_time(Time):-
   Time is cputime.
cputime(Time):-
   Time is cputime.


cpu_time(Goal,Duration):-
   !,
   cputime(T1),
   (call(Goal) -> true; true),
   cputime(T2),
   Duration is T2 - T1.

cpu_time(_N,Goal,Duration):-
   !,
   cpu_time(Goal,Duration).
cpu_time(N,Goal,Duration):-
   !,
   cpu_time((repeat_n(N),(Goal -> fail);true),D1),
   cpu_time((repeat_n(N),(true -> fail);true),D2),
   Duration is D1 - D2.

benchmark(N,1,D):-
   cpu_time(N,
     (translate(flight(No,Dep,Dest,Type),flight(No,Dep,Dest,Type),Code),
	  printqueries(Code)),
   D).

benchmark(N,2,D):-
   cpu_time(N,
     (translate(capacity(No,Dep,Dest,Type,Seats),
	    (flight(No,Dep,Dest,Type),
		 plane(Type,Seats),
		 Type='b-737'),Code),
	   printqueries(Code)),
   D).

benchmark(N,3,D):-
   cpu_time(N,
      (translate(no_planes(No,Dep,Dest,Type),
	      (flight(No,Dep,Dest,Type),
                 not(plane(Type,_Seats))),Code),
	   printqueries(Code)),
	D).

benchmark(N,4,D):-
   cpu_time(N,(translate(X,X is count(S,P^plane(P,S)),Code),printqueries(Code)),D).

benchmark(N,5,D):-
   cpu_time(N,
      (translate(big_planes(munich,Dest,Type,Seats),
	      FNo^(flight(FNo,munich,Dest,Type),
		       plane(Type,Seats),
			   Seats > avg(S, T^plane(T,S))),Code),
	  printqueries(Code)),
   D).

benchmark(N,6,D):-
   cpu_time(N,(
     translate(big_planes(munich,Dest,Type,Seats),
	     FNo^(flight(FNo,munich,Dest,Type),
		      plane(Type,Seats),
			  Seats > avg(S, T^plane(T,S))),Code),
		 printqueries(Code)),
   D).

benchmark(N,7,D):-
   cpu_time(N,(
     translate(big_planes(munich,Dest,Type,Seats),
	     FNo^(flight(FNo,munich,Dest,Type),
		      plane(Type,Seats),
			  Seats > avg(S, T^plane(T,S))),Code),
               %queries_atom(Code,SQLQueryAtom),
	%	 writeq(query_atom(SQLQueryAtom)),
               printqueries(Code),
		 nl),
   D).
   
   
sql_compiler:relation(flight,4,'FLIGHT').
sql_compiler:relation(plane,2,'PLANE').


% --- sql_compiler:attribute(PrologArgumentPosition,SQLTableName,SQLSql_compiler:AttributeName) --

sql_compiler:attribute(1,'FLIGHT','FLIGHT_NO',string).
sql_compiler:attribute(2,'FLIGHT','DEPARTURE',string).
sql_compiler:attribute(3,'FLIGHT','DESTINATION',string).
sql_compiler:attribute(4,'FLIGHT','PLANE_TYPE',string).


sql_compiler:attribute(1,'PLANE','TYPE',string).
sql_compiler:attribute(2,'PLANE','SEATS',integer).

   

sql_compiler:relation(term,3).
sql_compiler:relation(term2term,3).
sql_compiler:attribute(1,term,id,integer).
sql_compiler:attribute(2,term,acc,string).
sql_compiler:attribute(3,term,name,string).
sql_compiler:attribute(1,term2term,pid,integer).
sql_compiler:attribute(2,term2term,tid,integer).
sql_compiler:attribute(3,term2term,cid,integer).

sql_compiler:view(class(ID,N),(term(_,ID,N))).
sql_compiler:view(parent(ID,PID),(term(UID,ID,_),term2term(PUID,_,UID),term(PUID,PID,_))).
sql_compiler:view(subclass(ID,PID),(term(UID,ID,_),
                                    term2term(PUID,TUID,UID),
                                    term(PUID,PID,_),
                                    term(TUID,_,is_a))).
sql_compiler:view(subclassT(ID,PID),(subclass(ID,ZID),subclassT(ZID,PID))).
sql_compiler:unique(term,id).
sql_compiler:unique(term,acc).

unittest:testq(c2p(CN,PN),(term(CID,_,CN),term2term(PID,_,CID),term(PID,_,PN))).
unittest:testq(class(ID,N),(term(_,ID,N))).
unittest:testq(parent(ID,PID),((term(UID,ID,_),term2term(PUID,_,UID)),term(PUID,PID,_))).
unittest:testq(parentN(N,PN),(class(ID,N),parent(ID,PID),class(PID,PN))).
unittest:testq(subclassN(N,PN),(class(ID,N),subclass(ID,PID),class(PID,PN))).
unittest:testq(testid(PID),(term(ID,_,N),(N=fred;N=joe),term2term(ID,_,PID))).
unittest:testq(testid(ID),(term(PID,_,N),(N=fred;N=joe),term2term(ID,_,PID))).
unittest:testq(testid(ID),(class(ID,N),(N=fred;N=joe))).
%unittest:testq(testid(B), (D=fred,term(B, C, D))).
unittest:testq(testid(B), (term(B, C, D), D=fred, term(B,C,D))).
unittest:testq(testid(N,PID),(class(ID,N),(N=fred;N=joe),subclass(ID,PID))).
%unittest:testq(foo(A),(term(_,ID,N),(N=1 -> A=yes ; A=no))).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                forall(testq(Proj,Goal),
                       (   writeln(trying((Proj :- Goal))),
                           plterm_to_sqlterm(Proj,Goal,SqlTerm),
                           writeln(Proj/SqlTerm),
                           print_sqlterm(SqlTerm),
                           nl))),
            true)).


unittest(test(bench,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                forall(test_sql_compiler:benchmark(10,N,T),
                       writeln(N/T))),
            true)).



