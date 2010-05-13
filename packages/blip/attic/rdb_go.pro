
:- module(rdb_go,
          [
           testq/0,
           teste/0,
           connect/1
          ]).

:- use_module(bio(rdb_util)).
:- use_module(library(odbc)).

connect(X):-
        odbc_connect('go', X,
                     [ user(cjm),
                                %password(xxx),
                       alias(go),
                       open(once)
                     ]),
        assert(cx(X)).

class(ID,N):-
        cx(X),
        sql_lookup(X,term,[acc=ID,name=N]).

testq:-
        rdb_connect(Dbh,go),
        rdb_lookup(Dbh,term(ID,N,cellular_component)),
        writeln(ID/N),
        fail.

teste:-
        rdb_connect(Dbh,hs),
        rdb_lookup(Dbh,gene(ID)),
        writeln(ID),
        fail.

query1(X):-
        odbc_prepare(X,
                     'SELECT name,term_type FROM term WHERE acc = ?',
                     [default],
                     Sth,
                     []),
        odbc_execute(Sth,['GO:0003673'],row(N,M)),
        write(N/M),
        fail.
query2(X):-
        odbc_prepare(X,
                     'SELECT acc,name FROM term WHERE term_type = ?',
                     [default],
                     Sth,
                     []),
        odbc_execute(Sth,['biological_process'],R),
        write(R),
        fail.
