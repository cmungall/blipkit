:- module(test_sql_compiler2,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).

:- load_schema_defs(bio('sql_schema/schema_go')).

goal_expansion(childTermByName(X,Y),rewrite_as_sql(childTermByName(X,Y))).

%sql_compiler:view(class(ID,N),(term(_,N,_,ID,_,_))).
%sql_compiler:view(belongs(ID,O),(term(_,_,O,ID,_,_))).

childTermByName(N,CN):-
        class(ID,N),
        subclass(CID,ID),
        class(CID,CN).

getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

rewrite_as_sql(G):-
        writeln(rewrite(G)),
        clause(G,B),
        getrdb(Rdb),
        rdb_query(Rdb,G,B).


unittest:testq(id(ID),(class(ID,molecular_function))).
unittest:testq(foo(PN,PT),(class(TID,'cysteine biosynthesis'),
                         association(TID,PID,_),
                         gene_product(PID,PN,PT))).
unittest:testq(testid(Ont),(class(ID,'guanine transporter activity'),belongs(ID,Ont))).
unittest:testq(parent(PID),(class(ID,'cysteine biosynthesis'),subclass(ID,PID))).
unittest:testq(c(X),X is count(ID,B^C^D^E^F^term(ID,B,C,D,E,F))).
unittest:testq(X,X is count(ID,belongs(ID,cellular_component))).
%unittest:testq(testid(PID),(term(_,ID,N),(N=fred;N=joe),subclass(ID,PID))).
%unittest:testq(closure(PID),(subclassT(go1,PID))).
%unittest:testq(foo(ID,N2),(term(_,ID,N),term(_,ID,N2))).
%unittest:testq(foo(A),(term(_,ID,N),(N=1 -> A=yes ; A=no))).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                rdb_connect(Rdb,go),
                forall(testq(Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           plterm_to_sqlterm(Proj,Goal,SqlTerm),
                           print_sqlterm(SqlTerm),
                           nl))),
            true)).


unittest(test(inline,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                test_sql_compiler2:test_inline),
            true)).

test_inline:-
        rdb_connect(Rdb,go),
        setrdb(Rdb),
        forall(childTermByName(molecular_function,X),
               writeln(childN=X)).
