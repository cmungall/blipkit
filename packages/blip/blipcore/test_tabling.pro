:- module(test_tabling, [fib/2,fibtest/1,isaT/2]).
:- use_module(bio(tabling)).

isa(a,b).
isa(a,c).
isa(c,d).
isa(d,e).
isa(d,f).
isa(f,a).

isaT(X,Y):- isa(X,Z),isaT(Z,Y).
isaT(X,Y):- isa(X,Y).

foo(a,_).
foo(b,_).
foo(b,_).

fib(X,V):-
        (   X<2
        ->  V=X
        ;   Xm1 is X-1,
            Xm2 is X-2,
            fib(Xm1,V1),
            fib(Xm2,V2),
            V is V1+V2).

fibtest(Max):-
        fibtest(Max,1).
fibtest(Max,N):-
        N =< Max,
        !,
        time_goal(fib(N,V),T),
        format('~w ~w ~f~n',[N,V,T]),
        N1 is N+1,
        fibtest(Max,N1).
fibtest(_,_).

time_goal(G,T):-
        get_time(T1),
        G,
        get_time(T2),
        T is T2-T1.

% this test should be run first
unittest(test(fib_nt,
            [],
            (   fibtest(25)),
            true)).

unittest(test(fib_t,
            [],
            (   ensure_loaded(bio(tabling)),
                table_pred(test_tabling:fib/2),
                fibtest(40),
                clear_table_pred(test_tabling:fib/2)
            ),
            true)).

unittest(test(isa,
            [],
            (   ensure_loaded(bio(tabling)),
                table_pred(test_tabling:isaT/2),
                findall(X,isaT(a,X),Xs),
                writeln(Xs),
                findall(X,isaT(a,X),Xs2),
                writeln(Xs2),
                table_pred(test_tabling:foo/2),
                % 2nd arg is left unbound
                findall(A-B,test_tabling:foo(A,B),ABs),
                writeln(ABs),
                % ideally this would be the same as the above
                % but we can't distinguish the results
                % because of the unbound var... TODO?
                findall(A-B,test_tabling:foo(A,B),ABs2),
                writeln(ABs2),
                nl),
            true)).

unittest(load(go)=
      load_bioresource(go)/[]).

unittest(test(go1,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                findall(X-Y-Z,redundant_subclass(X,Y,Z),XYZs),
                length(XYZs,Num),
                writeln(num=Num)),
            true)).


unittest(test(go2,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(tabling)),
                table_pred(ontol_db:subclassT/2),
                findall(X-Y-Z,redundant_subclass(X,Y,Z),XYZs),
                length(XYZs,Num),
                writeln(num=Num)),
            true)).



      
/*

  tabling is noticeable after 8 or so

*/

  
