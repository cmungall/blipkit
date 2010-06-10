/* -*- Mode: Prolog -*- */

:- use_module(sql_compiler).

:- op(1100,fx,testq).
:- op(1000,xfy,where).

test_query(Proj,Goal) :-
        writeln(Proj where Goal),
        plterm_to_sqlterm(Proj,Goal,SqlTerm),
        writeln(SqlTerm),
        sqlterm2atom(SqlTerm,Sql),
        writeln(Sql),
        nl.

:- begin_tests(flights, []).

%% flight(Flight_no,Departure,Destination,Plane_type)
sql_compiler:relation(flight,4).

%% plane(Type, Seats)
sql_compiler:relation(plane,2).

%% flight_departure_time(Flight, Mins)
sql_compiler:relation(flight_departure_time,2).

%% flight_arrival_time(Flight, Mins)
sql_compiler:relation(flight_arrival_time,2).

sql_compiler:attribute(1,flight,flight_no,string).
sql_compiler:attribute(2,flight,departure,string).
sql_compiler:attribute(3,flight,destination,string).
sql_compiler:attribute(4,flight,plane_type,string).

sql_compiler:attribute(1,plane,type,string).
sql_compiler:attribute(2,plane,seats,integer).

sql_compiler:attribute(1,flight_departure_time,flight_no,string).
sql_compiler:attribute(2,flight_departure_time,mins,integer).

sql_compiler:attribute(1,flight_arrival_time,flight_no,string).
sql_compiler:attribute(2,flight_arrival_time,mins,integer).

% non-recursive datalog clauses are treated as views
user:connecting_flight(F1,F2,Dep,Dest,Via) :-
        flight(F1,Dep,Via,_),
        flight(F2,Via,Dest,_).

user:flight_duration(F,Dur) :-
        flight_departure_time(F,T1),
        flight_arrival_time(F,T2),
        Dur is T2-T1.

user:connecting_flight_duration(F1,F2,Dur) :-
        flight_departure_time(F1,T1),
        flight_arrival_time(F2,T2),
        Dur is T2-T1.

user:b737_flight(F,Dep,Dest) :-
        flight(F,Dep,Dest,b737).

testq flight(No,Dep,Dest,Type) where flight(No,Dep,Dest,Type).
testq capacity(No,Dep,Dest,Type,Seats) where
	    (flight(No,Dep,Dest,Type),
             plane(Type,Seats),
             Type='b-737').
testq no_planes(No,Dep,Dest,Type) where
	      (flight(No,Dep,Dest,Type),
		   \+ plane(Type,Seats)).


testq big_planes(munich,Dest,Type,Seats) where
	      FNo^(flight(FNo,munich,Dest,Type),
		       plane(Type,Seats),
			   Seats > avg(S, T^plane(T,S))).


%testq p(P,X) where plane(P,_),X is count(S,plane(P,S)).
testq X where X is count(S,plane(P,S)). % pointless query; need existential, see next
testq X where X is avg(S,P^plane(P,S)).
testq P^X where X is avg(S,plane(P,S)).

testq conn(F1,F2,Via) where
   (   flight(F1,Dep,Via,_),
        flight(F2,Via,Dest,_),
       Dep=sfo,
       Dest=edi).

testq conn(F1,F2,Via) where
   (   connecting_flight(F1,F2,Dep,Dest,Via),
       Dep=sfo,
       Dest=edi).

testq duration(D) where
   flight_duration(f1,D).

testq X where X is avg(D,F^flight_duration(F,D)).

testq Via-D where
   connecting_flight(F1,F2,sfo,edi,Via),
   connecting_flight_duration(F1,F2,D).

testq F where b737_flight(F,_,_).

test(all):-
        findall(Proj-Where,
                (   (   testq Proj where Where),
                    \+ test_query(Proj,Where)),
                Fails),
        (   Fails=[]
        ->  format('passed~n')
        ;   format('FAILED:~n'),
            maplist(writeln,Fails),
            fail).

:- end_tests(flights).

