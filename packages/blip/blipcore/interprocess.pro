:- module(interprocess,[
                        testpr/0,
                        init_pr/1,
                        ipr_assert/2,
                        ipr_query/3,
                        ipr_query/4
                        ]).

:- use_module( library(lists) ).
:- use_module( library(readutil) ). % read_line_to_codes/2.

% Swi declaration:
:- use_module( library(process) ).   % process_create/3.

r_read_lines( Ro, TermLine, Lines ) :-
     read_line_to_codes( Ro, Line ),
     debug(ipr,' Codes: ~s :: ~w',[Line,Line]),
     r_read_lines_1( Line, TermLine, Ro, Lines ).

r_read_lines_1( eof, _TermLine, _Ro, Lines ) :- !, Lines = [].
r_read_lines_1( end_of_file, _TermLine, _Ro, Lines ) :- !, Lines = [].
r_read_lines_1( [255], _TermLine, _Ro, Lines ) :- !, Lines = []. 
     % yap idiosyncrasy
r_read_lines_1( TermLine, TermLine, _Ro, Lines ) :- !, Lines = [].
r_read_lines_1( Line, TermLine, Ro, [Line|Lines] ) :-
     debug(ipr,'     Line: ~s',[Line]),
     %\+ at_end_of_stream(Ro),
     read_line_to_codes( Ro, NewLine ),
     debug(ipr,'     Codes: ~s',[NewLine]),
     r_read_lines_1( NewLine, TermLine, Ro, Lines ).

testpr:-
        init_process(Ri,Ro,Re),
        debug(ipr,'Setting streams: ~w ~w ~w',[Ri,Ro,Re]),
        set_pr_streams(Ri,Ro,Re),
        Stream = s(Ri,Ro,Re,_),
        forall(member(X,[a,bb,ccc,dddd]),
               (   ipr_query(Stream,X,writeln(X),Result),
                   format('result=~w',[Result]))).

%% ipr_query(+Stream,+Goal,?Result) is
% Stream = s(In,Out,Err,PID) -- see set_pr_streams/3
ipr_query(Stream,Goal,Results) :-
        ipr_query(Stream,Goal,Goal,Results).

ipr_query(Stream,Template,Goal,Result) :-
        Stream = s(Ri,Ro,_Re,_),
        format(Ri,'findall(~q,(~q),L),writeq(L),nl.~n',[Template,Goal]),
        format(Ri,'~q.~n',[writeln(end)]),
        r_read_lines(Ro,"end",CodesList),
        debug(ipr,'CodesList: ~w',[CodesList]),
        % may have side-effects; we want the last line output
        reverse(CodesList,[Codes|_]),
        atom_codes(A,Codes),
        atom_to_term(A,Result,_Bindings).
        

ipr_assert(Stream,Goal) :-
        Stream = s(Ri,Ro,_Re,_),
        format(Ri,'assert((~q)).~n',[Goal]),
        format(Ri,'~q.~n',[writeln(end)]),
        r_read_lines(Ro,"end",_).

ipr_kill(s(_,_,_,PID)) :-
        process_release(PID).

init_process( Ri, Ro, Re ) :-
        init_process( Ri, Ro, Re, _).

init_process( Ri, Ro, Re, PID ) :-
     Opts = [process(PID),stdin(pipe(Ri)),stdout(pipe(Ro)),stderr(pipe(Re))], 
     process_create( path(swipl), [], Opts ).

init_pr( S ) :-
        init_process(Ri,Ro,Re,PID),
        set_pr_streams(Ri,Ro,Re),
        S = s(Ri,Ro,Re,PID).
set_pr_streams( Ri, Ro, Re ) :-
     set_stream( Ri, buffer(false) ), set_stream( Ri, close_on_abort(true) ),
     set_stream( Ro, buffer(false) ), set_stream( Ro, close_on_abort(true) ),
     set_stream( Re, buffer(false) ), set_stream( Re, close_on_abort(true) ).

/** <module> interprocess communication between prolog engines

  ---+ Synopsis

==
init_pr(S),ipr_query(S,_,assert(foo(fred)),_),ipr_query(S,_,assert(foo(jim)),_),ipr_query(S,X,foo(X),L).
==

==
init_pr(S),ipr_assert(S,foo(fred)),ipr_assert(S,foo(jim)),ipr_query(S,X,foo(X),L).
==

---+ Details

modified from r_session.pl

*/
