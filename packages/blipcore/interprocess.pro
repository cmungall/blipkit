:- module(interprocess,[
                        init_ipr_session/1,
                        init_ipr_session/2,
                        kill_ipr_session/1,
                        ipr_assert/2,
                        ipr_retractall/2,
                        ipr_query/3,
                        ipr_query/4,
                        ipr_call/3,
                        list_active_ipr_sessions/0,
                        kill_all_active_ipr_sessions/0
                        ]).

:- use_module( library(lists) ).
:- use_module( library(readutil) ). % read_line_to_codes/2.

% Swi declaration:
:- use_module( library(process) ).   % process_create/3.

:- dynamic active_ipr_session/1.

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

%% ipr_query(+Stream,+Goal,?Results:list) is det
%
% calls findall/3 on Goal in slave database
%
% Stream = s(In,Out,Err,PID) -- see set_pr_streams/3
ipr_query(Stream,Goal,Results) :-
        ipr_query(Stream,Goal,Goal,Results).

%% ipr_query(+Stream,+Template,+Goal,?Results:list) is det
% selects Template from Goal
ipr_query(Stream,Template,Goal,Result) :-
        Stream = s(Ri,Ro,_Re,_),
        % multiple newlines required to stymie y/n prompts
        format(Ri,'findall(~q,(~q),L_internal__),writeq(L_internal__),nl.~n~n~n~n~n~n~n',[Template,Goal]),
        flush_output(Ri),
        format(Ri,'~q.~n',[writeln(end_of_pl_out____)]),
        flush_output(Ri),
        r_read_lines(Ro,"end_of_pl_out____",CodesList),
        debug(ipr,'FinalCodesList: ~w',[CodesList]),
        % may have side-effects; we want the last line output
        reverse(CodesList,[Codes|_]),
        atom_codes(A,Codes),
        atom_to_term(A,Result,_Bindings).

ipr_call(Stream,Goal,Result) :-
        Stream = s(Ri,Ro,_Re,_),
        % multiple newlines required to stymie y/n prompts
        %format(Ri,'forall(~q,true),nl.~n~n~n~n~n~n~n',[Goal]),
        format(Ri,'(~q,fail);nl.~n~n~n~n~n~n~n',[Goal]),
        flush_output(Ri),
        format(Ri,'~q.~n',[writeln(end_of_pl_out____)]),
        flush_output(Ri),
        r_read_lines(Ro,"end_of_pl_out____",CodesList),
        debug(ipr,'CallFinalCodesList: ~w',[CodesList]),
        % may have side-effects; we want the last line output
        findall(A,(member(Codes,CodesList),
                   atom_codes(A,Codes)),
                Result).


%% ipr_assert(+Stream,+Goal) is det
% asserts a clause in the slave database
ipr_assert(Stream,Goal) :-
        Stream = s(Ri,Ro,_Re,_),
        format(Ri,'assert((~q)).~n',[Goal]),
        format(Ri,'~q.~n',[writeln(end)]),
        r_read_lines(Ro,"end",_).

ipr_retractall(Stream,Goal) :-
        Stream = s(Ri,Ro,_Re,_),
        format(Ri,'retractall((~q)).~n',[Goal]),
        format(Ri,'~q.~n',[writeln(end)]),
        r_read_lines(Ro,"end",_).

%% kill_ipr_session(+S)
% kill slave process
kill_ipr_session(S) :-
        debug(interprocess,'halting slave: ~w',[S]),
        S = s(Ri,_Ro,_Re,PID),
        format(Ri,'halt.~n',[]),
        retractall(active_ipr_session(S)),
        debug(interprocess,'releasing process: ~w',[PID]),
        process_release(PID).

/*
init_process( Ri, Ro, Re ) :-
        init_process( Ri, Ro, Re, _).

init_process( Ri, Ro, Re, PID ) :-
        Opts = [process(PID),stdin(pipe(Ri)),stdout(pipe(Ro)),stderr(pipe(Re))],
        debug(interprocess,'creating slave process with opts: ~w',[Opts]),
        process_create( path(blip), [], Opts ).
*/

%% init_ipr_session( ?S ) is det
% create a slave database
init_ipr_session( S ) :-
        init_ipr_session( S, []).

init_ipr_session( S, Opts ) :-
        POpts = [process(PID),stdin(pipe(Ri)),stdout(pipe(Ro)),stderr(pipe(Re))],
        debug(interprocess,'creating slave process with opts: ~w',[Opts]),
        option(exe(Exe), Opts, swipl),
        option(args(Args), Opts, []),
        process_create( path(Exe), Args, POpts ),
        set_pr_streams(Ri,Ro,Re),
        S = s(Ri,Ro,Re,PID),
        assert(active_ipr_session(S)),
        additional_calls(S,Exe).

additional_calls(S,yap) :- ipr_call(S,use_module(dialect/swi),_),!.
additional_calls(S,_).


%% set_pr_streams( +In, +Out, +Err ) is det
set_pr_streams( Ri, Ro, Re ) :-
     set_stream( Ri, buffer(false) ), set_stream( Ri, close_on_abort(true) ),
     set_stream( Ro, buffer(false) ), set_stream( Ro, close_on_abort(true) ),
     set_stream( Re, buffer(false) ), set_stream( Re, close_on_abort(true) ).

list_active_ipr_sessions :-
        forall(active_ipr_session(S),
               writeln(S)).

kill_all_active_ipr_sessions :-
        forall(active_ipr_session(S),
               kill_ipr_session(S)).
        

/** <module> interprocess communication between prolog engines

  ---+ Synopsis

==
setup_call_cleanup(init_ipr_session(S),
  (ipr_query(S,_,assert(foo(fred)),_),ipr_query(S,_,assert(foo(jim)),_),ipr_query(S,X,foo(X),L)),
  kill_ipr_session(S)).
==

  using the convenience ipr_assert/2 predicate:
  
==
setup_call_cleanup(init_ipr_session(S),
  (ipr_assert(S,foo(fred)),ipr_assert(S,foo(jim)),ipr_query(S,X,foo(X),L)),
  kill_ipr_session(S)).
==

---+ Details

modified from r_session.pl

*/
