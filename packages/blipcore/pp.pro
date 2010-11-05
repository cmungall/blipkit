:- module(pp,
	  [
           peval/1,
           peval/2
           ]).

:- op(900,xfy,:=).
:- op(800,fx,run).
:- op(700,xfy,into).

% ----------------------------------------
% JOB ADMIN AND STATISTICS
% ----------------------------------------

%% used?
:- dynamic job/3.

%% job_status(?Job,?Status)
% Status = ok | running
:- dynamic job_status/2.

number_of_jobs(Status,Num) :-
        aggregate(count,job_status(_,Status),Num).

show_status :-
        aggregate(count,Job,job_status(Job,Status),Num),
        debug(pp,'Jobs [~w] = ~w',[Status,Num]),
        fail.
show_status.


% ----------------------------------------
% EVALUATION
% ----------------------------------------

%% thread_peval( +Goal , ?ThreadId) is det
% create a thread to execute Goal in the pp emulator
thread_peval( G , Id) :-
        thread_create(peval(G), Id, [at_exit(writeln(done(Id)))]).

%% peval(+Goal)
% use meta-circular interpreter to perform or-parallel evaluation of Goal.
% Execution is serial until an external goal is called.
peval( G ) :-
        show_status,
        debug(pp,'Eval: ~w',[G]),
        peval( G, SG),
        (   SG=true
        ->  debug(pp,'Completed: ~w',[G]),
            true
        ;   debug(pp,'Pending goal; pausing...',[]),
            sleep(1),
            peval(SG)),
        show_status.


%% peval(+Goal, ?SuspendedGoal)
% perform a pass-through execution of Goal. Goal is partially executed and
% rewritten as SuspendedGoal. If Goal is wholly executed then SuspendedGoal=true
peval( G, _ ) :-
        info(peval(G)),         % show status
        fail.

peval( G, _ ) :-                % var Goals disallowed
        var(G),
        !,
        throw(var(G)).

peval( call(G), SG ) :-         % call/1
        !,
        peval(G, SG).
        %throw(call(G)).

peval( p(G), true ) :-          % p/1 -- like call/1
        !,
        G,
        info(p(G)).

peval( (G1,G2), SG ) :-         % sequential AND
        !,
        peval(G1,SG1),          
        (   SG1=true
        ->  peval(G2,SG)        % continue
        ;   SG=(SG1,G2)).       % suspend

% TODO: collect.. or perhaps we don't need it.. only for findall clauses
peval( (G1 ; G2), SG ) :-       % parallelizable OR
        % DEPRECATED
        !,
        % TODO - need to copy term
        peval(G1,SG1),     % partially execute G1
        peval(G2,SG2),     % partially execute G2; G1 may be suspended
        (   SG1=true       % if G1 completed, then
        ->  SG=SG2 %    unify findal suspended goal with remainder from G2
        ;   SG2=true            % if G2 completed, then
        ->  SG=SG1 %    unify findal suspended goal with remainder from G1
        ;   SG = (SG1;SG2)). % otherwise OR the partially complete suspended goals


% map(T,G,R,L,Results:list)
%  T template - bound to each member of L
%  G goal
%  R result - bound on each call of G
%  Results - list of R
%
% e.g. map(File, wc(File, WC), WC, Files, WCs)
%    ==> collect(File, wc(File, WC), WC, Files, WCs)
% e.g. map(X,(R is X+1),R,[1,2,3],RL)
peval( map(Template, G, Result, InList, ResultList), SGFinal ) :-
        !,
        info(mapping(Template-InList)),
        % iterate through InList rewriting each Goal
        findall(SG,
                (   member(Template,InList),
                    peval(G, SG)
                ),
                SGs),
        % collection partially executed suspended goals as an AND-goal
        goals_to_goal(SGs,SG1),
        info(sg(SG1)),
        % we do not attempt to collect the results on this sweep; leave
        % this partially executed
        SGFinal = (   SG1,
                      collect(Template, G, Result, InList, ResultList)).


% separate collect stage required to bind variables
peval( collect(Template, G, Result, InList, ResultList), SG ) :-
        !,
        info(collect(Template-InList)),
        % we attempt to collect results by ensuring G has fully completed
        % for all elements in the list.
        findall(Result,
                (   member(Template,InList),
                    peval(G, true)
                ),
                ResultList),
        length(InList,Len),
        (   length(ResultList,Len)
        ->  SG=true
        ;   throw(uhoh(ResultList))). % TODO: check this. Just make SG = input goal?

% external goals
%    deprecated? use x/1 instead
% E.g. Uid := run [ls,Dir] into dir_files(Dir),
peval( Uid := run L into Path, SG ) :-
        !,
        concat_atom(L,' ',Cmd),
        cf(Path,Toks,[]),
        concat_atom(Toks,' ',Uid),
        p( x(Cmd,[]), SG).


% x/2 - external goals
%  Execute Cmd via shell
%  x/2 is like sformat, i.e. x(CmdFmt-IdFmt,Args)
% CmdFmt-IdFmt-Args then becomes a key for the job.
%   CmdFmt is used for generating a shell command.
%   IdFmt is used for generating a filename-safe ID to store the results on the filesystem
%   (in future there will be other options besides the filesystem)
peval( x(Cmd,Args), SG ) :-
        !,
        debug(pp,' x(~w,~w)',[Cmd,Args]),
        %p(cmd_uid(x(Cmd,Args),Uid)),
        cmd_uid(x(Cmd,Args),Uid),
        bg(Cmd-Args,Uid,Finished),
        (   Finished
        ->  SG=true,
            debug(pp,'DONE: ~w',[Uid]),
            retractall(job_status(Cmd-Args,_)),
            assert(job_status(Cmd-Args,ok))
        ;   SG=suspended(x(Cmd,Args))).

peval( suspended(G), SG ) :-
        !,
        info(test_is_finished(G)),
        (   is_finished(G)
        ->  info(finished(G)),
            SG=true,
            debug(pp,'Collecting: ~w',[G]),
            peval(G, SG)
        ;   SG=suspended(G)).

peval( G, SG ) :-
        catch(clause(G,Body),_,fail),
        !,
        peval(Body,SG).


peval( G, true ) :-
        !,
        G.


x(G,A) :-
        throw(lost_context(G-A)).

p(G) :- G.

% old
cmd_uid(x(_-Fmt,Args), Uid) :-
        sformat(Uid,Fmt,Args).

% new
cmd_uid(UidExpr == CmdExpr, Uid) :-
        cf(CmdExpr,CToks,[]),
        concat_atom(CToks,' ',Uid),
        cf(UidExpr,UidToks,[]),
        concat_atom(UidToks,Uid).

cf([]) --> [].
cf([H|L]) --> !,cf(H),cf(L).
cf(H) --> [H].


bg(Cmd,Uid,Finished) :-
      Cmd=Fmt-_-Args,
      sformat(X,Fmt,Args),
      atom_concat(Uid,'.tmp',Tmp),
      %sformat(X2,'(~w > ~w.tmp && mv ~w.tmp ~w) &',[X,Uid,Uid,Uid]),
      concat_atom([test,'-f',Uid],' ',TestCmd),
      shell(TestCmd,Test),
      (   Test=0
      ->  debug(pp,'DONE: ~w',[Uid]),
          Finished=true
      ;   concat_atom(['(',X,'>',Tmp,'&&',mv,Tmp,Uid,')','&'],' ',X2),
          debug(pp,'EXEC: ~w',[X2]),
          shell(X2),
          assert(job_status(Cmd,running)),
          Finished=false).


is_finished(G) :-
        cmd_uid(G,Uid),
        exists_file(Uid).

file_parse_int(F,Num) :-
        p((file_readlines(F,[Line|_]),
           atom_number(Line,Num))).
%        p(read_file_to_terms(F,[Num|_],[])).

goals_to_goal([],true) :- !.
goals_to_goal([G],G) :- !.
goals_to_goal([G|Gs],(G,G2)) :-
        !,
        goals_to_goal(Gs,G2).

info(X) :-  debug(pp_info,'~w',[X]).

% ----------------------------------------
% TEST WORKFLOW
% ----------------------------------------


%xc(dir_files(Dir,Files), Files=ls(Dir)).
dir_files(Dir,Files) :-
        Cmd=x('ls ~w*'-'x-ls-~w',[Dir]),
        %Uid := run [ls,Dir] into dir_files(Dir),
        Cmd,
        cmd_uid(Cmd,Uid),
        p(file_readlines(Uid,Files)).

file_readlines(File,Lines) :-
        info(opening(File)),
        nl,
        open(File,read,IO),
        readlines(IO,Lines),
        info(lines=Lines),
        close(IO).
readlines(IO,[]) :- at_end_of_stream(IO), !.
readlines(IO,[Line|L]) :-
        read_line_to_codes(IO,Codes),
        atom_codes(Line,Codes),
        readlines(IO,L).

%xc(grepc(File,Arg,C), C=[grep,-c,Arg,File]).
grepc(File,Arg,C) :-
        Cmd=x('grep -c ~w ~w'-'x-grepc-~w-~w',[Arg,File]),
        %Cmd= grepc(Arg,File) := [grep,-c,Arg,File],
        Cmd,
        cmd_uid(Cmd,Uid),
        file_parse_int(Uid,C).
                    

grepcdir(Dir, Arg, TotalC) :-
        dir_files(Dir, Files),
        map(File, grepc(File, Arg, C), C, Files, Cs),
        p(sumlist(Cs, TotalC)).

% find all instances of 'x' in directory 'foo'
t :-
        peval(grepcdir(foo,x,N)),
        writeln(n=N).

t1 :-
        peval(grepc('README',x,N)),
        writeln(n=N).

t1b :-
        peval(  map(File, grepc(File, x, C), C, ['README','foo-README'], L) ),
        writeln(L).

t1c :-
        peval(  dir_files(foo,L) ),
        writeln(L).

t1d :-
        peval(  (dir_files(foo,Files), map(File, grepc(File, x, C), C, Files, L)) ),
        writeln(L).

t2 :-
        peval( map(T,(x('echo ~w'-'echo-~w',T),X is T+1),X,[1,2,3],L) ),
        writeln(L).

t3 :-
        peval( map(T,(C=x('echo ~w'-'echo-~w',T),C,cmd_uid(C,Uid),file_parse_int(Uid,N),X is N+1),X,[1,2,3],L) ),
        writeln(L).

        


/** <module> parallel job scheduling using prolog rules

  ---+ Synopsis

==
:- use_module(bio(pp)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
        
