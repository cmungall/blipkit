/* -*- Mode: Prolog -*- */

:- module(bioprolog_util,
          [load_factfiles/1,
           load_factfile/1,
           load_factfile/2,
           die/1,
           trust_current_user/0,
           getopt/2,
           getopt/3,
	   atom_atom_concat/4,
           clause_source/2,
           clause_source_short/2,
           read_file_to_tokens/2,
           atom_to_tokens/2,
           atom_to_tokens/3,
           codes_to_tokens/2,
           codes_to_tokens/3,
           read_tab_delimited_line/2,
           replace_chars/3,
           replace_codes/3,
           remove_chars/3,
           remove_codes/3,
           list_subtract/3,
           list_min/2,
           list_max/2,
           list_avg/2,
           read_file_to_rows/3,
           read_file_to_rows/4,
           read_file_to_db/3,
           time_goal/2,
           n_time_goal/3,
           n_times/2,
           conset/2,
           conget/2,
           coninc/2,

           writetab/0,
           write_termrow/1,
           write_termrow/2,
           writecols/1,
           writecols/2,

           list_to_tuple/2,
           tuple_to_list/2,

           iterate_over_input/3,
           iterate_over_input/4,
           
           ensure_nonvar/2,
           compare_two_lists/4,
           pred_to_unground_term/2,
           term_gensym/3,

           call_on_stream/3,
           dsetof/3,
           solutions/3,
           oneof/1,
           call_det/1,
           call_unique/1,
           sumof/3,
           number_setof/3,
           number_list_items/2,
           number_list_items/3,
           setof_count/3,
%           count_distinct/3,
           group_by/3,
           group_by/4,
           count_by/3,
           count_by/4,
	   aggregate_by/4,
           forall_distinct/2,
           forall_strict/2,
           findmax/4,
           findmax/5,

           dot_product/3,
           vector_magnitude/2,
           cosine_similarity/3,
           if/2,
           op(800,xfy,if)
          ]).
:- op(800,xfy,if).

:- module_transparent
        if/2,
        time_goal/2,
        load_factfiles/1,load_factfile/1,iterate_over_input/3,
        call_on_stream/3,
        iterate_over_input/4,dsetof/3,solutions/3,oneof/1,call_det/1,sumof/3,setof_count/3,group_by/3,group_by/4,count_by/3,count_by/4,forall_distinct/2,forall_strict/2,findmax/4,findmax/5.

:- dynamic con/2.
:- multifile user:trusted_user/1.
:- multifile user:bioresource/1, bioresource/2.
:- multifile user:rebuild_all_qlf/0.

Goal if Condition:-
    (	Condition
    ->	Goal
    ;	true).


%% load_factfiles(+PlFiles) is det.
%   as load_factfile/1 but loads a list of fact files
%  
load_factfiles([]).
load_factfiles(L) :-
    findall(F,(member(F,L),load_factfile(F)),_).

%% load_factfile(+PlFile) is det.
%  consults a prolog fact file. uses qcompiled version if present.
%  will recompile on-the-fly if required.
%  This module is transparent, so facts will be loaded into calling
%  module space
load_factfile(PlFile) :-
        style_check(-discontiguous),
        style_check(-atom),
	file_name_extension(Base, _Ext, PlFile),
	file_name_extension(Base, qlf, QlfFile),
        debug(load,'checking for: ~w',[QlfFile]),
	(   exists_file(QlfFile),
            not(user:rebuild_all_qlf),
	    time_file(QlfFile, QlfTime),
	    time_file(PlFile, PlTime),
	    QlfTime >= PlTime
	->  load_files([QlfFile])
	;   access_file(QlfFile, write)
	->  qcompile(PlFile)
        ;   debug(load,'  cannot write to qlf, loading from: ~w',[PlFile]),
            load_files(PlFile)
	).

% TODO: MERGE THESE!

%% load_factfile(+PlFile,+Mod)
%   as load_factfile/1, but explicitly load into module Mod
load_factfile(PlFile,Mod):-
        style_check(-discontiguous),
        style_check(-atom),
	file_name_extension(Base, _Ext, PlFile),
	file_name_extension(Base, qlf, QlfFile),
	statistics(cputime, CpuOld),
	(   exists_file(QlfFile),
	    time_file(QlfFile, QlfTime),
	    time_file(PlFile, PlTime),
            debug(load,'Found qlf file: ~w; checking times (pro,qlf): ~f, ~f',[QlfFile,PlTime,QlfTime]),
	    QlfTime >= PlTime,
            % now check version are in sync
            % see email to swi-prolog list, 2005-12-06
            debug(load,'qlf more recent; checking qlf version: ~w',[QlfFile]),
            %'$qlf_info'(QlfFile,V,V,_,_),
            V=fake,             % no longer works, at least in pl.5.6.34
            debug(load,'qlf version: ~w',[V])
	->  Mod:load_files([QlfFile])
	;   access_file(QlfFile, write)
	->  debug(load,'Recompiling ~w',[QlfFile]),
            qcompile(PlFile),Mod:load_files([QlfFile])
	;   debug(load,'cannot write to ~w; using ~w (may be slower to load)',[QlfFile,PlFile]),
            Mod:load_files(PlFile)
	),
	statistics(cputime, CpuNew),
        Time is CpuNew - CpuOld,
        debug(load,'Loaded ~w into ~w time:~w',[PlFile,Mod,Time]).

die(M):-
    format(user_error,
	   '~nERROR: ~w~n[program terminated with error]~n',[M]),
    halt(1).

% trust unless specified otherwise.
% generally trusted_user(false) this would be asserted in a wrapper script to
% be executed via the web
trust_current_user:-
        %user:trusted_user(true),
        forall(user:trusted_user(True),
               True=true).

%% getopt(+OptL,?FileL) is det
%  FileL unifies with list of files after processing command line options
%
%  options are:
%
%  Type(+Opt,?Val)
%  Type(+Opt,?Val,+Default)
%
%  where Type = opt | bool | number
%        Opt  = atom | list
%
%  examples:
%
%  ==
%  getopt([number(port,P)],FileL).
%  getopt([number([p,port],P,8080)],FileL).
%  ==
%
%  if an option is not specified by the user, and no default is specified,
%  then Val will not be unified - it will remain a var
%
%  ALGORITHM:
%
%  currently it goes throw the opt spec in order and extracts options
%from the command line arg list; this could lead to some unusual side
%effects if the command line list is wrong; eg
%
%  -port -conf foo
%
%  with spec [atom(port,P),atom(conf,C)] - port will be processed
%first, and "-conf" will be the argument
%
%  IMPLEMENTATION MAY CHANGE!!!!!
getopt(OptL,RemL):-
        current_prolog_flag(argv, ArgvIn),
        append(_,[--|ArgL],ArgvIn),
        getopt_args(OptL,RemL,ArgL).

getopt_args(OptL,RemL,ArgL):-
        current_prolog_flag(argv, ArgvIn),
        append(_,[--|ArgL],ArgvIn),
        debug(opt,'opts ~w',[OptL]),
        (   \+ trust_current_user
        ->  forall(member(Opt,OptL),
                   check_opt(Opt,ArgL))
        ;   true),
        (   getopt(OptL,ArgL,RemL)
        ->  true
        ;   throw(getopt(OptL,ArgL,RemL))
        ).

check_opt(Opt,ArgL):-
        debug(opt,'checking ~w',[Opt]),
        Opt =.. [_,Tags1|_],        
        (   is_list(Tags1) -> Tags = Tags1 ; Tags = [Tags1]),
        debug(opt,'  checking ~w',[Tags]),
        (   member(ITag,Tags),
            user:opt_insecure(ITag)
        ->  forall(member(Tag,Tags),
               (   atom_concat('-',Tag,Sw),
                   %format(user_error,'checking ~w in ~w~n',[Sw,ArgL]),
                   (   member(Sw,ArgL)
                   ->  throw(permission('You do not have sufficient permission to run ',Sw))
                   ;   true)))
        ;   true).


% getopt(+OptL,+ArgL,?RemL)
getopt([],L,L).


getopt([Sw|SwL],ArgL,RemL):-
        (   \+ trust_current_user
        ->  check_opt(Sw,ArgL)
        ;   true),
        Sw =.. [Type,Opt,Val|DefaultL],
        (is_list(Opt) -> OptL = Opt ; OptL = [Opt]),
        process_opt(Type,OptL,Val,DefaultL,ArgL,ArgL2), % det
        getopt(SwL,ArgL2,RemL).

normalize_opt(Sw/_,Sw):- !.
normalize_opt(Sw,Sw):- !.

% (+,?) nondet
% generates a possible tag that may be used by user;
% example ([p,port], '-p')
opt_tag(OptL,Opt):-
        member(OptX,OptL),
        atom_concat('-',OptX,Opt).

% (+,+,?,+,+,?) det
% bool: no succeeding arguments
process_opt(bool,OptL,1,_,ArgL,ArgL2):-
        opt_tag(OptL,Opt),
        member(Opt,ArgL),
        append(Left,[Opt|Right],ArgL),
        !,
        append(Left,Right,ArgL2).
process_opt(bool,_,0,_,ArgL,ArgL):-
        !.
process_opt(atoms,OptL,Vals,_,ArgL,ArgLret):-
        process_opt(atom,OptL,Val,_,ArgL,ArgL2),
        (var(Val)
        ->  Vals=[],
            ArgLret=ArgL2
        ;   process_opt(atoms,OptL,ValsT,_,ArgL2,ArgLret),
            Vals=[Val|ValsT]),
        !.
process_opt(terms,OptL,Vals,_,ArgL,ArgLret):-
        process_opt(term,OptL,Val,_,ArgL,ArgL2),
        (var(Val)
        ->  Vals=[],
            ArgLret=ArgL2
        ;   process_opt(terms,OptL,ValsT,_,ArgL2,ArgLret),
            Vals=[Val|ValsT]),
        !.
% non-bool: has a succeeding argument
process_opt(Type,OptL,Val,_,ArgL,ArgL2):-
        opt_tag(OptL,Opt),
        append(Left,[Opt,ValX|Right],ArgL), % (?,[+,?|?],+) sd
        !,
        opt_type_map(Type,ValX,Val),
        append(Left,Right,ArgL2).
% not on cmd line, use default
process_opt(_,_,Val,[Val],ArgL,ArgL):- !.
process_opt(_,_,_,_,ArgL,ArgL).

% (+,+,?) det
% checks the incoming type
opt_type_map(number,Val,Num):-
        atom_number(Val,Num),
        !.
opt_type_map(term,Atom,Term):-
        atom_to_term(Atom,Term,_),
        !.
opt_type_map(_,Val,Val).

        
anonvarlist([],[]).
anonvarlist([_|T],[_|T2]):-
        anonvarlist(T,T2).

atom_atom_concat(A,B,C,D) :-
	atom_concat(A,Z,D),
	atom_concat(B,C,Z).


clause_source(G,S):-
        clause(G,_,DClause),
        clause_property(DClause,file(S)).

clause_source_short(G,S):-
        clause_source(G,S1),
        concat_atom(L,'/',S1),
        reverse(L,[S|_]).


%% read_file_to_tokens(+File,?Tokens) is det
read_file_to_tokens(File,Tokens):-
        read_file_to_codes(File,Codes,[]),
        atom_codes(FileContentsAtom,Codes),
        atom_to_tokens(FileContentsAtom,Tokens).

%% atom_to_tokens(+A,?AL)
%  as atom_to_tokens/3, uses default_token_list/1
atom_to_tokens(A,AL):-
        atom_codes(A,CL),
        default_token_list(TL),
        codes_to_tokens(CL,TL,AL).
%% atom_to_tokens(+A,+TL,?AL)
%  converts an atom into a list of token atoms
atom_to_tokens(A,TL,AL):-
        atom_codes(A,CL),
        codes_to_tokens(CL,TL,AL).

%% codes_to_tokens(+CL,?AL)
codes_to_tokens(CL,AL):-
        default_token_list(TL),
        codes_to_tokens(CL,TL,AL).
%% codes_to_tokens(+CL,+TL,?AL)
%  converts a list of tokens into a list of token atoms
%  optional tokenlist (as codes); defaults to 9,10,13,32
codes_to_tokens(CL,TL,AL):-
        codes_to_tokens(CL,TL,AL,[]).
codes_to_tokens([],_,[A],BL):-
        atom_codes_rev(A,BL).
codes_to_tokens([C|CL],TL,ALr,BL):-
        whitespace_code(C,TL),
        !,
        (BL=[]
        ->  ALr = AL
        ;   atom_codes_rev(A,BL),
            ALr = [A|AL]),
        codes_to_tokens(CL,TL,AL,[]).
codes_to_tokens([C|CL],TL,ALr,BL):-
        nontoken_code(C,TL),
        !,
        (BL=[]
        ->  ALr = AL
        ;   atom_codes_rev(A,BL),
            ALr = [A|AL]),
        codes_to_tokens(CL,TL,AL,[C]).
codes_to_tokens([C|CL],TL,AL,BL):-
        codes_to_tokens(CL,TL,AL,[C|BL]).

is_word_code(95).
is_word_code(C):-
        C >= 97, C =< 122.
is_word_code(C):-
        C >= 65, C =< 90.
whitespace_code(C,TL):-
        member(ws(WL),TL),
        member(C,WL).
nontoken_code(C,TL):-
        member(words,TL),
        not(is_word_code(C)).
        

% these are all semi-deprecated; moved to tokenizer module
default_token_list([ws([9,10,13,32])]).
%default_line_token_list([ws([9,10,13,32])]).
read_tab_delimited_line(IO,AL):-
        default_token_list(TL),
        read_line_to_tokens(IO,TL,AL).
read_line_to_tokens(IO,TL,AL):-
        read_line_to_codes(IO,CL),
        codes_to_tokens(CL,TL,AL).

atom_codes_rev(A,CL):-
        reverse(CL,CLr),
        atom_codes(A,CLr).

replace_chars(S,T,S2):-
        atom_codes(S,CL),
        findall([C,C2],(member([A,A2],T),atom_codes(A,[C]),atom_codes(A2,C2)),T2),
        replace_codes(CL,T2,CL2),
        atom_codes(S2,CL2).
replace_codes([],_,[]).
replace_codes([C|CL],Trans,[C2|CL2]):-
        (member([C,C2],Trans)
        ->  true
        ;   C2=C2),
        replace_codes(CL,Trans,CL2).

remove_chars(S,RAL,S2):-
        atom_codes(S,CL),
        findall(C,(member(A,RAL),atom_codes(A,[C])),RCL),
        remove_codes(CL,RCL,CL2),
        atom_codes(S2,CL2).
remove_codes([],_,[]).
remove_codes([C|CL],RL,CL2):-
        member(C,RL),
        !,
        remove_codes(CL,RL,CL2).
remove_codes([C|CL],RL,[C|CL2]):-
        remove_codes(CL,RL,CL2).

list_subtract(L1,L2,L):-
        sort(L1,L1S),
        sort(L2,L2S),
        set_subtract(L1S,L2S,L).

set_subtract(L,[],L):- !.
set_subtract([H|L1],[H|L2],L):-
        !,
        set_subtract(L1,L2,L).
set_subtract([H1|L1],[H2|L2],[H1|L]):-
        H1 @< H2,
        !,
        set_subtract(L1,[H2|L2],L).
set_subtract(_,_,_):- !, fail.


%% list_min(+List,?Min)
%   finds minimum value in list according to arithmetic function min
% DEPRECATED: use min_list/2 in library(lists)
list_min([H|L],Min):-
        list_min(L,Min,H).

list_min([],Min,Min).
list_min([H|L],Min,CurMin):-
        NewMin is min(H,CurMin),
        list_min(L,Min,NewMin).

%% list_max(+List,?Max)
%   finds maximum value in list according to arithmetic function max
% DEPRECATED: use max_list/2 in library(lists)
list_max([H|L],Max):-
        list_max(L,Max,H).

list_max([],Max,Max).
list_max([H|L],Max,CurMax):-
        NewMax is max(H,CurMax),
        list_max(L,Max,NewMax).

%% list_avg(+List,?Avg)
%   average value of all elements in list. Must all be numeric
list_avg(L,Avg):-
        sumlist(L,S),
        length(L,Len),
        (   Len>0
        ->  Avg is S/Len
        ;   Avg=0).

conset(C,V):-
        retractall(con(C,_)),
        assert(con(C,V)).
conget(C,V):-
        con(C,V).
coninc(Cnt, N) :-
     conget(Cnt, N),
     N1 is N + 1,
     conset(Cnt, N1).


%% rem_at_end(+InCodes,+CharCode,?Out)
%   removes chars from end of list of codes
%  (tail recursive)
rem_at_end(In,C,Out) :- rem_at_end(In,C,Cs,Cs,Out).

% (+In,+CharCode,?Codes,?Codes,?)
rem_at_end([],_,_,_,[]).
rem_at_end([X|R],C,Cs,TailCs,Out) :-
        (X \== C ->
            Out = Cs,
            TailCs = [X|NewOut],
            rem_at_end(R,C,NewCs,NewCs,NewOut)
        ;
            TailCs = [C|NewTailCs],
            rem_at_end(R,C,Cs,NewTailCs,Out)
        ).

% todo - rewrite less hacky
:- dynamic row/1.
read_file_to_rows(File,Cols,Rows) :-
        read_file_to_rows(File,Cols,Rows,row).
read_file_to_rows(File,Cols,Rows,RowPred) :-
        retractall(row(_)),
        open(File,read,In,[]),
	repeat,
	(   at_end_of_stream(In)
	->  !
	;   read_line_to_codes(In, Codes),
	    atom_codes(A,Codes),
	    concat_atom(Vals,'\t',A),
            findall(X,
                    (   member(Col,Cols),
                        nth1(Col,Vals,X)),
                    Xs),
            Row=..[RowPred|Xs],
            assert(row(Row)),
	    fail
	),
        close(In),
        findall(Row,row(Row),Rows),
        retractall(row(_)).

read_file_to_db(File,Cols,RowPred) :-
        read_file_to_db(File,Cols,RowPred,user).
read_file_to_db(File,Cols,RowPred,Mod) :-
        debug(read,'Reading: ~w',[File]),
        open(File,read,In,[]),
	repeat,
	(   at_end_of_stream(In)
	->  !
	;   read_line_to_codes(In, Codes),
	    atom_codes(A,Codes),
	    concat_atom(Vals,'\t',A),
            findall(X,
                    (   member(Col,Cols),
                        nth1(Col,Vals,X)),
                    Xs),
            Row=..[RowPred|Xs],
            Mod:assert(Row),
	    fail
	),
        debug(read,'DONE - Read: ~w',[File]),
        close(In).




%% time_goal(+Goal,?Time)
%  calls Goal and unifies Time with the cputime taken
time_goal(Goal,Time):-
        statistics(cputime,T1),
        Goal,
        statistics(cputime,T2),
        Time is T2-T1.

:- module_transparent(n_time_goal/3).
n_time_goal(N,Goal,Time) :-
	time_goal(n_times(N,Goal),Time).

:- module_transparent(n_times/2).
n_times(N,Goal) :-
	between(0,N,_),
	Goal,
	fail.
n_times(_,_).

	
%% write_termrow(+Term)
%   writes a predicate out as a tab delimited line, with predicate name as first column
% uses writecols/1
write_termrow(T):-
        T=..L,
        writecols(L),
        nl.
%% write_termrow(+Stream,+Term)
%   as write_termrow/1 but to a stream
write_termrow(S,T):-
        T=..L,
        writecols_to_stream(S,L),
        nl(S).

writetab:-
        atom_codes(Del,[9]),
        write(Del).

%% writecols(+Cols)
%   write each element in a list separated by tabs. does not write newline
writecols(L):-
        atom_codes(Del,[9]),
        writecols(L,Del).
%% writecols(+Cols,+DelimCode)
%   write each element in a list separated by an atom code. does not write newline
writecols([],_).
writecols([X],_):-
        !,
        writecol(X).
writecols([X|L],Del):-
        !,
        writecol(X),
        write(Del),
        writecols(L,Del).

writecol(null(_)):- !.
writecol(X):- write(X).


writecols_to_stream(S,L):-
        atom_codes(Del,[9]),
        writecols_to_stream(S,L,Del).

writecols_to_stream(_,[],_).
writecols_to_stream(S,[X],_):-
        !,
        write(S,X).
writecols_to_stream(S,[X|L],Del):-
        !,
        write(S,X),
        write(S,Del),
        writecols_to_stream(S,L,Del).

%% ensure_nonvar(?Var,+Default)
%  If Var is ground, do nothing. If Var is unground, unify with Default
ensure_nonvar(Var,Default):-
        var(Var) -> Var=Default ; true.


%% compare_two_lists(+List1,+List2,?UnmatchedInList1,?UnmatchedInList2) is det
% List1 and List2 must be sorted in advance
compare_two_lists([],L,[],L).
compare_two_lists(L,[],L,[]).
compare_two_lists([H1|L1],[H2|L2],R1,R2):-
        compare('=',H1,H2),
        !,
        compare_two_lists(L1,L2,R1,R2).
compare_two_lists([H1|L1],[H2|L2],[H1|R1],R2):-
        compare('<',H1,H2),
        !,
        compare_two_lists(L1,[H2|L2],R1,R2).
compare_two_lists([H1|L1],[H2|L2],R1,[H2|R2]):-
        % compare('>',H1,H2),  -- implicit
        !,
        compare_two_lists([H1|L1],L2,R1,R2).

%% tuple_to_list(+Tup,?List)
% converts tuples of form (X,Y) to list
tuple_to_list((G,Tup),[G|Gs]):-
        !,
        tuple_to_list(Tup,Gs).
tuple_to_list(G,[G]).

%% list_to_tuple(+List,?Tup)
% see also tuple_to_list/1  
list_to_tuple([G],G):- !.
list_to_tuple([G|Gs],(G,Tup)):-
        list_to_tuple(Gs,Tup).

%% iterate_over_input(+Prompt,+Template,+Goal)
% as iterate_over_input/3, uses current_input/1
iterate_over_input(Prompt,Template,Goal):-
        current_input(IO),
        iterate_over_input(Prompt,Template,Goal,IO).
%% iterate_over_input(+Prompt,+Template,+Goal,+InStream)
% reads input from InStream, writing Prompt and transforming input
%
% =|iterate_over_input('Enter a name',Name,format('Hello ~w~n',[Name]))|=
iterate_over_input(Prompt,Template,Goal,IO):-
        repeat,
        format(Prompt),
        read_stream_to_codes(IO,Codes),
        (   Codes=end_of_file
        ->  !
        ;   atom_codes(Template,Codes),
            Goal,
            fail).

unground_list(N,[]):- N =<0,!.
unground_list(N,[_|L]):-
        N1 is N-1,
        unground_list(N1,L).

%% pred_to_unground_term(+PredSpec,?Term) is det
% translates a predicate specification into an unground term
% e.g. foo/2 ==> foo(_,_)
%
% PredSpec can be Mod:P/A or P/A
pred_to_unground_term(Mod:P/A,Mod:Term):-
        !,
        pred_to_unground_term(P/A,Term).
pred_to_unground_term(Pred/Arity,Term):-
        !,
        unground_list(Arity,L),
        Term =.. [Pred|L].
% passthru
pred_to_unground_term(Term,Term).

:- dynamic term_gensym_dyn/3.
%% term_gensym(+Term,+NS,?Sym)
% generates or fetches a unique ID for Term in namespace NS
% uses get_time/1 to ensure uniqueness
term_gensym(Term,NS,Sym):-
        nonvar(Sym),
        !,
        term_gensym_dyn(Term,NS,Sym).
term_gensym(Term,NS,Sym):-
        term_gensym_dyn(Term,NS,Sym),
        !.
term_gensym(Term,NS,Sym):-
        get_time(Time),
        sformat(Base,'~w~f',[NS,Time]),
        gensym(Base,Sym), % normal gensym as well to be safe
        assert(term_gensym_dyn(Term,NS,Sym)).


%% call_on_stream(+StreamTemplate,+Goal,?Atom) is nondet
% Goal should include StreamTemplate
% example: call_on_stream(S,sgml_write(S,Node,[]),XML)
call_on_stream(S,G,Atom):-
        tmp_file(s,File),
        open(File,write,S,[]),
        G,
        close(S),
        read_file_to_codes(File,Codes,[]),
        atom_codes(Atom,Codes).


%% dsetof(+Template,+Goal,-Set)
% deterministic setof. equiv to setof/3, except will not fail. Set unifies to empty list if no goals are satisfied
dsetof(X,Goal,Xs):-
        (   setof(X,Goal,Xs)
        ->  true
        ;   Xs=[]).

%% solutions(+Template,+Goal,-Set)
%   deterministic setof (will not fail). Goal is existentially quantified
solutions(X,Goal,Xs):-
        (   setof(X,Goal^Goal,Xs)
        ->  true
        ;   Xs=[]).


%% oneof(:Goal)
%   calls Goal once
oneof(X):- call(X),!.

%% call_det(:Goal)
%   calls Goal once, throws exception if Goal does not succeed
call_det(X):- call(X) -> true ; throw(det_pred_failed(X)).

:- module_transparent call_unique/1.
call_unique(X) :- setof(X,X,Xs),member(X,Xs).


%% sumof(+Template,+Goal,?Sum)
%   Sum is unified with the aggregate sum of all Templates satisfying Goal
sumof(X,Goal,Sum):-
        findall(X,Goal,Xs),
        sumlist(Xs,Sum).

%% setof_count(+Template,+Goal,?Count)
%   number of distinct solutions. transparent.
setof_count(Var,Goal,Count):-
        (setof(Var,Goal^Goal,Vars)
        ->  true
        ;   Vars=[]),
        length(Vars,Count).

:- module_transparent user:count_distinct/2.
:- arithmetic_function(user:count_distinct/2).
user:count_distinct(Var,Goal,Count):-
        (   setof(Var,Goal^Goal,Vars)
        ->  true
        ;   Vars=[]),
        length(Vars,Count).


%% group_by(+Template,+Goal,?Groups)
%  partition all distinct solutions to Goal into a list of groups, each group is a compound term of the form Template-[Goal1, ...]
%  Example: =|group_by(Job,person_job(_,Job),JobToTermList)|=
group_by(Var,Goal,Groups):-
        setof(Var,Goal^Goal,Set),
        findall(Var-Group,(member(Var,Set),
                           findall(Goal,Goal,Group)),
                Groups).

%% group_by(+Template,+Goal,+GroupTemplate,?Groups)
%  partition all distinct solutions to Goal into a list of groups, each group is a compound term of the form Template-[GroupingElement1, ...]
%  Example: =|group_by(Job,person_job(Person,Job),Person,JobToTermList)|=
group_by(Var,Goal,GroupTemplate,Groups):-
        setof(Var,Goal^Goal,Set),
        findall(Var-Group,
                (   member(Var,Set),
                    findall(GroupTemplate,Goal,Group)),
                Groups).

%% count_by(+Template,+Goal,?Groups)
%   partition all distinct solutions to Goal into a list of groups, each group is a compound term of the form Template-Count. transparent.
count_by(Var,Goal,Counts):-
        solutions(Var,Goal^Goal,Set),
        findall(Var-Count,(member(Var,Set),
                           setof_count(Goal,Goal,Count)),
                Counts).

%% count_by(+GroupTemplate,+CountTemplate,+Goal,?CountTerms)
%   partition all distinct solutions to Goal into a list of groups, each group is a compound term of the form Template-Count. transparent.
count_by(Var,CountVar,Goal,Counts):-
        solutions(Var-CountVar,Goal^Goal,Set),
        solutions(Var,member(Var-_,Set),Groups),
        findall(Var-Count,(member(Var,Groups),
                           setof_count(CountVar,member(Var-CountVar,Set),Count)),
                Counts).

%% aggregate_by(+AggFunc,+Var,+Goal,?AggVal)
% DEPRECATED - use swi-prolog aggregate/4 instead
aggregate_by(with(AggFunc),Var,Goal,With):-
	!,
	count_by(Var,Goal,TCs),
	findall(Val,member(_-Val,TCs),Vals),
	aggregate(AggFunc,Vals,AggVal),
	member(With-AggVal,TCs), % choose arbitrary one
	!.
aggregate_by(AggFunc,Var,Goal,AggVal):-
	count_by(Var,Goal,TCs),
	findall(Val,member(_-Val,TCs),Vals),
	aggregate(AggFunc,Vals,AggVal).

%aggregate(max,L,X):- list_max(L,X).
%aggregate(min,L,X):- list_min(L,X).
%aggregate(avg,L,X):- list_avg(L,X).


% optimized version of above
user:goal_expansion(forall_distinct(Pre,Post),
                    (   setof(Pre,Pre^Pre,PreL)
                    ->  forall(member(Pre,PreL),Post)
                    ;   true
                    )).
forall_distinct(_,_):- fail.

%% forall_strict(+Cond,+Action)
%   all builtin predicate forall/2, throws an error if Action fails for any Cond
%
%  deprecated: consider using the mode module instead
forall_strict(Pre,Post):-
        forall(Pre,
               (   Post
               ->  true
               ;   throw(error(forall_strict(Pre,Post))))).

%% number_setof(+Template,+Goal,?NumberedList)
%  @param NumberedList
%  [1-Item1,2-Item2,...,N-ItemN]
%   as setof/3, numbers each solution as Number-Template
%  
number_setof(X,Goal,L):-
        dsetof(X,Goal,L1),
        number_list_items(1,L1,L).
number_list_items(L,L2):-
        number_list_items(1,L,L2).
number_list_items(_,[],[]).
number_list_items(N,[H|L],[N-H|L2]):-
        N2 is N+1,
        number_list_items(N2,L,L2).

%% findmax(+Template,+Val,+Goal,?BestTemplate)
%   as findmax/5
%  
%  @deprecated -  use aggregate/4 e.g. aggregate(max(X,Best),Goal,Val)
findmax(X,Val,Goal,BestX):-
        findmax(X,Val,Goal,BestX,_).
%% findmax(+Template,+Val,+Goal,?BestTemplate,?BestVal)
%   of all instantiations Template gets on backtracking over Goal, find the best, where the
%  best is defined by Val (which must be in Template)
% Example:
% =|findmax(Label,Len,(entity_label(E,Label),atom_length(Label,Len)),T,V).  % finds longest label|=
%
%  @deprecated -  use aggregate/4 e.g. aggregate(max(X,Best),Goal,Val)
findmax(X,Val,Goal,BestX,BestVal):-
        setof(Val-X,Goal^Goal,Pairs),
        sort(Pairs,PairsSorted),
        reverse(PairsSorted,[BestVal-BestX|_]).

%% fold_list(+Op, +Default, +List, -Result) is det
% apply Op between all the members of List to fold into
% a single value.
% Choose Default such that Op(Default, A, A).
% e.g., plus(0, A, A), or append([], A, A).
fold_list(_Op, Default, [], Default) :- !.
fold_list(_Op, _Default, [A], A) :- !.
fold_list(Op, Default, [A,B|C], D) :-
        call(Op, A, B, E),
        fold_list(Op, Default, [E|C], D).

%aggregate(Generator, Aggregator, Initial, Final) :-
% setof(Fact, call(Generator, Fact), Facts),
% fold_in(Aggregator, Facts, Initial, Final).

fold_in(_Aggregator, [], Final, Final).
fold_in(Aggregator, [F|Facts], Initial, Final) :-
 call(Aggregator, F, Initial, Intermediate),
 fold_in(Aggregator, Facts, Intermediate, Final).


dot_product([],[],0).
dot_product([A|AL],[B|BL],S) :-
        dot_product(AL,BL,S1),
        S is A*B+S1.

vector_magnitude(L,M) :-
        dot_product(L,L,S),
        M is sqrt(S).

% Since the angle, θ, is in the range of [0,π], the resulting similarity will yield the value of π as meaning exactly opposite, π / 2 meaning independent, 0 meaning exactly the same, with in-between values indicating intermediate similarities or dissimilarities.
cosine_similarity(A,B,S) :-
        dot_product(A,B,S1),
        vector_magnitude(A,MA),
        vector_magnitude(B,MB),
        S is acos(S1 / (MA * MB)).

% This cosine similarity metric may be extended such that it yields the Jaccard coefficient in the case of binary attributes. This is the Tanimoto coefficient, T(A,B), represented as
tanimoto_coefficient(A,B,T) :-
        dot_product(A,B,S1),
        vector_magnitude(A,MA),
        vector_magnitude(B,MB),
        T is S1 / (MA*MA + MB*MB - S1).

% use popcount instead!
%bitcount(Num,Count) :-
%        bitcount(Num,0,Count).
        
%bitcount(0,Count,Count) :- !.
%bitcount(Num,InCount,RetCount) :-
%        HasBit is Num /\ 1,
%        NumDiv is Num >> 1,
%        (   HasBit=1
%        ->  Count2 is InCount+1,
%            bitcount(NumDiv,Count2,RetCount)
%        ;   bitcount(NumDiv,InCount,RetCount)).



% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(findmax,
            [],
            (   ensure_loaded(bio(bioprolog_util)),
                findmax(X,V,(member(X-V,[abc-1,def-2,ghi-3])),X1,V1)),
            (X1=ghi,V1=3))).


/** <module> General purpose misc utility predicates
  
  ---+ Synopsis

  ==
  :- use_module(bio(bioprolog_util)).
  ==

  ---+ Description

  general hodge-podge of utils
  
  ---++ TODO

  Some of these predicates should move to more specific modules

  @author Chris Mungall
  @version  $Revision: 1.35 $
  @license LGPL

  
**/
