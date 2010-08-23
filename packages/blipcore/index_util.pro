/* -*- Mode: Prolog -*- */

:- module(index_util,[
		      materialize_index/1,
		      materialize_index/2,
		      materialize_indexes_to_file/2,
		      materialize_index_to_stream/2,
		      materialize_goal_to_file/2,
		      materialize_goals_to_file/2
		     ]).

:- module_transparent materialize_index/1.
:- module_transparent materialize_index/2.
:- module_transparent materialize_indexes_to_file/2.
:- module_transparent materialize_index_to_path/2.
:- module_transparent materialize_indexes_to_path/2.

:- dynamic is_indexed/2.

%% materialize_index(+Term) is det
% materialize and index a set of facts, using first-argument indexing.
% Term follows the same structure as in index/1. E.g.
% ==
% materialize_index(my_fact(1,0,1,1))
% ==
% assumes that my_fact(?,?,?,?) can be exhaustively enumerated.
materialize_index(Mod:Term) :-
	!,
	materialize_index(Mod, Term).
materialize_index(Term) :-
	context_module(M),
	!,
	materialize_index(M, Term).
materialize_index(M, Term) :-
        index_util:is_indexed(M,Term),
	debug(index, '  already indexed ~w:~w', [M, Term]),
        !.
materialize_index(M, Term) :-
	debug(index, 'indexing ~w:~w', [M, Term]),
        !,
	Term=..[CalledPred, _|IxArgsRest],
	IxArgs=[1|IxArgsRest],  % always index the first argument - this is the default index
	length(IxArgs, Arity),
	predicate_ixname(CalledPred, StoredPred, 1),
	functor(CalledGoal, CalledPred, Arity),
	CalledGoal=..[CalledPred|Args],
	StoredGoal=..[StoredPred|Args],
	debug(index, 'rewriting ~w', [CalledGoal]),
	rewrite_goal_with_index_list(M, CalledGoal, 1, IxArgs),
	DefaultGoal = ( CalledGoal :- StoredGoal ),
	M:assert(DefaultGoal),
	M:compile_predicates([CalledPred/Arity]),
        assert(index_util:is_indexed(M,Term)),
	debug(index, 'done indexing ~w:~w', [M, Term]).


materialize_indexes_to_path(Terms,Dir) :-
	forall(member(Term,Terms),
	       materialize_index_to_path(Term,Dir)).

%% materialize_index_to_path(+Term,+Dir)
%
% caches 
%
% Dir is expanded using absolute_file_name/2
materialize_index_to_path(Term,Dir) :-
	Term=..[P|_],
	atom_concat(P,'.pl',FileName),
	absolute_file_name(Dir,DirAbs),
	absolute_file_name(DirAbs/FileName,AbsFilePath),
	materialize_indexes_to_file([Term],AbsFilePath).

%% materialize_indexes_to_file(+Terms:list,+File) is det
%
% if File does not exist, materializes a set of indexes
% and caches results in File.
% if File does exist, consults File and materializes.
%
% note that this does not write the full index to file;
% just the main goal. When an index is re-used, the materialization
% step is still called. Thus there is only a benefit to
% using this when Terms contains intensional predicates.
materialize_indexes_to_file(Terms,File) :-
	exists_file(File),
	!,
        debug(index, 'using existing index file: ~w', [File]),
	% clear existing intensional predicate
	forall(member(Term,Terms),
	       (   functor(Term,Pred,Arity),
		   abolish(Pred/Arity))),
	% load cached version:
	consult(File),
	forall(member(Term,Terms),
	       materialize_index(Term)).

materialize_indexes_to_file(Terms,File) :-
	open(File,write,IO),
	forall(member(Term,Terms),
	       materialize_index_to_stream(Term,IO)),
	close(IO).

:- module_transparent materialize_index_to_stream/2.
materialize_index_to_stream(M:Term,IO) :-
        !,
	materialize_index(M:Term),
	functor(Term,Pred,Arity),
	functor(Goal,Pred,Arity),
	forall(M:Goal,
	       format(IO,'~q.~n',[M:Goal])).
materialize_index_to_stream(Term,IO) :-
	materialize_index(Term),
	functor(Term,Pred,Arity),
	functor(Goal,Pred,Arity),
	forall(Goal,
	       format(IO,'~q.~n',[Goal])).

	



%% rewrite_goal_with_index_list(+Mod, +CalledGoal, +ArgNum:int, +IxArgs:list) is det
rewrite_goal_with_index_list(_, _, _, []).
rewrite_goal_with_index_list(M, CalledGoal, Ix, [A|Args]) :-
	rewrite_goal_with_index(M, CalledGoal, Ix, A),
	Ix2 is Ix+1,
	rewrite_goal_with_index_list(M, CalledGoal, Ix2, Args).

%% rewrite_goal_with_index_list(+Mod, +CalledGoal, +ArgNum:int, +IndexMe:int) is det
rewrite_goal_with_index(M, CalledGoal, Ix, 1) :-
	debug(index, '  index ~w', [Ix]),
        !,
	CalledGoal=..[CalledPred|Args],
	length(Args,Arity),
	predicate_storedname(CalledPred, StoredPred, Ix),
	debug(index, '  using ~w in ~w', [StoredPred/Arity,M]),
	reorder_args(Ix, IxVar, Args, ReorderedArgs),
	StoredGoal=..[StoredPred|Args],
	predicate_ixname(CalledPred, IxPred, Ix),
	IxGoal=..[IxPred|ReorderedArgs],
	forall(M:StoredGoal, M:assert(IxGoal)), % todo: uniquify?
	M:compile_predicates([IxPred/Arity]),
	(   Ix=1
	->  M:abolish(CalledPred/Arity)
	;   true),
	RewrittenGoal = ( CalledGoal :- nonvar(IxVar), !, IxGoal),
	M:assert(RewrittenGoal).
rewrite_goal_with_index(_, _, _, _). % no index

%% reorder_args(+Ix:int, ?IxVar, +Args1:list, +Args2:list) is det
reorder_args(Ix, IxVar, Args1, Args2) :-
	append(L1, [IxVar|L2], Args1),
	length(L1, Len),
	Len is Ix-1,
	!,
	append([IxVar|L1], L2, Args2).


predicate_storedname(N, N, 1) :- !. % use original predicate as source for first index
predicate_storedname(N1, N2, _) :-  % use first index as source for subsequent indexes
	!,
        predicate_ixname(N1, N2, 1).

predicate_ixname(N1, N2, Ix) :-
        concat_atom([N1, '_ix__', Ix], N2).


%% materialize_goal_to_file(+G:term,F) is det
materialize_goal_to_file(G,F) :-
	open(F,write,IO,[]),
	forall(G,format(IO,'~q~n.',[G])),
	close(IO).


%% materialize_goals_to_file(+Goals:list,F) is det
materialize_goals_to_file(Gs,F) :-
	open(F,write,IO,[]),
	forall(member(G,Gs),
	       forall(G,format(IO,'~q~n.',[G]))),
	close(IO).



/** <module> materializes indexes that exploit first-argument indexing

---+ Synopsis

==
use_module(bio(index_util)).

% assume a database of my_fact/4 
materialize_index(my_fact(1,1,0,1)).
==

---+ Details

This is designed to be a swap-in replacement for index/1. Indexing a
fact with M arguments on N of those arguments will generate N sets of
facts with arguments reordered to take advantage of first-argument
indexing. The original fact will be rewritten.

  For example, calling:

==
materialize_index(my_fact(1,0,1)). 
==

  will retract all my_fact/3 facts and generate the following clauses in its place:

==
my_fact(A,B,C) :-
    nonvar(A),
    !,
    my_fact__ix_1(A,B,C).
my_fact(A,B,C) :-
    nonvar(C),
    !,
    my_fact__ix_3(C,A,B).
my_fact(A,B,C) :-
    my_fact__ix_1(A,N,C).
==

here my_fact__ix_1 and my_fact__ix_3 contain the same data as the original my_fact/3 clause. In the second case, the arguments have been reordered
 
---+ Limitations

Single key indexing only. Could be extended for multikeys.
  
Reindexing is not a good idea. It could be smarter about this.

Should not be used on dynamic databases. 

Does not have to be used with fact (unit clauses) - but the clauses should enumerable

  
*/
