/* -*- Mode: Prolog -*- */



:- module(query_mediator,
          [order_conjunction/3,
           order_conjunction/4,
           order_goals/3,
           order_goals/4]).
:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util),[list_to_tuple/2,tuple_to_list/2]).

%rewrite_conjunction(Conj,ConjQueryList,Sources):-
%        tuple_to_list(Conj,Goals).

%query_graph(Conj,Graph):-
%        tuple_to_list(Conj,Goals),
%        x.

%% order_conj(+Conj,?OrderedConj,?NegScore)
%   see order_conj/4

:- mode order_conjunction(+,?,?) is nondet.
order_conjunction(Conj,ConjNew,Score):-
        tuple_to_list(Conj,Goals),
        order_goals(Goals,GoalsNew,Score),
        list_to_tuple(GoalsNew,ConjNew).
%% order_conj(+Conj,?OrderedConj,?NegScore,+Stack)
%  @param Conj
%  a tuple of conjoined goals; eg (G1,G2,G3,...,Gn)
%  @param OrderedConj
%  the same tuple reordered
%  @param NegScore
%  inv score of this ordering
%  @param Stack
%  = stack(+VarList,+PrevGoalList)
%  
%   assuming Conj is a conjunction tuple;
%  reorder them in a way that is judged to be most efficient.
%
%  
%  * nonvar arguments in goals will prioritise that goal
%  * var arguments that succeed a goal in which that var is unified is deemed very good
%  * var arguments in which this is the first occurrence of that var are deemed bad
%  
%
%  this will succeed multiple times for equally scoring orderings
%
%  low scores are deemed good
%  
order_conjunction(Conj,ConjNew,Score,Stack):-
        tuple_to_list(Conj,Goals),
        order_goals(Goals,GoalsNew,Score,Stack),
        list_to_tuple(GoalsNew,ConjNew).

%% order_goals(+Goals,?OrderedGoals,?NegScore)
%   assuming Goals is a list of goals to be ANDed, this will
%  reorder them in a way that is judged to be most efficient.
%
%  see order_conjunction/3
%  
:- mode order_goals(+,?,?) is nondet.
order_goals(Goals,GoalsNew,Score):-
        order_goals(Goals,GoalsNew,Score,stack([],[])).

:- mode order_goals(+,?,?,+) is nondet.
order_goals([],[],0,_).
order_goals(Goals,[G|GoalsNew],TotalScore,Stack):-
        Stack=stack(_UnifiedVars,GoalsPrev),
        pick_a_goal(Goals,G,GoalsRest,Score,Stack,UnifiedVarsNew),
        StackNew=stack(UnifiedVarsNew,[G|GoalsPrev]),
        order_goals(GoalsRest,GoalsNew,NewScore,StackNew),
        TotalScore is Score+NewScore.

:- mode pick_a_goal(+,?,?,?,+,?) is nondet.
pick_a_goal(Goals,G,GoalsNew,Score,Stack,UnifiedVarsNew):-
        Stack=stack(UnifiedVars,_GoalsPrev),
        negscore_goals(Goals,NegscoredGoals,Stack),
        %sort(NegscoredGoals,[_-G|_]), % det
        sort(NegscoredGoals,[Score-_|_]), % det
        member(Score-G,NegscoredGoals), % nondet
        selectequiv(G,Goals,GoalsNew),
        goal_vars(G,Vars),
        append(Vars,UnifiedVars,UnifiedVarsNew).

:- mode negscore_goals(+,?,+) is det.
negscore_goals(Goals,NegscoredGoals,Stack):-
        maplist(negscore_goal(Stack),Goals,NegscoredGoals).

:- mode negscore_goals(+,+,?) is det.
% disjunction
negscore_goal(_Stack,G,S-G):-
        G=(G1;G2),
        !,
        % todo - unified vars
        order_conjunction(G1,_,S1),
        order_conjunction(G2,_,S2),
        S is max(S1,S2)+1.        % max is worst; add extra penalty
% meta_goal -- nothing is unified after calling a meta goal
negscore_goal(Stack,G,STotal-G):-
        Stack=stack(UnifiedVars,_GoalsPrev),
        meta_goal(G,SubGoalTuple), % eg forall(Cond,Act)
        !,
        tuple_to_list(SubGoalTuple,SubGoals),
        % findall does not copy - but that's fine for meta_goals
        % [should we find the BEST here?]
        findall(S,(member(SubG,SubGoals),negscore_goal(Stack,SubG,S-_)),Scores),
        writeln(meta=Scores),
        sumlist(Scores,STotal).
% ordinary goal
negscore_goal(Stack,G,SFinal-G):-
        Stack=stack(UnifiedVars,GoalsPrev),
        G=..[_|Args],
        negscore_args(Args,Stack,0,S),
        % try and ensure similar dispatch methods follow each other
        (   GoalsPrev=[PrevGoal|_],
            goal_dispatch_method(G,Meth1),
            goal_dispatch_method(PrevGoal,Meth2),
            Meth1\=Meth2
        ->  SFinal is S+5       % penalty. how do we set
        ;   SFinal=S).

:- mode goal_vars(+,?) is det.
goal_vars((G1;G2),Vars):-
        !,
        goal_vars(G1,Vars1),
        goal_vars(G2,Vars2),
        % TODO: intersection 1 and 2 (worst case)
        % for now just assume nothing is unified;
        % as a general rule disjunctions should come at the end..
        % hmm what about [(N=a;N=b),class(ID,N)]
        %collectall(V,(memberequiv(V,Vars1),memberequiv(V,Vars2)),Vars).
        (   memberequiv(V,Vars1),
            memberequiv(V,Vars2)
        ->  Vars=[V]            % just take one for now
        ;   Vars=[]).
goal_vars(G,Vars):-
        G=..[_|Args],
        filter_vars(Args,Vars).

:- mode filter_vars(+,?) is det.
filter_vars([],[]).
filter_vars([V|Args],[V|Vars]):-
        var(V),
        !,
        filter_vars(Args,Vars).
filter_vars([_|Args],Vars):-
        filter_vars(Args,Vars).

%  low score is good (for sort purposes)
%
%  we give particularly good scores if a variable occurs previously
%  in the reordered conj chain. this helps avoid repeated pointless calculations
%
%  in the following:
%  isa(X,Y),foo(W,5),bar(Y,Z)
%  isa(X,Y),bar(Y,Z),foo(W,5)
%  the second ordering is to be favored. in the first foo/2 will be calculated
%  multiple times. bar/2 gains priority because Y is unified immediately before
%  (TODO: should we give priority to IMMEDIATELY before?)
:- mode negscore_args(+,+,+,?) is det.
negscore_args([],_,S,S).
negscore_args([A|Args],Stack,S,SFinal):-
        Stack=stack(UnifiedVars,_GoalsPrev),
        (   var(A)
        ->  (   memberequiv(A,UnifiedVars)
            ->  SNew is S-5     % already unified, v good
            ;   SNew is S+2)    % unbound var, v bad
        ;   SNew is S-1),       % nonvar, good
        negscore_args(Args,Stack,SNew,SFinal).

:- mode memberequiv(?,+) is nondet.
memberequiv(E,[Next|_]):-
        E==Next.
memberequiv(E,[_|L]):-
        memberequiv(E,L).

memberequiv(E,[Next|_],Next):-
        E==Next.
memberequiv(E,[_|L],X):-
        memberequiv(E,L,X).

%% selectequiv(?E,+L,?R)
%   as builtin select/3, but does equiv == check
:- mode selectequiv(?,+,?) is nondet.
selectequiv(E,L,R):-
        selectequiv(E,L,R,[]).
:- mode selectequiv(?,+,?,+) is nondet.
selectequiv(E,[Next|Tail],Remaining,Prev):-
        E==Next,
        append(Prev,Tail,Remaining).
selectequiv(E,[Next|L],R,Prev):-
        selectequiv(E,L,R,[Next|Prev]).

collectall(X,G,L):-
        collectall(X,G,L,[]).

collectall(X,G,[X|L],Tried):-
        G,
        \+ memberequiv(X,Tried),
        !,
        collectall(X,G,L,[X|Tried]).
collectall(_,_,[],_).
        
meta_goal(forall(Cond,_),Cond).

% different dispatch methods should be grouped together;
% this will allow bundling of eg all SQL together
goal_dispatch_method(G,M):-
        functor(G,F,A),
        predicate_dispatch_method(F/A,M),
        !.
goal_dispatch_method(_,native).

% test
predicate_dispatch_method(inst/2,sql).
predicate_dispatch_method(inst/3,sql).
predicate_dispatch_method(_,_):- fail.


/* OLD */

:- mode goals_edges(+,?) is det.
goals_edges(Goals,EdgesNew):-
        select(G1,Goals,GoalsRest),
        findall(E,goal_edge_in_goals(G1,GoalsRest,E),Edges1),
        goals_edges(GoalsRest,Edges),
        append(Edges1,Edges,EdgesNew).

:- mode goal_edge_in_goals(+,+,?) is nondet.
goal_edge_in_goals(G1,Goals,E):-
        member(G2,Goals),
        goal_pair_edge(G1,G2,E).

:- mode goal_pair_edge(+,+,?) is det.
goal_pair_edge(G1,G2,E):-
        G1=..[_|Args1],
        G2=..[_|Args2],
        member(Arg1,Args1),
        member(Arg2,Args2),
        \+ground(Arg1),
        \+ground(Arg2),
        Arg1==Arg2,
        E=edge(G1,G2),
        !.                      % we don't care WHICH args unify
        
% declarative version
instX(I,U):- inst(I,U1),isaX(U1,U).
instX(I,U):-
        genus(U,G),
        isaX(U1,G),
        inst(I,U1),
        forall(diff(U,R,D),
               (inst_rel(I,R1,X1),subrelX(R1,R),instX(X1,D))).

% rewritten, for SQL
instX(I,U):-
        var(I),
        q(U1,(genus(U,G),
              isaX(U1,G)),U1s), % collect all for efficiency in SQL
        qdb(I,inst(I,U1)/U1-U1s),   % count must be >0
        forall(diff(U,R,D),
               (inst_rel(I,R1,X1),subrelX(R1,R),instX(X1,D))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

   optimisation algorithm:

   conjunctions only; others nested

   starting from head, follow all paths through graph

   edge exists between two arguments in a goal with the same argument
   \+ground(X),\+ground(Y),X==Y

   [this is for any call and can be stored once for each query]

   throw an error if isolated subgraphs

   each path is evaluated FOR THE GIVEN ARGUMENTS

   step 2: chunk machines. some goals (eg externally called eg sql)
   prefer to receive all arguments at once. these are collector edges

   db calls best at start or end but not inbetween

   score accordingly

   special cases:
   forall - links in but no edges out
   
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

t(1):-
        forall((instX(I,gene),inst_rel(I,has_func,Fi),instX(Fi,tfactor)),
               (inst(Fi,Fu),label(I,Lab),format('~w ~w ~w',[I,Lab,Fu]))).

t(1.1):-
        query((inst(Fi,Fu),label(I,Lab)),
              (instX(I,gene),inst_rel(I,has_func,Fi),instX(Fi,tfactor))),
        format('~w ~w ~w',[I,Lab,Fu]),
        fail.

t(2.1):-
        query((inst(Fi,Fu),label(U,ULab)),
              (instX(g1,U),inst_rel(g1,has_func,Fi),instX(Fi,tfactor))),
        format('~w ~w ~w',[Fi,ULab,Fu]),
        fail.

entailq(instX(I,U),
        (   genus(U,G),
            isaX(U1,G),
            inst(I,U1),
            forall(diff(U,R,D),
                   (inst_rel(I,R1,X1),subrelX(R1,R),instX(X1,D))))).

testq(P,G):- P=instX(i,U),entailq(P,G).
testq(P,G):- P=instX(I,w),entailq(P,G).
testq(P,G):- P=instX(i,w),entailq(P,G).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(query_mediator)),
                forall(query_mediator:testq(Proj,Goal),
                       (   writeln(trying((Proj :- Goal))),
                           order_conjunction(Goal,GoalNew,S,stack([],[])),
                           writeln(S-GoalNew),
                           nl))),
            true)).


/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
  ---++ query_mediator
- 

  ---+ Synopsis

  ==
  :- use_module(bio(query_mediator)).

  ==

  ---+ Description

**/