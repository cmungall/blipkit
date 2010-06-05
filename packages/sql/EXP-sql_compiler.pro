/* -*- Mode: Prolog -*- */

%% <module> Maps prolog terms to SQL
% ------------------------------------------------------------------------
%
% This Prolog to SQL compiler may be distributed free of charge provided that it is
% not used in commercial applications without written consent of the author, and
% that the copyright notice remains unchanged.
%
%                    (C) Copyright by Christoph Draxler, Munich
%                        Version 1.1 of Dec. 21st 1992
%
% I would like to keep in my hands the further development and distribution of the
% compiler. This does not mean that I don't want other people to suggest or even
% implement improvements - quite on the contrary: I greatly appreciate contributions 
% and if they make sense to me I will incorporate them into the compiler (with due
% credits given!). 
% 
% For further development of the compiler, address your requests, comments and
% criticism to the author:
%
%                    Christoph Draxler
%                    CIS Centre for Information and Speech Processing
%                    Ludwig-Maximilians-University Munich
%                    Wagmuellerstr. 23 
%                    D 80538 Munich
%                    Tel : ++49 / +89 / 211 06 64 (-60)
%                    Fax : ++49 / +89 / 211 06 74
%                    Mail: draxler@cis.uni-muenchen.de
%
%
% A report describing the implementation is available upon request from the
% author. 
%
%
% RELEASE INFORMATION
% ===================
% Current version is v. 1.1 of Dec. 21st 1992.
% Version 1.0 Sept. 3 1992
% CJM mods 2005
% ------------------------------------------------------------------------

:- module(sql_compiler,
          [
           plterm_to_sqlterm/3,
           print_sqlterm/1,
           sqlterm2atom/2,
           load_schema_defs/1,
           get_type/2,
           rewrite_query/2,
           op(1150,xfy,(<-))
          ]).

:- use_module(bio(mode)). % CJM

:- multifile relation/2,relation/3,attribute/4,unique/2.
:- discontiguous relation/2,relation/3,attribute/4,unique/2.

% CJM: required by SWI-Prolog
:- op(900,fy,not).
:- op(1150,xfy,(<-)).
%:- op(800,xfy,in).

:- discontiguous view/2,sql_expand/1.
:- multifile view/2,sql_expand/1.

% CJM:
% sqlschema_connection(?Schema,?Rdb)
% globals for database handles
:- multifile sqlschema_connection/2.
:- dynamic sqlschema_connection/2.

% Views: declared analagous to clausesm with <- not :-
% rewrite to a view/2 fact
system:term_expansion( (Mod:Head <- Body),
                     [   sql_compiler:view(Head,Body),
                         (   Mod:Head :- getrdb(Rdb),rdb_query(Rdb,Head,Head))]):- !.
system:term_expansion( (Head <- Body),
                     sql_compiler:view(Head,Body)).

load_schema_defs(File):-
        consult(File).



%% plterm_to_sqlterm(+ProjectionTerm,+DatabaseGoal,?SQLQueryTerm)
:- module_transparent(plterm_to_sqlterm/3).
plterm_to_sqlterm(ProjectionTerm,G,SQLQueryTerm):-
        debug(sql_compiler,'goal ~w',G),
        rewrite_query(G,G2),
        debug(sql_compiler,'rewritten_as: ~w',G2),
        optimize_query_all(G2,G3),
        debug(sql_compiler,'optimized as ~w',G3),
        translate(ProjectionTerm,G3,SQLQueryTerm).

plterm_allterms((G1,G2),GL):-
        !,
        plterm_allterms(G1,G1L),
        plterm_allterms(G2,G2L),
        append(G1L,G2L,GL).
plterm_allterms((G1;G2),GL):-
        !,
        plterm_allterms(G1,G1L),
        plterm_allterms(G2,G2L),
        append(G1L,G2L,GL).
plterm_allterms(not(G),[G|GL]):-
        !,
        plterm_allterms(G,GL).
plterm_allterms(G,[G]).

optimize_query_vars(G):-
        plterm_allterms(G,GL),
        length(GL,Num),
        numlist(1,Num,NumList),
        findall(X-Y,(member(X,NumList),member(Y,NumList)),IndexPairs),
        unify_pairs(IndexPairs,GL).

unify_pairs([],_).
unify_pairs([X-Y|NL],GL):-
        nth1(X,GL,G1),
        nth1(Y,GL,G2),
        (   G1\==G2,
            unify_plterm1(G1,G2)
        ->  true
        ;   true),
        unify_pairs(NL,GL).

%% optimize_query_all(+G,?G2)
% [CJM] - added to account for the fact that multiple iterations over optimize_query/2 are required:
% eg merging two referenced terms affects terms referencing these as FKs.
% see for example schema_go, we may merge two dbxrefds after attempting to merge gene_product
% it is only after the dbxrefd terms are merged that the dbxref_id FK var is unified
optimize_query_all(G,G2):-
        optimize_query_vars(G),
        optimize_query(G,G1),
        (   G == G1             % identical if optimizing yielded nothing
        ->  G2=G                % base case
        ;   optimize_query_all(G1,G2)). % keep on optimizing

% if two terms can be proved to unify to the same row, merge them
% unifying vars
:- mode optimize_query(+,?) is det.
optimize_query((G,Gs),Go):-
        unify_plterm(G,Gs),
        !,
        optimize_query(Gs,Go).
optimize_query((G,Gs),Gm):-
        !,
        optimize_query(Gs,Gs1),
        merge_goals(G,Gs1,Gm).
optimize_query(not(G),not(G2)):-
        !,
        optimize_query(G,G2).
optimize_query(X is T,X is T2):-
        T=..[F,V,G],
        aggregate_functor(F,_),
        !,
        optimize_query(G,G2),
        T2=..[F,V,G2].
optimize_query(X,X).

%% unify_plterm(+TermToUnify,?Term) is semidet.
% example:
%  unify_plterm(person(A,B),(person(C,D),person(A,C))) ==> B=C (if arg1 of person/2 is unique)
:- mode unify_plterm(+,?) is semidet.
unify_plterm(G,(G2,_)):-
        unify_plterm1(G,G2),    % no need to recurse further
        !.
unify_plterm(G,(_,Gs)):-
        !,
        unify_plterm(G,Gs).
unify_plterm(G,not(Gs)):-
        !,
        unify_plterm(G,Gs).
unify_plterm(G,G2):-
        unify_plterm1(G,G2),
        !.

unify_plterm1(G,G2):-
        functor(G,F,Arity),
        functor(G2,F,Arity),
        unique(F,Attr),
        attribute(AttrOrd,F,Attr,_),
        arg(AttrOrd,G,V1),
        arg(AttrOrd,G2,V2),
        V1 == V2,               % unique key is equivalent
        !,                      % only succeed once
        debug(sql_compiler,'unifying based on unique key: ~w = ~w',[G,G2]),
        G=G2.                   % unify if equivalent


% ========================================
% views
% ========================================

:- module_transparent rewrite_query/2.
%% rewrite_query(+GoalIn,?GoalOut) is det
rewrite_query((G,Gs),Gm):-
        !,
        rewrite_query(G,G1),
        rewrite_query(Gs,Gs1),
        merge_goals(G1,Gs1,Gm).
rewrite_query((G;Gs),(G1;Gs1)):-
        !,
        rewrite_query(G,G1),
        rewrite_query(Gs,Gs1).
rewrite_query(G,G2):-
        view(G,Gz),
        rewrite_query(Gz,G2),
        !.
rewrite_query(_:G,G2):- % ignore module
        view(G,Gz),
        rewrite_query(Gz,G2),
        !.
rewrite_query(X is G,X is G2):-
        !,
        rewrite_query(G,G2).
rewrite_query(G,G2):-
        G=..[F,V,X],
        aggregate_functor(F,_), % what to do about free variables in rewritten term?
        !,
        rewrite_query(X,X2),
        G2=..[F,V,X2].
rewrite_query(not(G),not(G2)):-
        !,
        rewrite_query(G,G2).
rewrite_query(H,G2):- % treat prolog predicates as views
        %guitracer,
        %trace,
        context_module(Mod),
        debug(sql_compiler,'  ?is_clause: ~w in module: ~w',[H,Mod]),
        setof(B,clause(H,B),Bs), % prolog predicate
        debug(sql_compiler,'  clause: ~w ',[Bs]),
        !,
        list_to_disj(Bs,BDisj),
        % TODO: check for recursive predicates
        debug(sql_compiler,'rewrite: ~w => ~w',[H,BDisj]),
        rewrite_query(BDisj,G2).
rewrite_query(G,G).

merge_goals((H,T),G,(H,G2)):-
        !,
        merge_goals(T,G,G2).
merge_goals(G1,G2,(G1,G2)).

goal_to_list((H,T),[H2|T2]):-
        !,
        goal_to_list(H,H2),
        goal_to_list(T,T2).
goal_to_list(X,[X]).

% ------------------------------------------------------------------------
%
% Top level predicate translate/3 organizes the compilation and constructs a
% Prolog term representation of the SQL query. 
%
% ------------------------------------------------------------------------


:- mode translate(+,+,?) is det.
translate(ProjectionTerm,DatabaseGoal,SQLQueryTerm):-
   % --- initialize variable identifiers and range variables for relations -----
   init_gensym(var),
   init_gensym(rel),

   % --- tokenize projection term and database goal ------------------------
   tokenize_term(DatabaseGoal,TokenDatabaseGoal),
   tokenize_term(ProjectionTerm,TokenProjectionTerm),

   % --- lexical analysis: reordering of goals for disjunctive normalized form -
   disjunction(TokenDatabaseGoal,Disjunction),
   debug(sql_compiler,'disj ~w',[Disjunction]),
   
   % --- code generation -----------------------------------------------------
   query_generation(Disjunction,TokenProjectionTerm,SQLQueryTerm).





%% disjunction(Goal,Disjunction) 
%
% turns original goal into disjunctive normalized form by computing all conjunctions
% and collecting them in a list
%
% ------------------------------------------------------------------------

:- mode disjunction(+,?) is det.
disjunction(Goal,Disjunction):-
   findall(Conjunction,linearize(Goal,Conjunction),Disjunction).




%% linearize(Goal,ConjunctionList) 
%
% Returns a conjunction of base goals for a complex disjunctive or conjunctive goal
% Yields several solutions upon backtracking for disjunctive goals
%
% ------------------------------------------------------------------------

:- mode linearize(+,?) is nondet.
linearize(((A,B),C),(LinA,(LinB,LinC))):-
   % --- transform left-linear to right-linear conjunction (',' is associative) ---
   linearize(A,LinA),
   linearize(B,LinB),
   linearize(C,LinC).

linearize((A,B),(LinA,LinB)):-
   A \= (_,_),
   % --- make sure A is not a conjunction -----------------------------------
   linearize(A,LinA),
   linearize(B,LinB).

linearize((A;_B),LinA):-
   linearize(A,LinA).

linearize((_A;B),LinB):-
   linearize(B,LinB).

linearize(not A, not LinA):-
   linearize(A,LinA).

linearize(Var^A, Var^LinA):-
   linearize(A,LinA).

linearize(A,A):-
   A \= (_,_),
   A \= (_;_),
   A \= _^_,
   A \= not(_).




%% tokenize_term(Term,TokenizedTerm) 
%
% If Term is a 
%
%  - variable, then this variable is instantiated with a unique identifier 
%    of the form '$var$'(VarId), and TokenizedTerm is bound to the same 
%    term '$var$'(VarId). 
%
%  - constant, then TokenizedTerm is bound to '$const$'(Term).
%
%  - complex term, then the term is decomposed, its arguments are tokenized,
%    and TokenizedTerm is bound to the result of the composition of the original
%    functor and the tokenized arguments.
%
% ------------------------------------------------------------------------

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
   var(VarId),
   % --- uninstantiated variable: instantiate it with unique identifier.
   gensym(var,VarId).

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
   nonvar(VarId).

tokenize_term(Constant,'$const$'(Constant)):-
   nonvar(Constant),
   functor(Constant,_,0).

tokenize_term(Term,TokenizedTerm):-
   nonvar(Term),
   Term \= '$var$'(_),
   Term \= '$const$'(_),
   Term =.. [Functor|Arguments],
   Arguments \= [],
   tokenize_arguments(Arguments,TokenArguments),
   TokenizedTerm =.. [Functor|TokenArguments].



%% tokenize_arguments(Arguments,TokenizedArguments) 
%
% organizes tokenization of arguments by traversing list and calling tokenize_term
% for each element of the list.
%
% ------------------------------------------------------------------------

tokenize_arguments([],[]).

tokenize_arguments([FirstArg|RestArgs],[TokFirstArg|TokRestArgs]):-
   tokenize_term(FirstArg,TokFirstArg),
   tokenize_arguments(RestArgs,TokRestArgs).







%% query_generation(ListOfConjunctions, ProjectionTerm, ListOfQueries) 
%
% For each Conjunction translate the pair (ProjectionTerm,Conjunction) to an SQL query
% and connect each such query through a UNION-operator to result in the ListOfQueries.
%
% A Conjunction consists of positive or negative subgoals. Each subgoal is translated 
% as follows:
%  - the functor of a goal that is not a comparison operation is translated to
%    a relation name with a range variable
%  - negated goals are translated to NOT EXISTS-subqueries with * projection
%  - comparison operations are translated to comparison operations in the WHERE-clause
%  - aggregate function terms are translated to aggregate function (sub)queries
% 
% The arguments of a goal are translated as follows:
%  - variables of a goal are translated to qualified attributes
%  - variables occurring in several goals are translated to equality comparisons
%    (equi join) in the WHERE-clause
%  - constant arguments are translated to equality comparisons in the WHERE-clause
% 
% Special treatment of arithmetic functions:
%  - arithmetic functions are identified through the Prolog is/2 operator
%  - an arithmetic function may contain an unbound variable only on its left side
%  - the right side of the is/2 operator may consist of 
%    * bound variables (bound through occurrence within a positive database goal, or 
%      bound through preceeding arithmetic function), or of 
%    * constants (numbers, i.e. integers, reals)
% 
% The following RESTRICTION holds:
%
%  - the binding of variables follows Prolog: variables are bound by positive base goals
%    and on the left side of the is/2 predicate - comparison operations, negated goals
%    and right sides of the is/2 predicate do not return variable bindings and may even 
%    require all arguments to be bound for a safe evaluation.
%
% ------------------------------------------------------------------------

:- mode query_generation(+,+,?) is det.
query_generation([],_,[]).
query_generation([Conjunction|Conjunctions],ProjectionTerm,[Query|Queries]):-
        debug(sql_compiler,'conj ~w',[Conjunction]),
        debug(sql_compiler,'projterm ~w',[ProjectionTerm]),
        projection_term_variables(ProjectionTerm,InitDict),
        debug(sql_compiler,'InitDict ~w',[InitDict]),
        reorder_conjunction(Conjunction,ConjunctionOrdered), % CJM
        debug(sql_compiler,'Translating conjunction ~w',[ConjunctionOrdered]),
        translate_conjunction(ConjunctionOrdered,SQLFrom,SQLWhere,InitDict,Dict),
        debug(sql_compiler,'from=~w where=~w',[SQLFrom,SQLWhere]),
        debug(sql_compiler,'translating ~w',[ProjectionTerm]),
        translate_projection(ProjectionTerm,Dict,SQLSelect),
        Query = query(SQLSelect,SQLFrom,SQLWhere),
        debug(sql_compiler,'query= ~w',[Query]),
        query_generation(Conjunctions,ProjectionTerm,Queries).

/*
  reorder_conjunction(Conjunction,NewConjunction)

  make sure comparison operators do not precede relations
  */
:- mode reorder_conjunction(+,?) is det.
reorder_conjunction(C,CNew):-
        tuple_to_list(C,Gs),
        reorder_conjunction_list(Gs,Gs1,Gs2),
        append(Gs1,Gs2,GsNew),
        list_to_tuple(GsNew,CNew).

reorder_conjunction_list([],[],[]).
reorder_conjunction_list([G|Gs],Gs1,[G|Gs2]):-
        G =.. [CompOp,_,_],
        comparison(CompOp,_),
        %G = (_=_),              % put compara assignments at end
        !,
        reorder_conjunction_list(Gs,Gs1,Gs2).
reorder_conjunction_list([G|Gs],[G|Gs1],Gs2):-
        reorder_conjunction_list(Gs,Gs1,Gs2).

% DRY? TODO
tuple_to_list((G,Tup),[G|Gs]):-
        !,
        tuple_to_list(Tup,Gs).
tuple_to_list(G,[G]).

list_to_tuple([G],G):- !.
list_to_tuple([G|Gs],(G,Tup)):-
        list_to_tuple(Gs,Tup).

list_to_disj([G],G):- !.
list_to_disj([G|Gs],(G;Tup)):-
        list_to_disj(Gs,Tup).


%% translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict) 
%
% translates a
%
%   - positive database goal to the associated FROM- and WHERE clause of an SQL query
%   - a negated goal to a negated existential subquery
%   - an arithmetic goal to an arithmetic expression or an aggregate function query
%   - a comparison goal to a comparison expression
%   - a negated comparison goal to a comparison expression with the opposite comparison
%     operator
%
% ------------------------------------------------------------------------

:- mode translate_goal(+,?,?,+,?) is det.
translate_goal(SimpleGoal,[SQLFrom],SQLWhere,Dict,NewDict):-
   % --- positive goal binds variables - these bindings are held in the dictionary -----
   functor(SimpleGoal,Functor,Arity),
   translate_functor(Functor,Arity,SQLFrom),
   SimpleGoal =.. [Functor|Arguments],
   translate_arguments(Arguments,SQLFrom,1,SQLWhere,Dict,NewDict).

translate_goal(Result is Expression,[],SQLWhere,Dict,NewDict):-
        translate_arithmetic_function(Result,Expression,SQLWhere,Dict,NewDict).

% CJM : TODO
% e.g. for concat
translate_goal(eval(Result, Expression),[],SQLWhere,Dict,NewDict):-
        translate_builtin_function(Result,Expression,SQLWhere,Dict,NewDict).


%% negated goals do not bind variables - hence Dict is returned unchanged -------
translate_goal(not NegatedGoals,[],SQLNegatedSubquery,Dict,Dict):-
   functor(NegatedGoals,Functor,_),
   not comparison(Functor,_),
   translate_conjunction(NegatedGoals,SQLFrom,SQLWhere,Dict,_),
   SQLNegatedSubquery = [negated_existential_subquery([*],SQLFrom,SQLWhere)].

translate_goal(not ComparisonGoal,[],SQLCompOp,Dict,Dict):-
   % --- comparison operations do not bind variables - Dict is returned unchanged -----
   ComparisonGoal =.. [ComparisonOperator,LeftArg,RightArg],
   comparison(ComparisonOperator,SQLOperator),
   negated_comparison(SQLOperator,SQLNegOperator),
   translate_comparison(LeftArg,RightArg,SQLNegOperator,Dict,SQLCompOp).

translate_goal(ComparisonGoal,[],SQLCompOp,Dict,Dict):-
   % --- comparison operations do not bind variables - Dict is returned unchanged -----
   ComparisonGoal =.. [ComparisonOperator,LeftArg,RightArg],
   comparison(ComparisonOperator,SQLOperator),
   translate_comparison(LeftArg,RightArg,SQLOperator,Dict,SQLCompOp).

translate_goal(not_null(Arg),[],[not_null(Term)],Dict,Dict):- % CJM
        evaluable_expression(Arg,Dict,Term,_ArgType).

% expand prolog goals; module must be loaded to have effect
%translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict):-
%        Goal \= (_,_),
%        clause(Goal,Body),      % view
%        translate_goal(Body,SQLFrom,SQLWhere,Dict,NewDict).


translate_goal(Goal,_,_,Dict,_):-
        format(user_error,'Could not translate goal: ~w in :: ~w~n',[Goal,Dict]),
        fail. % TODO: throw?




%% translate_conjunction(Conjunction,SQLFrom,SQLWhere,Dict,NewDict) 
% 
% translates a conjunction of goals (represented as a list of goals preceeded by 
% existentially quantified variables) to FROM- and WHERE-clause of an SQL query.  
% A dictionary containing the associated SQL table and attribute names is built up
% as an accumulator pair (arguments Dict and NewDict)
%
% ------------------------------------------------------------------------

:- mode translate_conjunction(+,?,?,+,?) is det.
translate_conjunction('$var$'(VarId)^Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   % --- add info on existentially quantified variables to dictionary here -----------
   add_to_dictionary(VarId,_,_,_,existential,Dict,TmpDict),
   translate_conjunction(Goal,SQLFrom,SQLWhere,TmpDict,NewDict).

translate_conjunction(Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   Goal \= (_,_),
   debug(sql_compiler,'simple conj, translating goal ~w',[Goal]),
   translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict),
   debug(sql_compiler,'translated goal ~w',[Goal]).

translate_conjunction((Goal,Conjunction),SQLFrom,SQLWhere,Dict,NewDict):-
        debug(sql_compiler,'complex conj, translating goal ~w',[Goal]),
        translate_goal(Goal,FromBegin,WhereBegin,Dict,TmpDict),
        debug(sql_compiler,'OK ~w with ~w',[Goal,TmpDict]),
        debug(sql_compiler,'translating conjunction: ~w',[Conjunction]),
        translate_conjunction(Conjunction,FromRest,WhereRest,TmpDict,NewDict),
        debug(sql_compiler,'conj ~w',[Conjunction-FromRest-WhereRest]),
        append(FromBegin,FromRest,SQLFrom),
        append(WhereBegin,WhereRest,SQLWhere).





%% translate_arithmetic_function(Result,Expression,SQLWhere,Dict,NewDict) 
%
% Arithmetic functions (left side of is/2 operator is bound to value of expression on
% right side) may be called with either
%
% - Result unbound: then Result is bound to the value of the evaluation of Expression
% - Result bound: then an equality condition is returned between the value of Result
%   and the value of the evaluation of Expression.
%
% Only the equality test shows up in the WHERE clause of an SQLquery.
%
% ------------------------------------------------------------------------

translate_arithmetic_function('$var$'(VarId),Expression,[],Dict,NewDict):-
   % assigment of value of arithmetic expression to variable - does not
   % show up in WHERE-part, but expression corresponding to
   % variable must be stored in Dict for projection translation
   evaluable_expression(Expression,Dict,ArithExpression,Type),
   add_to_dictionary(VarId,is,ArithExpression,Type,all,Dict,NewDict).


translate_arithmetic_function('$var$'(VarId),Expression,ArithComparison,Dict,Dict):-
   % --- test whether left side evaluates to right side: return equality comparison ----
   % Left side consists of qualified attribute, i.e. range variable must not be
   % arithmetic operator is/2 

   lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
   not (PrevRangeVar = is),

   % test whether type of attribute is numeric - if not, there's no sense in 
   % continuing the translation

   type_compatible(PrevType,number),
   evaluable_expression(Expression,Dict,ArithExpression,ExprType),
   type_compatible(ExprType,number),
   ArithComparison = [comp(att(PrevRangeVar,PrevAtt),'=',ArithExpression)].


translate_arithmetic_function('$var$'(VarId),Expression,ArithComparison,Dict,Dict):-
   % --- test whether left side evaluates to right side: return equality comparison ----
   % Left side consists of arithmetic expression, i.e. VarId is stored in Dict as 
   % belonging to arithmetic expression which is expressed as RangeVar-argument 
   % of lookup returning is/2. Type information is implicit through the is/2 functor

   lookup(VarId,Dict,is,LeftExpr,Type),
   type_compatible(Type,number),
   evaluable_expression(Expression,Dict,RightExpr,ExprType),
   type_compatible(ExprType,number),
   ArithComparison = [comp(LeftExpr,'=',RightExpr)].


translate_arithmetic_function('$const$'(Constant),Expression,ArithComparison,Dict,Dict):-
   % --- is/2 used to test whether left side evaluates to right side ----------------
   get_type('$const$'(Constant),ConstantType),
   type_compatible(ConstantType,number),
   evaluable_expression(Expression,Dict,ArithExpression,ExprType),
   type_compatible(ExprType,number),
   ArithComparison = [comp('$const$'(Constant),'=',ArithExpression)].



%% translate_comparison(LeftArg,RightArg,CompOp,Dict,SQLComparison) 
%
% translates the left and right arguments of a comparison term into the
% appropriate comparison operation in SQL. The result type of each 
% argument expression is checked for type compatibility
%
% -----------------------------------------------------------------

:- mode translate_comparison(+,+,+,+,?) is det.
translate_comparison(LeftArg,RightArg,CompOp,Dict,Comparison):-
   evaluable_expression(LeftArg,Dict,LeftTerm,LeftArgType),
   evaluable_expression(RightArg,Dict,RightTerm,RightArgType),
   type_compatible(LeftArgType,RightArgType),
   Comparison = [comp(LeftTerm,CompOp,RightTerm)].







%% translate_functor(Functor,QualifiedTableName) 
%
% translate_functor searches for the matching relation table name for
% a given functor and creates a unique range variable to result in
% a unique qualified relation table name.
%
% ------------------------------------------------------------------------

translate_functor(Functor,Arity,rel(TableName,RangeVariable)):-
        relation(Functor,ActualArity,TableName),
        ActualArity >= Arity,   % we allow dropping of arguments [CJM]
        lgensym([rel,'_',TableName],RangeVariable).


%% translate_arguments(Arguments,RelTable,ArgPos,Conditions,Dict) 
%
% translate_arguments organizes the translation of term arguments. One
% term argument after the other is taken from the list of term arguments
% until the list is exhausted. 
%
% ------------------------------------------------------------------------

translate_arguments([],_,_,[],Dict,Dict).

translate_arguments([Arg|Args],SQLTable,Position,SQLWhere,Dict,NewDict):-
   translate_argument(Arg,SQLTable,Position,Where,Dict,TmpDict),
   NewPosition is Position + 1,
   translate_arguments(Args,SQLTable,NewPosition,RestWhere,TmpDict,NewDict),
   append(Where,RestWhere,SQLWhere).




%% translate_argument(Argument,RelTable,Position,Condition,Dict) 
%
% The first occurrence of a variable leads to its associated SQL attribute information
% to be recorded in the Dict. Any further occurrence creates an equi-join condition 
% between the current attribute and the previously recorded attribute.
% Constant arguments always translate to equality comparisons between an attribute and 
% the constant value.
%
% ------------------------------------------------------------------------

translate_argument('$var$'(VarId),rel(SQLTable,RangeVar),Position,[],Dict,NewDict):-
   attribute(Position,SQLTable,Attribute,Type),
   add_to_dictionary(VarId,RangeVar,Attribute,Type,all,Dict,NewDict).

translate_argument('$var$'(VarId),rel(SQLTable,RangeVar),Position,AttComparison,Dict,Dict):-
   % --- Variable occurred previously - retrieve first occurrence data from dictionary -
   lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
   attribute(Position,SQLTable,Attribute,Type),
   type_compatible(PrevType,Type),
   AttComparison = [comp(att(RangeVar,Attribute),=,att(PrevRangeVar,PrevAtt))].

translate_argument('$const$'(Constant),rel(SQLTable,RangeVar),Position,ConstComparison,Dict,Dict):-
   % --- Equality comparison of constant value and attribute in table ---------------
   attribute(Position,SQLTable,Attribute,Type),
   get_type('$const$'(Constant),ConstType),
   type_compatible(ConstType,Type),
   ConstComparison = [comp(att(RangeVar,Attribute),=,'$const$'(Constant))].





%% projection_term_variables(ProjectionTerm,Dict) 
%
% extracts all variables from the ProjectionTerm and places them into the
% Dict as a dict/4 term with their Identifier, a non instantiated RangeVar and 
% Attribute argument, and the keyword existential for the type of quantification
%
% ------------------------------------------------------------------------

:- mode projection_term_variables(+,?) is det.
projection_term_variables('$const(_)$',[]).

projection_term_variables('$var$'(VarId),[dict(VarId,_,_,_,existential)]).

projection_term_variables(ProjectionTerm,ProjectionTermVariables):-
   ProjectionTerm =.. [Functor|ProjectionTermList],
   not (Functor = '$var$'),
   not (ProjectionTermList = []),
   projection_list_vars(ProjectionTermList,ProjectionTermVariables).


projection_list_vars([],[]).
projection_list_vars(['$var$'(VarId)|RestArgs],[dict(VarId,_,_,_,existential)|RestVars]):-
   projection_list_vars(RestArgs,RestVars).
projection_list_vars(['$const$'(_)|RestArgs],Vars):-
   projection_list_vars(RestArgs,Vars).






% ------------------------------------------------------------------------
% RESTRICTION! ProjectionTerm underlies the following restrictions:
%
%  - ProjectionTerm must have a functor other than the built-in
%    operators, i.e. ',',';', etc. are not allowed
%
%  - only variables and constants are allowed as arguments,
%    i.e. no structured terms
%
% ------------------------------------------------------------------------

translate_projection('$var$'(VarId),Dict,SelectList):-
   projection_arguments(['$var$'(VarId)],SelectList,Dict).

translate_projection('$const$'(Const),_,['$const$'(Const)]).

translate_projection(ProjectionTerm,Dict,SelectList):-
   ProjectionTerm =.. [Functor|Arguments],
   not (Functor = '$var$'),
   not (Functor = '$const$'),
   not (Arguments = []),
   projection_arguments(Arguments,SelectList,Dict).



projection_arguments([],[],_).

projection_arguments([Arg|RestArgs],[Att|RestAtts],Dict):-
   retrieve_argument(Arg,Att,Dict),
   projection_arguments(RestArgs,RestAtts,Dict).




% - retrieve_argument(Argument,SQLAttribute,Dictionary) 
%
% retrieves the mapping of an argument to the appropriate SQL construct, i.e.
%
%  - qualified attribute names for variables in base goals
%  - arithmetic expressions for variables in arithmetic goals
%  - constant values for constants
% 
% ------------------------------------------------------------------------

retrieve_argument('$var$'(VarId),Attribute,Dict):-
   lookup(VarId,Dict,TableName,AttName,_),
   (
    TableName = is ->
      Attribute = AttName
   ;
      Attribute = att(TableName,AttName)
   ).

retrieve_argument('$const$'(Constant),'$const$'(Constant),_).





%% lookup(Key,Dict,Value) 

lookup(VarId,Dict,RangeVar,Attribute,Type):-
   member(dict(VarId,RangeVar,Attribute,Type,Quant),Dict),
   (
    Quant = all ->
      true
   ;
      nonvar(RangeVar),
      nonvar(Attribute)
   ).



%% add_to_dictionary(Key,RangeVar,Attribute,Quantifier,Dict,NewDict) 

add_to_dictionary(Key,RangeVar,Attribute,Type,_,Dict,Dict):-
   member(dict(Key,RangeVar,Attribute,Type,existential),Dict).

add_to_dictionary(Key,RangeVar,Attribute,Type,Quantifier,Dict,NewDict):-
   not member(dict(Key,_,_,_,_),Dict),
   NewDict = [dict(Key,RangeVar,Attribute,Type,Quantifier)|Dict].




%% aggregate_function(AggregateFunctionTerm,Dict,AggregateFunctionQuery) 
%
% aggregate_function discerns five Prolog aggregate function terms: count, avg, min,
% max, and sum. Each such term is has two arguments: a variable indicating the attribute
% over which the function is to be computed, and a goal argument which must contain in 
% at least one argument position the variable:
%
%    e.g.  avg(Seats,plane(Type,Seats))
%
% These aggregate function terms correspond to the SQL built-in aggregate functions.
% 
% RESTRICTION: AggregateGoal may only be conjunction of (positive or negative) base 
% goals
% 
% ------------------------------------------------------------------------

aggregate_function(AggregateFunctionTerm,Dict,AggregateFunctionExpression):-
        AggregateFunctionTerm =..[AggFunctor,AggVar,AggGoal],
        aggregate_functor(AggFunctor,SQLFunction),
        conjunction(AggGoal,AggConjunction),
        aggregate_query_generation(SQLFunction,AggVar,AggConjunction,Dict,AggregateFunctionExpression).


conjunction(Goal,Conjunction):-
   disjunction(Goal,[Conjunction]).




%% aggregate_query_generation(Function,FunctionVariable,AggGoal,Dict,AggregateQuery) 
%
% compiles the function variable (representing the attribute over which the aggregate 
% function is to be computed) and the aggregate goal (representing the selection and 
% join conditions for the computation of the aggregate function) to an SQL aggregate 
% function subquery.
% 
% ------------------------------------------------------------------------

:- mode aggregate_query_generation(+,+,+,+,?) is det.
aggregate_query_generation(count,'$const$'('*'),AggGoal,Dict,AggregateQuery):-
   translate_conjunction(AggGoal,SQLFrom,SQLWhere,Dict,_TmpDict),

   % ATTENTION! It is assumed that in count(*) aggregate query terms there cannot be
   % free variables because '*' stands for "all arguments"

   AggregateQuery = agg_query(_Function,(count,['$const$'(*)]),SQLFrom,SQLWhere,[]).

aggregate_query_generation(count_distinct,'$const$'('*'),AggGoal,Dict,AggregateQuery):-
   translate_conjunction(AggGoal,SQLFrom,SQLWhere,Dict,_TmpDict),

   % ATTENTION! It is assumed that in count(*) aggregate query terms there cannot be
   % free variables because '*' stands for "all arguments"

   AggregateQuery = agg_query(_Function,(count_distinct,['$const$'(*)]),SQLFrom,SQLWhere,[]).


aggregate_query_generation(Function,FunctionVariable,AggGoal,Dict,AggregateQuery):-
   translate_conjunction(AggGoal,SQLFrom,SQLWhere,Dict,TmpDict),

   % --- only variables occurring in the aggregate goal are relevant to the translation
   % of the function variable and the free variables in the goal.
   % Thus subtract from TmpDict all entries of Dict
   set_difference(TmpDict,Dict,AggDict),
   
   translate_projection(FunctionVariable,AggDict,SQLSelect),
   translate_grouping(FunctionVariable,AggDict,____SQLGroup), % ???
%   AggregateQuery = agg_query(Function,SQLSelect,SQLFrom,SQLWhere,SQLGroup).
   AggregateQuery = agg_query(Function,SQLSelect,SQLFrom,SQLWhere,[]). % CJMTEST!!!!




%% translate_grouping(FunctionVariable,Dict,SQLGroup) 
%
% finds the free variables in the aggregate function term and collects their
% corresponding SQL qualified attributes in the SQLGroup list.
% 
% ------------------------------------------------------------------------

translate_grouping(FunctionVariable,Dict,SQLGroup):-
   free_vars(FunctionVariable,Dict,FreeVariables),
   translate_free_vars(FreeVariables,SQLGroup).




%% free_vars(FunctionVariable,Dict,FreeVarList) 
%
% A Variable is free if it neither occurs as the FunctionVariable, nor is stored as
% existentially quantified (through ^/2 in the original goal) in the dictionary
% 
% FreeVars contains for each variable the relevant attribute and relation information 
% contained in the dictionary
% 
% ------------------------------------------------------------------------

free_vars(FunctionVariable,Dict,FreeVarList):-
  projection_term_variables(FunctionVariable,FunctionVariableList),
  findall((Var,Table,Attribute),
      (member(dict(Var,Table,Attribute,_Type,all),Dict),
       not member(dict(Var,_,_,_,_),FunctionVariableList)
      ),
      FreeVarList).


%% function_variable_list(FunctionVariable,FunctionVariableList) 
%
% extracts the list of variables which occur in the function variable term
%
% RESTRICTION: FunctionVariable may only contain one single variable.
% 
% ------------------------------------------------------------------------

function_variable_list('$var$'(VarId),[VarId]).




%% translate_free_vars(FreeVars,SQLGroup) 
%
% translates dictionary information on free variables to SQLGroup of aggregate
% function query
% 
% ------------------------------------------------------------------------

translate_free_vars([],[]).
translate_free_vars([(_VarId,Table,Attribute)|FreeVars],[att(Table,Attribute)|SQLGroups]):-
   translate_free_vars(FreeVars,SQLGroups).


%% evaluable_expression(ExpressionTerm,Dictionary,Expression,Type) 
%
% evaluable_expression constructs SQL arithmetic expressions with qualified attribute names
% from the Prolog arithmetic expression term and the information stored in the dictionary.
%
% The type of an evaluable function is returned in the argument Type.
%
% The dictionary is not changed because it is used for lookup only. 
% 

evaluable_expression(AggregateFunctionTerm,Dictionary,AggregateFunctionExpression,number):-
   aggregate_function(AggregateFunctionTerm,Dictionary,AggregateFunctionExpression).

evaluable_expression(LeftExp + RightExp,Dictionary,LeftAr + RightAr,number):-
   evaluable_expression(LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(RightExp,Dictionary,RightAr,number).

evaluable_expression(LeftExp - RightExp,Dictionary,LeftAr - RightAr,number):-
   evaluable_expression(LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(RightExp,Dictionary,RightAr,number).

evaluable_expression(LeftExp * RightExp,Dictionary,LeftAr * RightAr,number):-
   evaluable_expression(LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(RightExp,Dictionary,RightAr,number).

evaluable_expression(LeftExp / RightExp,Dictionary, LeftAr / RightAr,number):-
   evaluable_expression(LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(RightExp,Dictionary,RightAr,number).

evaluable_expression('$var$'(VarId),Dictionary,att(RangeVar,Attribute),Type):-
   lookup(VarId,Dictionary,RangeVar,Attribute,Type),
   RangeVar \= is.

evaluable_expression('$var$'(VarId),Dictionary,ArithmeticExpression,Type):-
   lookup(VarId,Dictionary,is,ArithmeticExpression,Type).

evaluable_expression('$const$'(Const),_,'$const$'(Const),ConstType):-
   get_type('$const$'(Const),ConstType).

% CJM 
evaluable_expression(case(TestExp,ThenExp,ElseExp),Dictionary,case(TestAr,ThenAr,ElseAr),number):-
        evaluable_expression(TestExp,Dictionary,TestAr,_),
        %translate_goal(TestExp,Dictionary,TestAr,_),
        %TestAr=TestExp,
        evaluable_expression(ThenExp,Dictionary,ThenAr,_),
        evaluable_expression(ElseExp,Dictionary,ElseAr,_).

% CJM 
evaluable_expression(concat(L,J,R),Dictionary,concat(LAr,J,RAr),varchar):-
        evaluable_expression(L,Dictionary,LAr,_),
        %translate_goal(TestExp,Dictionary,TestAr,_),
        %TestAr=TestExp,
        evaluable_expression(R,Dictionary,RAr,_).

% CJM : allow arithmentic functions to work over subtypes of number
evaluable_expression('$var$'(VarId),Dictionary,ArithmeticExpression,SuperType):-
        nonvar(SuperType),
        subtype(Type,SuperType),
        SuperType\=Type,
        evaluable_expression('$var$'(VarId),Dictionary,ArithmeticExpression,Type).


% ----------------------------------------------------------------
% Pretty printing of queries
% ----------------------------------------------------------------

queries_tokens([Query]) --> !,
	query_tokens(Query),
	[';'].
queries_tokens([Query|Queries]) -->
	query_tokens(Query),
	[' UNION '],
	queries_tokens(Queries).

query_tokens(query([agg_query(Function, Select, From, Where, Group)], _, _)) -->
	% --- ugly rule here: aggregate function only in SELECT Part of query ----
	!,
	query_tokens(agg_query(Function, Select, From, Where, Group)).
query_tokens(query(Select, From, Where)) -->
	clause3_tokens('SELECT', Select, ','),
	clause3_tokens(' FROM', From, ','),
	clause3_tokens(' WHERE', Where, 'AND').
query_tokens(agg_query('COUNT_DISTINCT', Select, From, Where, Group)) -->
	clause4_tokens_with_distinct('SELECT', 'COUNT', Select, ','),
	clause3_tokens(' FROM', From, ','),
	clause3_tokens(' WHERE', Where, 'AND'),
	clause3_tokens(' GROUP BY', Group, ',').
query_tokens(agg_query(Function, Select, From, Where, Group)) -->
	clause4_tokens('SELECT', Function, Select, ','),
	clause3_tokens(' FROM', From, ','),
	clause3_tokens(' WHERE', Where, 'AND'),
	clause3_tokens(' GROUP BY', Group, ',').
query_tokens(negated_existential_subquery(Select, From, Where)) -->
	['NOT EXISTS ('],
	query_tokens(query(Select, From, Where)),
	[')'].

clause4_tokens(Keyword, Function, [Column], Separator) -->
	atom_tokens(Keyword),
	[' '],
	atom_tokens(Function),
	['('],
	clause2_tokens([Column], Separator),
	[')'].

% CJM: a bit hacky...
clause4_tokens_with_distinct(Keyword, Function, [Column], Separator) -->
	atom_tokens(Keyword),
	[' '],
	atom_tokens(Function),
	['(DISTINCT '],
	clause2_tokens([Column], Separator),
	[')'].

clause3_tokens(_Keyword, [], _) --> [].
clause3_tokens(Keyword, [Column|RestColumns], Separator) -->
	atom_tokens(Keyword),
	[' '],
	clause2_tokens([Column|RestColumns], Separator).

clause2_tokens([Item], _) -->
	column_tokens(Item).
clause2_tokens([Item, NextItem|RestItems], Separator) -->
	column_tokens(Item),
	[' '],
	atom_tokens(Separator),
	[' '],
	clause2_tokens([NextItem|RestItems], Separator).

column_tokens('*') -->
	['*'].
column_tokens(att(rel1, Attribute)) --> !, % HACK FOR MySQL!!!
	atom_tokens(Attribute).
column_tokens(att(RangeVar, Attribute)) -->
	atom_tokens(RangeVar),
	['.'],
	atom_tokens(Attribute).
column_tokens(rel(Relation, rel1)) --> !, % HACK FOR MySQL!!!
	atom_tokens(Relation).
column_tokens(rel(Relation, RangeVar)) -->
	atom_tokens(Relation),
	[' '],
	atom_tokens(RangeVar).
column_tokens('$const$'(String)) -->
	{ get_type('$const$'(String), string) }, !,
        esc_atom_tokens(String).
column_tokens('$const$'(Number)) -->
	{ get_type('$const$'(Number), NumType),
          debug(sql_compiler,'check_type_compat ~w ~w',[Number,NumType]),
	  %type_compatible(NumType, num) TODO!!
          true
	},
	atom_tokens(Number).
column_tokens(case(Test, Then, Else)) -->
        !,
        [' CASE WHEN '],
	column_tokens(Test),
	[' THEN '],
	column_tokens(Then),
	[' ELSE '],
	column_tokens(Else),
        [' END CASE '].
/*
column_tokens(concat(Left, Join, Right)) -->
        !,
        [' CONCAT( '],
	column_tokens(Left),
	[', '],
	esc_atom_tokens(Join),
	[', '],
	column_tokens(Right).
        [' ) '].
*/
column_tokens(comp(LeftArg, Operator, RightArg)) -->
	column_tokens(LeftArg),
	[' '],
	atom_tokens(Operator),
	[' '],
	column_tokens(RightArg).
column_tokens(null(Arg)) --> % CJM
	column_tokens(Arg),
	[' IS NULL'].
column_tokens(not_null(Arg)) --> % CJM
	column_tokens(Arg),
	[' IS NOT NULL'].
column_tokens(LeftExpr * RightExpr) -->
	column_tokens(LeftExpr),
	[' * '],
	column_tokens(RightExpr).
column_tokens(LeftExpr / RightExpr) -->
	column_tokens(LeftExpr),
	[' / '],
	column_tokens(RightExpr).
column_tokens(LeftExpr + RightExpr) -->
	column_tokens(LeftExpr),
	[' + '],
	column_tokens(RightExpr).
column_tokens(LeftExpr - RightExpr) -->
	column_tokens(LeftExpr),
	[' - '],
	column_tokens(RightExpr).
column_tokens(agg_query(Function, Select, From, Where, Group)) -->
	['('],
	query_tokens(agg_query(Function, Select, From, Where, Group)),
	[')'].
column_tokens(negated_existential_subquery(Select, From, Where)) -->
	query_tokens(negated_existential_subquery(Select, From, Where)).

string_tokens(S, Xs, Ys) :-
	append(S, Ys, Xs).

atom_tokens(A) --> [A].
esc_atom_tokens(A) --> [''''],[AEsc],[''''],{sql_atom_escape(A,AEsc)}.

sql_atom_escape(X,X):- \+ sub_atom(X,_,_,_,''''),!.
sql_atom_escape(A,AE):-
        atom_chars(A,Toks),
        sql_escape_chars(Toks,Toks2),
        atom_chars(AE,Toks2).

sql_escape_chars([],[]).
sql_escape_chars(['\''|L],['\'','\''|L2]):-
        !,
        sql_escape_chars(L,L2).
sql_escape_chars([C|L],[C|L2]):-
        sql_escape_chars(L,L2).





% ----------------------------------------------------------------
% Conversion of SQL term to string
% ----------------------------------------------------------------

% Original code fails on some SQLQueryTerms for which queries_dstring/1 succeeds!
% Doing this serious kludge instead for now:
sqlterm2atom(SQLQueryTerm, SQLQueryAtom) :-
	queries_tokens(SQLQueryTerm, SQLQueryTokens, []),
        concat_atom(SQLQueryTokens,SQLQueryAtom).





%% Meta Database for schema definition of SQL DB in Prolog ------------
%
% maps Prolog predicates to SQL table names, Prolog predicate argument positions to SQL
% attributes, and Prolog operators to SQL operators. 
%
% ATTENTION! It is assumed that the arithmetic operators in Prolog and SQL are the same,
% i.e. + is addition in Prolog and in SQL, etc. If this is not the case, then a mapping
% function for arithmetic operators is necessary too.
% ---------------------------------------------------------------------


%% relation(PrologFunctor,Arity,SQLTableName) 

relation(F,A,F):- relation(F,A).

% ------------------------------------------------------------------------
%
% Output to screen predicates - rather crude at the moment
%
% ------------------------------------------------------------------------

:- mode print_sqlterm(+) is det.
print_sqlterm(T):-
        %printqueries(T).
        sqlterm2atom(T,S),
        writeln(S).





%% query_atom(QueryCode) 

query_atom(query([agg_query(Function,Select,From,Where,Group)],_,_),QueryList,Diff):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   query_atom(agg_query(Function,Select,From,Where,Group),QueryList,Diff).

query_atom(query(Select,From,Where),QueryList,Diff):-
   clause_atom('SELECT',Select,',',QueryList,X1),
   clause_atom('FROM',From,',',X1,X2),
   clause_atom('WHERE',Where,'AND',X2,Diff).

query_atom(agg_query(Function,Select,From,Where,Group),QueryList,Diff):-
   clause_atom('SELECT',Function,Select,',',QueryList,X1),
   clause_atom('FROM',From,',',X1,X2),
   clause_atom('WHERE',Where,'AND',X2,X3),
   clause_atom('GROUP BY',Group,',',X3,Diff).

query_atom(negated_existential_subquery(Select,From,Where),QueryList,Diff):-
   column_atom('NOT EXISTS(',QueryList,X1),   
   clause_atom('SELECT',Select,',',X1,X2),
   clause_atom('FROM',From,',',X2,X3),
   clause_atom('WHERE',Where,'AND',X3,X4),
   column_atom(')',X4,Diff).




%% clause_atom(Keyword,ClauseCode,Junctor,CurrAtom,QueryAtom) 
%
% with 
% Keyword    one of SELECT, FROM, WHERE, or GROUP BY, 
% ClauseCode the code corresponding to the appropriate clause of an SQL query, and 
% Junctor    indicating the character(s) through which the items of a clause
%            are separated from each other (',' or 'AND').

clause_atom(_Keyword,[],_,QueryList,QueryList).

clause_atom(Keyword,[Column|RestColumns],Junctor,QueryList,Diff):-
   column_atom(Keyword,QueryList,X1),
   column_atom(' ',X1,X2),
   clause_atom([Column|RestColumns],Junctor,X2,X3),
   column_atom(' ',X3,Diff).

clause_atom(Keyword,Function,[Column],Junctor,QueryList,Diff):-
   column_atom(Keyword,QueryList,X1),
   column_atom(' ',X1,X2),
   column_atom(Function,X2,X3),
   column_atom('(',X3,X4),
   clause_atom([Column],Junctor,X4,X5),
   column_atom(') ',X5,Diff).






%% clause_atom(ClauseCode,Junctor) 

clause_atom([Item],_,QueryList,Diff):-
   column_atom(Item,QueryList,Diff).

clause_atom([Item,NextItem|RestItems],Junctor,QueryList,Diff):-
   column_atom(Item,QueryList,X1),
   column_atom(' ',X1,X2),
   column_atom(Junctor,X2,X3),
   column_atom(' ',X3,X4),
   clause_atom([NextItem|RestItems],Junctor,X4,Diff).





column_atom(att(RangeVar,Attribute),QueryList,Diff):-
   column_atom(RangeVar,QueryList,X1),
   column_atom('.',X1,X2),
   column_atom(Attribute,X2,Diff).

column_atom(rel(Relation,RangeVar),QueryList,Diff):-
   column_atom(Relation,QueryList,X1),
   column_atom(' ',X1,X2),
   column_atom(RangeVar,X2,Diff).

column_atom('$const$'(String),QueryList,Diff):-
   get_type('$const$'(String),string),
   column_atom('"',QueryList,X1),
   column_atom(String,X1,X2),
   column_atom('"',X2,Diff).

column_atom('$const$'(Number),QueryList,Diff):-
   get_type('$const$'(Number),NumType),
   type_compatible(NumType,number),
   column_atom(Number,QueryList,Diff).

column_atom(comp(LeftArg,Operator,RightArg),QueryList,Diff):-
   column_atom(LeftArg,QueryList,X1),
   column_atom(' ',X1,X2),
   column_atom(Operator,X2,X3),
   column_atom(' ',X3,X4),
   column_atom(RightArg,X4,Diff).

column_atom(LeftExpr * RightExpr,QueryList,Diff):-
   column_atom(LeftExpr,QueryList,X1),
   column_atom('*',X1,X2),
   column_atom(RightExpr,X2,Diff).

column_atom(LeftExpr + RightExpr,QueryList,Diff):-
   column_atom(LeftExpr,QueryList,X1),
   column_atom('+',X1,X2),
   column_atom(RightExpr,X2,Diff).

column_atom(LeftExpr - RightExpr,QueryList,Diff):-
   column_atom(LeftExpr,QueryList,X1),
   column_atom('-',X1,X2),
   column_atom(RightExpr,X2,Diff).

column_atom(LeftExpr / RightExpr,QueryList,Diff):-
   column_atom(LeftExpr,QueryList,X1),
   column_atom('/',X1,X2),
   column_atom(RightExpr,X2,Diff).

column_atom(agg_query(Function,Select,From,Where,Group),QueryList,Diff):-
   column_atom('(',QueryList,X1),
   query_atom(agg_query(Function,Select,From,Where,Group),X1,X2),
   column_atom(')',X2,Diff).

column_atom(negated_existential_subquery(Select,From,Where),QueryList,Diff):-
   query_atom(negated_existential_subquery(Select,From,Where),QueryList,Diff).


column_atom(Atom,List,Diff):-
   atom(Atom),
   name(Atom,X1),
   append(X1,Diff,List).






%% gensym(Root,Symbol) 
%
% SEPIA 3.2. version - other Prolog implementations provide gensym/2
% and init_gensym/1 as built-ins. */
%
% (C) Christoph Draxler, Aug. 1992
%
% 

lgensym(L,Symbol):-
        concat_atom(L,Root),
        gensym(Root,Symbol).

   

init_gensym(Root):-
   nonvar(Root),
   nb_setval(Root,0).



%% auxiliary predicates (some of them may be built-in... -----------------

append([],L,L).
append([H1|L1],L2,[H1|L3]):-
   append(L1,L2,L3).



member(X,[X|_]).
member(X,[_|T]):-
   member(X,T).



repeat_n(N):-
   integer(N),
   N > 0,
   repeat_1(N).

repeat_1(1):-!.
repeat_1(_).
repeat_1(N):-
   N1 is N-1,
   repeat_1(N1).





%% set_difference(SetA,SetB,Difference) 
%
% SetA - SetB = Difference

set_difference([],_,[]).

set_difference([Element|RestSet],Set,[Element|RestDifference]):-
   not member(Element,Set),
   set_difference(RestSet,Set,RestDifference).

set_difference([Element|RestSet],Set,RestDifference):-
   member(Element,Set),
   set_difference(RestSet,Set,RestDifference).


%% benchmarks of sample queries --------




%% Mapping of Prolog operators to SQL operators -------------------------------

comparison(=,=).
comparison('iz','IS').          % CJM
comparison(<,<).
comparison(>,>).
comparison(@<,<).
comparison(@>,>).


negated_comparison(is,'IS NOT'). % CJM 
negated_comparison(=,'!='). % CJM changed <> to !=
negated_comparison(\=,=).
negated_comparison(>,=<).
negated_comparison(=<,>).
negated_comparison(<,>=).
negated_comparison(>=,<).


%% aggregate_function(PrologFunctor,SQLFunction) 

aggregate_functor(avg,'AVG').
aggregate_functor(min,'MIN').
aggregate_functor(max,'MAX').
aggregate_functor(sum,'SUM').
aggregate_functor(count,'COUNT').
aggregate_functor(count_distinct,'COUNT_DISTINCT'). % CJM: ugly, this is translated later



%% type system ----------------------------------------------------
%
% A rudimentary type system is provided for consistency checking during the
% translation and for output formatting
%
% The basic types are string and number. number has the subtypes integer and
% real.
%
% -----------------------------------------------------------------


type_compatible(Type,Type):-
   is_type(Type).
type_compatible(SubType,Type):-
   subtype(SubType,Type).
type_compatible(Type,SubType):-
   subtype(SubType,Type).


%% subtype(SubType,SuperType) 
%
% Simple type hierarchy checking
%
% -----------------------------------------------------------------

subtype(SubType,SuperType):-
   is_subtype(SubType,SuperType).

subtype(SubType,SuperType):-
   is_subtype(SubType,InterType),
   subtype(InterType,SuperType).



%% is_type(Type) 
%
% Type names
%
% -----------------------------------------------------------------

% TODO [CJM]: add more for other SQL Types?
is_type(number).
is_type(integer).
is_type(int).
is_type(real).
is_type(string).
is_type(natural).
is_type(float).
is_type(double).
% mysql:
is_type(smallint).
is_type(tinyint).
is_type('tinyint unsigned').
is_type('smallint unsigned').
is_type('integer unsigned').


%% is_subtype(SubType,SuperType) 
%
% Simple type hierarchy for numeric types
%
% -----------------------------------------------------------------

is_subtype(integer,number).
is_subtype(float,number).
is_subtype(double,number).
is_subtype(int,number).
is_subtype(smallint,number).
is_subtype(tinyint,number).
is_subtype('integer unsigned',number).
is_subtype('smallint unsigned',number).
is_subtype('tinyint unsigned',number).
is_subtype(real,number).
is_subtype(natural,integer).
is_subtype(bool,string).        % not quite right but seems to be required..


%% get_type(Constant,Type)
%
% Prolog implementation specific definition of type retrieval
% sepia Prolog version given here
%
% -----------------------------------------------------------------

get_type('$const$'(Constant),integer):-
   number(Constant).

get_type('$const$'(Constant),string):-
   atom(Constant).



