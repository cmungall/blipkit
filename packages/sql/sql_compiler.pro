/* -*- Mode: Prolog -*- */
   
:- module(sql_compiler,
          [
           plterm_to_sqlterm/3,
           print_sqlterm/1,
           sqlterm2atom/2,
           load_schema_defs/1,
           get_type/2,
           rewrite_query/2,
           op(1150,xfy,(<-)),
           op(1200,xfy,(::))
          ]).

:- multifile relation/2,relation/3,attribute/4,unique/2,schema/1.
:- discontiguous relation/2,relation/3,attribute/4,unique/2,schema/1.
:- discontiguous view/2,view/3,sql_expand/1.
:- multifile view/2,view/3,sql_expand/1,schema_dbname/2.

% CJM:
% sqlschema_connection(?Schema,?Rdb)
% globals for database handles
:- multifile sqlschema_connection/2.
:- dynamic sqlschema_connection/2.

% CJM: required by SWI-Prolog
:- op(900,fy,not).		% Draxler uses this. Should we convert wholesale to '\+' ?
:- op(1150,xfy,(<-)).
:- op(1200,xfy,(::)).
%:- op(800,xfy,in).


%% view(?Head,?Body)
% SQL rewrite - Head is rewritten as Body.
% Can also be written as =|Head <- Body|=
:- dynamic view/2,view/3,schema_dbname/2.

%% expand_relation_name(+R,?RX)
% prefixes table name with database name if schema_dbname/2 is set for the schema to which
% R belongs
expand_relation_name(R,RX):-
        relation(R,A),
        clause(relation(R,A),_,Clause),
        clause_property(Clause,file(File)),
        clause(schema(S),_,SClause),
        clause_property(SClause,file(File)),
        schema_dbname(S,DB),
        sformat(RX,'~w.~w',[DB,R]),
        !.
expand_relation_name(R,R).


/*
% Views: declared analagous to clauses with <- not :-
% rewrite to a view/2 fact
system:term_expansion( (Mod:Head <- Body),
                     [   sql_compiler:view(Head,Body),
                         (   Mod:Head :- getrdb(Rdb),rdb_query(Rdb,Head,Head))]):- !.
% TODO: only do this when asked
*/
system:term_expansion( (_:Head <- Body :: Where),
                     sql_compiler:view(Head,Body,Where)).
system:term_expansion( (_:Head <- Body),
                     sql_compiler:view(Head,Body)).
system:term_expansion( (Head <- Body),
                     sql_compiler:view(Head,Body)).


%% load_schema_defs(+SchemaFileSpec)
%
% loads a schema into memory, allowing sql_compiler to make use of it for plterm_to_sqlterm/3
%
% Example: =|load_schema_defs(bio('sql_schema/schema_enscore44'))|=
load_schema_defs(File):-
        consult(File).



%% plterm_to_sqlterm(+ProjectionTerm,+DatabaseGoal,?SQLQueryTerm)
%
% rewrite a prolog goal as a SQL Term.
% The SQL term can then be translated via sqlterm2atom/2
% 
% @param ProjectionTerm
% @param DatabaseGoal
% @param SQLQueryTerm
:- module_transparent(plterm_to_sqlterm/3).
plterm_to_sqlterm(ProjectionTerm0,G,SQLQueryTerm):-
        debug(sql_compiler,'original proj ~w',ProjectionTerm0),
        debug(sql_compiler,'original goal ~w',G),
        
        % first of all we rewrite the query, this is essentially a view mechanism.
        % it can take advantage of view/2 predicates, or it can use normal prolog program
        % rules in the rewriting process.
        % it is important that we rewrite before we copy the term, because rewriting
        % may introduce unification to skolem terms in the projection. These must all match up
        % the resulting row values. As an example see intron/1 in genome_db.
        rewrite_query(G,G1),
        !,
        debug(sql_compiler,'rewritten_as: ~w',G1),

        % we copy terms because otherwise the prolog variables will be unified with sql_compiler
        % variable terms
        copy_term((ProjectionTerm0,G1),(ProjectionTerm,G2)),
        debug(sql_compiler,'copied to: ~w',G2),

        % merge terms based on unique database keys
        optimize_query_all(G2,G3),
        debug(sql_compiler,'optimized as ~w',G3),
        
        % unify SqlQueryTerm with the translation results
        translate(ProjectionTerm,G3,SQLQueryTerm),
        !.

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
% TODO: optimize truncated predicates: eg foo(A,B) is treated as foo(A,B,_) - but they are currently treated
% as different by optimization
% TODO: better to optimize *after* conversion to Disjunction? Or *both*?
% after disjunction we have translated prolog variables to $var$s
optimize_query_all(G,G2):-
        %optimize_query_vars(G),
        optimize_query(G,G1),
        (   G == G1             % identical if optimizing yielded nothing
        ->  G2=G                % base case
        ;   optimize_query_all(G1,G2)). % keep on optimizing

%% optimize_query(+Goal,?GoalOpt)
% if two terms can be proved to unify to the same row, merge them
% unifying vars
%:- mode optimize_query(+,?) is det.
optimize_query((A=B,Gs),Go):- % e.g. (a=a,...)
	%nonvar(A),
	%nonvar(B),
	\+compound(A), 
	\+compound(B),
	A==B,
        !,
        optimize_query(Gs,Go).
optimize_query((G,Gs),Go):-
        unify_plterm(G,Gs),
        !,
        optimize_query(Gs,Go).
optimize_query((G;Gs),(G2;Gs2)):-
        !,
        optimize_query(G,G2),
        optimize_query(Gs,Gs2).
optimize_query((G,Gs),Gm):-
        !,
        optimize_query(Gs,Gs1),
        debug(sql_compiler,'optimized: ~w -> ~w.~nNow merging ~w ~w',[Gs,Gs1,G,Gs1]),
        merge_goals(G,Gs1,Gm).
optimize_query(not(G),not(G2)):-
        !,
        optimize_query(G,G2).
optimize_query(X is T,X is T2):- % e.g. count
        T=..[F,V,G],
        aggregate_functor(F,_),
        !,
        optimize_query(G,G2),
        T2=..[F,V,G2].
optimize_query(X,X).

%% unify_plterm(+TermToUnify,?Term) is semidet.
% example:
%  unify_plterm(person(A,B),(person(C,D),person(A,C))) ==> B=C (if arg1 of person/2 is unique)
%:- mode unify_plterm(+,?) is semidet.
unify_plterm(G,(G2,_)):-
        unify_plterm1(G,G2),    % no need to recurse further
        !.
unify_plterm(G,(_,Gs)):-
        !,
        unify_plterm(G,Gs).
%unify_plterm(G,X is F):-
%        F=..[_,_,Gs],
%        !,
%        unify_plterm(G,Gs).
unify_plterm(_G,not(_Gs)):-
        !,
        fail.
        %unify_plterm(G,Gs).
unify_plterm(G,G2):-
        unify_plterm1(G,G2),
        !.

% use uniqueness constraint to reduce two terms to one
unify_plterm1(G,G2):-
        functor(G,F,Arity),
        functor(G2,F,Arity),
        unique(F,Attr),
        \+ Attr = [_|_],
        attribute(AttrOrd,F,Attr,_),
        arg(AttrOrd,G,V1),
        arg(AttrOrd,G2,V2),
        V1 == V2,               % unique key is equivalent
        !,                      % only succeed once
        debug(sql_compiler,'unifying based on unique key ~w: ~w = ~w',[Attr,G,G2]),
        G=G2.                   % unify if equivalent

% uniqueness constraint with multiple keys
unify_plterm1(G,G2):-
        functor(G,F,Arity),
        functor(G2,F,Arity),
        unique(F,Attrs),
        Attrs = [_|_],
        forall(member(Attr,Attrs),
               (   attribute(AttrOrd,F,Attr,_),
                   arg(AttrOrd,G,V1),
                   arg(AttrOrd,G2,V2),
                   V1 == V2)),  % unique key is equivalent
        !,                      % only succeed once
        debug(sql_compiler,'unifying based on unique keyset ~w: ~w = ~w',[Attrs,G,G2]),
        G=G2.                   % unify if equivalent


% ========================================
% views
% ========================================

% todo: make sure that datalog predicates and sql relations are not confused
% e.g. see genome_bridge_from_seqfeature

:- module_transparent rewrite_query/2.
%% rewrite_query(+GoalIn,?GoalOut) is det
rewrite_query(call(_),_):- !,fail.
rewrite_query(aggregate(count,X,G,Num),	is(Num,count_distinct(X,G))) :-
	!.
rewrite_query((G,Gs),Gm):-
        !,
        rewrite_query(G,G1),
        rewrite_query(Gs,Gs1),
        merge_goals(G1,Gs1,Gm).
rewrite_query((G;Gs),(G1;Gs1)):-
        !,
        rewrite_query(G,G1),
        rewrite_query(Gs,Gs1).
rewrite_query(_:G,G2):- % todo - or clauses from multiple views...
        % will not do full disjunction under some circumstances; see below
        view(G,B,Where), % /3
        Where,
        !,
        rewrite_query(B,G2),
        !.
%rewrite_query(_:G,G2):- % todo - or clauses from multiple views...
%        % will not do full disjunction under some circumstances; see below
%        view(G,B,Where),
%        trace,
%        setof(G-B,Where,GBs),
%        unify_keys(G,GBs,Bs),
%        !,
%        list_to_disj(Bs,BDisj),
%        rewrite_query(BDisj,G2),
%        !.
rewrite_query(_:G,G2):- % todo - or clauses from multiple views...
        rewrite_query(G,G2),
        !.
%rewrite_query(Head,G2):- % todo - or clauses from multiple views...
%	view(Head,Body),
%	!,
%	rewrite_query(Body,G2).
rewrite_query(Head,G2):- % proper disjoint - expmntl
        Head=..[_|Args],
	functor(Head,F,Arity),
	functor(HeadC,F,Arity),
	HeadC=..[_|ArgsC],
	% grab all relevant rules. we will rewrite these
	% foo(X, a) <- a(X) ==> foo(C,D) <- D=a,a(C).
	% the head is all variables, unification is explicit in the body
        setof(HeadC <- (ArgsC=Args,Body),
	      sql_compiler:view(Head, Body),
	      Rules1),
	rules_to_normal_form(Rules1,Rules),
	setof(Body,member(Head<-Body,Rules),DisjList),
				%setof(B,sql_compiler:view(G,B),Bs),
        list_to_disj(DisjList,BDisj),
        rewrite_query(BDisj,G2),
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
rewrite_query(\+(G),not(G2)):-
        !,
        rewrite_query(G,G2).

rewrite_query(V^G,V^G2):-
        !,
        rewrite_query(G,G2).

% rewrite prolog predicates as views.
% first of all, the simple non-disjunctive case:
rewrite_query(H,G2):-
        H \= _^_,               % guard against ^/2 as predicate; we use as existential
        % note that the rewritten clause must either
        %  - be an exported module predicate
        %  - in the user module
        context_module(Mod),
        debug(sql_compiler,'  testing if clause(~w,_) in module: ~w ?',[H,Mod]),
	% first we check there is only one matching clause:
	% if there are more, we use the disjunction version below
        catch(findall(B,clause(H,B),[_]), % prolog predicate
              _,
              fail),
        catch(setof(B,clause(H,B),Bs), % prolog predicate
              _E,
              fail),
        debug(sql_compiler,'  clause: ~w ',[Bs]),
        !,
        list_to_disj(Bs,BDisj),
        % TODO: check for recursive predicates
        debug(sql_compiler,'rewrite (single clause): ~w => ~w',[H,BDisj]),
        rewrite_query(BDisj,G2).
% treat prolog predicates as views
% disjunctive case
rewrite_query(Head,G2):- 
        Head \= _^_,               % guard against ^/2 as predicate; we use as existential
	% rewrites multiple matching clauses as disjunction
	% DOES NOT WORK FOR SKOLEMS: assumes Head args are non-compound
        context_module(Mod),
        debug(sql_compiler,'  is_clause: ~w in module: ~w ?',[Head,Mod]),
        % fetch all possibilities when H contains
        % free variables not in B.
        % E.g. foo(X,a):- a(X). foo(X,b):- b(X).
        % What we get is [(a(X),Arg2=a),(b(X),Arg2=b)] 
        Head=..[_|Args],
	functor(Head,F,Arity),
	\+relation(F,Arity),
	functor(HeadC,F,Arity),
	HeadC=..[_|ArgsC],
	% grab all relevant rules. we will rewrite these
	% foo(X, a) <- a(X) ==> foo(C,D) <- D=a,a(C).
	% the head is all variables, unification is explicit in the body
        catch(setof(HeadC <- (ArgsC=Args,Body),
		    (	clause(Head, Body)),
		    Rules1),
	      _E,
	      fail),
	rules_to_normal_form(Rules1,Rules),
	setof(Body,Rules^member(Head<-Body,Rules),DisjList),
        list_to_disj(DisjList,BDisj),

        %catch(setof(B,clause(H,B),Bs), % prolog predicate
        %      _E,
         %     fail),
        %debug(sql_compiler,'  clause: ~w ',[Bs]),
        %!,
        %list_to_disj(Bs,BDisj),
        % TODO: check for recursive predicates
        debug(sql_compiler,'rewrite (disjunction of clauses): ~w => ~w',[Head,BDisj]),
        rewrite_query(BDisj,G2).
rewrite_query(G,G).

%% rules_to_normal_form(+RulesIn:list,?RulesOut:list)
% RulesIn = [Head<-(ArgList1=ArgList2,Body), ...]
% RulesOut = [Head<-NewBody, ...]
% where NewBody is conjunction using ,/2
rules_to_normal_form([],[]).
rules_to_normal_form([H<-(Args=Args2,Body)|T],[H<-ArgConj|T2]):-
	arglists_to_conj(Body,Args,Args2,ArgConj),
	rules_to_normal_form(T,T2).

%% arglists_to_conj(+Body,+ArgList1,+ArgList2,?Conj)
% where Conj 
arglists_to_conj(Body,[],[],Body).
arglists_to_conj(Body,[H1|T1],[H2|T2],TN):-
	var(H1),	
	var(H2),
	!,			% original head used variable in this position
	H1=H2,
	arglists_to_conj(Body,T1,T2,TN).
arglists_to_conj(Body,[H1|T1],[H2|T2],(H1=H2,TN)):-
	arglists_to_conj(Body,T1,T2,TN).

merge_goals((H,T),G,(H,G2)):-
        !,
        merge_goals(T,G,G2).
merge_goals(G1,G2,(G1,G2)).

goal_to_list((H,T),[H2|T2]):-
        !,
        goal_to_list(H,H2),
        goal_to_list(T,T2).
goal_to_list(X,[X]).

%:- mode translate(+,+,?) is det.
%% translate(+ProjectionTerm,+DatabaseGoal,?SQLQueryTerm) is det
% Top level predicate translate/3 organizes the compilation and constructs a
% Prolog term representation of the SQL query.
% (in Ciao, this is called pl2sqlterm)
translate(ProjectionTerm,DatabaseGoal,SQLQueryTerm):-

   % --- tokenize projection term and database goal ------------------------
        tokenize_term(DatabaseGoal,TokenDatabaseGoal),
        tokenize_term(ProjectionTerm,TokenProjectionTerm),

   % --- lexical analysis: reordering of goals for disjunctive normalized form -
        convert_to_disj_norm_form(TokenDatabaseGoal,Disjunction),
	% TODO: we should do second optimization step; but we can't reuse the exist code based on prolog unification
        debug(sql_compiler,'disj ~w',[Disjunction]),

        remove_unsatisfiable_clauses(Disjunction,DisjunctionOpt),
        
   % --- code generation -----------------------------------------------------
%	pp_prologterm(DisjunctionOpt),
%	trace,
        query_generation(DisjunctionOpt,TokenProjectionTerm,SQLQueryTerm),
        !,
        debug(sql_compiler,'Generated query: ~w',[SQLQueryTerm]).
translate(ProjectionTerm, DatabaseGoal, _SQLQueryTerm) :-
	error_message("SQL translation failed for ~q / ~q", [ProjectionTerm, DatabaseGoal]),
	fail.

%% remove_unsatisfiable_clauses(+Disjunction:list, ?DisjunctionOptimized) is det
%
% if we start with a prolog goal
%  =|(bar(X),(foo(X,a);foo(X,b))),(wiggle(X),(foo(X,a);foo(X,b)))|=
%
% and convert to disjunctive normal form we end up with 4 disjunctions, 2 of which
% are unsatisfiable.
%
% we optimize here in case the RDB cannot.
%
% added by: CJM 2009/07/20
remove_unsatisfiable_clauses([],[]).
remove_unsatisfiable_clauses([C|Cs],[C|Cs2]) :-
        is_clause_satisfiable(C),
        !,
        remove_unsatisfiable_clauses(Cs,Cs2).
remove_unsatisfiable_clauses([_|Cs],Cs2) :-
        !,
        remove_unsatisfiable_clauses(Cs,Cs2).

is_clause_satisfiable(C) :-
        clause_bindings(C,Bindings),
        \+ ((member(V=C1,Bindings),
             member(V=C2,Bindings),
             C1\=C2)).

clause_bindings((C1,C2),Bindings) :-
        !,
        clause_bindings(C1,Bindings1),
        clause_bindings(C2,Bindings2),
        append(Bindings1,Bindings2,Bindings).
clause_bindings('$var$'(V)='$const$'(C),[V=C]) :- !.
clause_bindings(_,[]).


        
        
        

%% convert_to_disj_norm_form(+Goal,?Disjunction:list) 
%
% turns original goal into disjunctive normalized form by computing a
% set of flat conjunctions and collecting them in a list, whose
% elements are assumed to be joined by disjuntions.

%:- mode convert_to_disj_norm_form(+,?) is det.
convert_to_disj_norm_form(Goal,Disjunction):-
   findall(Conjunction,linearize(Goal,Conjunction),Disjunction).
%   forall(member(Conjunction,Disjunction),
%	  is_conjunction(Conjunction)).

is_conjunction(G) :-
	G=((A,B),C),
	format(user_error,'A: ~w~n',[A]),
	format(user_error,'B: ~w~n',[B]),
	format(user_error,'C: ~w~n',[C]),
	throw(not_a_conjunction(G)).
is_conjunction((_,B)) :-
	!,
	is_conjunction(B).
is_conjunction(_) :- !.


%% linearize(+Goal,?ConjunctionTerm) 
%
% Returns a conjunction of base goals for a complex disjunctive or conjunctive goal
% Yields several solutions upon backtracking for disjunctive goals

	% CJM NOTES:
	% Original Draxler code had ((a,b ; c),d) => (a, b),c [soln1] -- this is not
	%  correct, should be (a,b,d).
	% This fixes it such that we always have right-linear conjunctions

%:- mode linearize(+,?) is nondet.

%:- mode linearize(+,?) is nondet.
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

linearize(\+ A, \+ LinA):-
   linearize(A,LinA).

linearize(Var^A, Var^LinA):-
   linearize(A,LinA).

linearize(A,A):-
   A \= (_,_),
   A \= (_;_),
   A \= _^_,
   A \= \+(_),			% CJM
   A \= not(_).




%% tokenize_term(+Term,?TokenizedTerm) 
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

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
        var(VarId), 
        % --- uninstantiated variable: instantiate it with unique identifier.                        
        gensym(var,VarId).

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
        nonvar(VarId),
	%% with cut,  '$const$'(var22) is not instantiated. ^^ [CIAO]
	!. 

tokenize_term(Constant,'$const$'(Constant)):-
        nonvar(Constant),
        functor(Constant,_,0).
	%atm(Constant),   %[CIAO]
	%!,
	%atom_codes(Constant, RealConstant).

% [CIAO has other stuff here]

tokenize_term(Term,TokenizedTerm):-
        nonvar(Term),
        Term \= '$var$'(_),
        Term \= '$const$'(_),
        Term =.. [Functor|Arguments],
        Arguments \= [],
        maplist(tokenize_term,Arguments,TokenArguments),
        % [CJM] tokenize_arguments(Arguments,TokenArguments), 
        TokenizedTerm =.. [Functor|TokenArguments].



%% query_generation(+ListOfConjunctions:list, +ProjectionTerm, ?ListOfQueries:list) 
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

%:- mode query_generation(+,+,?) is det.
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
        debug(sql_compiler,'translating projection: ~w',[ProjectionTerm]),
        translate_projection(ProjectionTerm,Dict,SQLSelect),
        Query = query(SQLSelect,SQLFrom,SQLWhere),
        debug(sql_compiler,'query= ~w',[Query]),
        query_generation(Conjunctions,ProjectionTerm,Queries).

% the above differs a little from Ciao, which uses check_xxx predicates for better error reporting

%%  reorder_conjunction(+Conjunction,?NewConjunction)
%
%  make sure comparison operators do not precede relations
%:- mode reorder_conjunction(+,?) is det.
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

check_translate_goal(Goal, SQLFrom, SQLWhere, Dict, NewDict) :-
	translate_goal(Goal, SQLFrom, SQLWhere, Dict, NewDict),
	!.
check_translate_goal(Goal, _SQLFrom, _SQLWhere, _Dict, _NewDict) :-
	error_message('translate_goal(~w)', [Goal]),
	fail.


%% translate_goal(+Goal,?SQLFrom,?SQLWhere,+Dict,?NewDict) 
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

%:- mode translate_goal(+,?,?,+,?) is det.
translate_goal(SimpleGoal,[SQLFrom],SQLWhere,Dict,NewDict):-
   % --- positive goal binds variables - these bindings are held in the dictionary -----
	functor(SimpleGoal,Functor,Arity),
	translate_functor(Functor,Arity,SQLFrom),
	SimpleGoal =.. [Functor|Arguments],
	debug(sql_compiler,'  Dict: ~w',[Dict]),
	debug(sql_compiler,'     translating arguments from siple goal: ~w',[SimpleGoal]),
	translate_arguments(Arguments,SQLFrom,1,SQLWhere,Dict,NewDict),
	debug(sql_compiler,'  NewDict: ~w',[NewDict]).

translate_goal(Result is Expression,[],SQLWhere,Dict,NewDict):-
	debug(sql_compiler,'  translate_arithmetic_function: ~w is ~w',[Result,Expression]),
        translate_arithmetic_function(Result,Expression,SQLWhere,Dict,NewDict).

% CJM : TODO
% e.g. for concat
translate_goal(eval(Result, Expression),[],SQLWhere,Dict,NewDict):-
        translate_builtin_function(Result,Expression,SQLWhere,Dict,NewDict).
% TODO: registerable functions
%translate_goal(Goal,[],SQLWhere,Dict,NewDict):- 
%        registered_func_pred(Goal,Result),   % e.g. register(atom_concat(_,_,C),C)


% negated goals do not bind variables - hence Dict is returned unchanged -------
translate_goal(not NegatedGoals,[],SQLNegatedSubquery,Dict,Dict):-
   functor(NegatedGoals,Functor,_),
   \+ comparison(Functor,_),
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
   debug(sql_compiler,'  ComparisonOp: ~w in ~w',[SQLOperator,ComparisonGoal]),
   translate_comparison(LeftArg,RightArg,SQLOperator,Dict,SQLCompOp).

translate_goal(not_null(Arg),[],[not_null(Term)],Dict,Dict):- % CJM
        evaluable_expression(Arg,Dict,Term,_ArgType).

translate_goal(null(Arg),[],[null(Term)],Dict,Dict):- % CJM
        evaluable_expression(Arg,Dict,Term,_ArgType).

% expand prolog goals; module must be loaded to have effect
%translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict):-
%        Goal \= (_,_),
%        clause(Goal,Body),      % view
%        translate_goal(Body,SQLFrom,SQLWhere,Dict,NewDict).





%% translate_conjunction(+Conjunction,?SQLFrom,?SQLWhere,+Dict,?NewDict) 
% 
% translates a conjunction of goals (represented as a list of goals preceeded by 
% existentially quantified variables) to FROM- and WHERE-clause of an SQL query.  
% A dictionary containing the associated SQL table and attribute names is built up
% as an accumulator pair (arguments Dict and NewDict)
%
% Conjunction = Var^Goal | (Goal,Conjunction) | Var

%:- mode translate_conjunction(+,?,?,+,?) is det.
translate_conjunction('$var$'(VarId)^Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   % --- add info on existentially quantified variables to dictionary here -----------
   add_to_dictionary(VarId,_,_,_,existential,Dict,TmpDict),
   translate_conjunction(Goal,SQLFrom,SQLWhere,TmpDict,NewDict).

translate_conjunction(Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   Goal \= (_,_),
   debug(sql_compiler,'simple conj, translating goal ~w',[Goal]),
   check_translate_goal(Goal,SQLFrom,SQLWhere,Dict,NewDict),
   debug(sql_compiler,'translated goal ~w',[Goal]).

% CJM: see linearize
translate_conjunction((Goal,Conjunction),SQLFrom,SQLWhere,Dict,NewDict):-
	Goal = (G1,G2), 
	!,
	translate_conjunction((G1,(G2,Conjunction)),SQLFrom,SQLWhere,Dict,NewDict).

translate_conjunction((Goal,Conjunction),SQLFrom,SQLWhere,Dict,NewDict):-
        debug(sql_compiler,'complex conj, translating goal ~w',[Goal]),
        check_translate_goal(Goal,FromBegin,WhereBegin,Dict,TmpDict),
        debug(sql_compiler,'OK ~w with ~w',[Goal,TmpDict]),
        debug(sql_compiler,'translating conjunction: ~w',[Conjunction]),
        translate_conjunction(Conjunction,FromRest,WhereRest,TmpDict,NewDict),
        debug(sql_compiler,'conj ~w',[Conjunction-FromRest-WhereRest]),
        append(FromBegin,FromRest,SQLFrom),
        append(WhereBegin,WhereRest,SQLWhere).

translate_builtin_function('$var$'(VarId),Expression,[],Dict,NewDict):-
   % assigment of value of arithmetic expression to variable - does not
   % show up in WHERE-part, but expression corresponding to
   % variable must be stored in Dict for projection translation
   evaluable_expression(Expression,Dict,ArithExpression,Type),
   add_to_dictionary(VarId,is,ArithExpression,Type,all,Dict,NewDict).


%% translate_arithmetic_function(+Result,+Expression,?SQLWhere,+Dict:list,?NewDict:list) 
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
% @param Result Example: $var$(Id)
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
   \+ (PrevRangeVar = is),

   % test whether type of attribute is numeric - if not, there's no sense in 
   % continuing the translation

   check_type_compatible(PrevType,number),
   evaluable_expression(Expression,Dict,ArithExpression,ExprType),
   check_type_compatible(ExprType,number),
   ArithComparison = [comp(att(PrevRangeVar,PrevAtt),'=',ArithExpression)].


translate_arithmetic_function('$var$'(VarId),Expression,ArithComparison,Dict,Dict):-
   % --- test whether left side evaluates to right side: return equality comparison ----
   % Left side consists of arithmetic expression, i.e. VarId is stored in Dict as 
   % belonging to arithmetic expression which is expressed as RangeVar-argument 
   % of lookup returning is/2. Type information is implicit through the is/2 functor

   lookup(VarId,Dict,is,LeftExpr,Type),
   check_type_compatible(Type,number),
   evaluable_expression(Expression,Dict,RightExpr,ExprType),
   check_type_compatible(ExprType,number),
   ArithComparison = [comp(LeftExpr,'=',RightExpr)].


translate_arithmetic_function('$const$'(Constant),Expression,ArithComparison,Dict,Dict):-
   % --- is/2 used to test whether left side evaluates to right side ----------------
   get_type('$const$'(Constant),ConstantType),
   check_type_compatible(ConstantType,number),
   evaluable_expression(Expression,Dict,ArithExpression,ExprType),
   check_type_compatible(ExprType,number),
   ArithComparison = [comp('$const$'(Constant),'=',ArithExpression)].

%% translate_comparison(LeftArg,RightArg,CompOp,Dict,SQLComparison) 
%
% translates the left and right arguments of a comparison term into the
% appropriate comparison operation in SQL. The result type of each 
% argument expression is checked for type compatibility

%:- mode translate_comparison(+,+,+,+,?) is det.
translate_comparison(LeftArg,RightArg,CompOp,Dict,Comparison):-
	evaluable_expression(LeftArg,Dict,LeftTerm,LeftArgType),
	evaluable_expression(RightArg,Dict,RightTerm,RightArgType),
	check_type_compatible(LeftArgType,RightArgType),
	Comparison = [comp(LeftTerm,CompOp,RightTerm)].

% CJM member/in support
translate_comparison(LeftArg,RightArgList,in,Dict,Comparison):-
	evaluable_expression(LeftArg,Dict,LeftTerm,LeftArgType),
	forall(member(RightArg,RightArgList),
                (   evaluable_expression(RightArg,Dict,RightTerm,RightArgType),
                    check_type_compatible(LeftArgType,RightArgType))),
	findall(RightTerm,
                (   member(RightArg,RightArgList),
                    evaluable_expression(RightArg,Dict,RightTerm,_)),
                RightTerms),
	Comparison = [comp(LeftTerm,in,RightTerms)].

%% translate_functor(Functor,QualifiedTableName) 
%
% translate_functor searches for the matching relation table name for
% a given functor and creates a unique range variable to result in
% a unique qualified relation table name.

translate_functor(Functor,Arity,rel(TableName,RangeVariable)):-
        relation(Functor,ActualArity,TableName),
        ActualArity >= Arity,   % we allow dropping of arguments [CJM]
	lgensym([TableName,'_'],RangeVariable). % CJM - friendlier aliases


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
   debug(sql_compiler,'  Translated arguments: ~w ~w ~w',[Arg,SQLTable,Where]),
   NewPosition is Position + 1,
   translate_arguments(Args,SQLTable,NewPosition,RestWhere,TmpDict,NewDict),
   append(Where,RestWhere,SQLWhere).




%% translate_argument(+Argument,+RelTable,+Position:int,?Condition,+Dict:list,?NewDict:list) 
%
% The first occurrence of a variable leads to its associated SQL attribute information
% to be recorded in the Dict. Any further occurrence creates an equi-join condition 
% between the current attribute and the previously recorded attribute.
% Constant arguments always translate to equality comparisons between an attribute and 
% the constant value.
%
% @param Argument $var$(Var) or $const$(Const)
% @param RelTable rel(Table,Alias)
% @param Position index to attribute/4, to retrueve type
% @param Condition 
% @param Dict
% @param NewDict
% ------------------------------------------------------------------------

translate_argument('$var$'(VarId),rel(SQLTable,RangeVar),Position,[],Dict,NewDict):-
        attribute(Position,SQLTable,Attribute,Type),
        add_to_dictionary(VarId,RangeVar,Attribute,Type,all,Dict,NewDict).

translate_argument('$var$'(VarId),rel(SQLTable,RangeVar),Position,AttComparison,Dict,Dict):-
        % --- Variable occurred previously - retrieve first occurrence data from dictionary -
        debug(sql_compiler,'  ** variable occurred previously: ~w',[VarId]),
        lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
        attribute(Position,SQLTable,Attribute,Type),
        check_type_compatible(PrevType,Type),
        AttComparison = [comp(att(RangeVar,Attribute),=,att(PrevRangeVar,PrevAtt))].

translate_argument('$const$'(Constant),rel(SQLTable,RangeVar),Position,ConstComparison,Dict,Dict):-
   % --- Equality comparison of constant value and attribute in table ---------------
        attribute(Position,SQLTable,Attribute,Type),
        get_type('$const$'(Constant),ConstType),
        debug(sql_compiler,'Checking for type compatibility ~w <=> ~w',[ConstType,Type]),
        check_type_compatible(ConstType,Type),
        debug(sql_compiler,'Compatible ~w <=> ~w',[ConstType,Type]),
        ConstComparison = [comp(att(RangeVar,Attribute),=,'$const$'(Constant))].




%% projection_term_variables(ProjectionTerm,Dict) 
%
% extracts all variables from the ProjectionTerm and places them into the
% Dict as a dict/4 term with their Identifier, a non instantiated RangeVar and 
% Attribute argument, and the keyword existential for the type of quantification

%:- mode projection_term_variables(+,?) is det.
projection_term_variables('$const(_)$',[]).

projection_term_variables('$var$'(VarId),[dict(VarId,_,_,_,existential)]).

% CJM: added to allow GROUP BY vars in projection term
projection_term_variables('$var$'(VarId) ^ ProjectionTerm, [dict(VarId,_,_,_,all)|ProjectionTermVariables]):-
%projection_term_variables('$var$'(VarId) ^ ProjectionTerm, ProjectionTermVariables):-
        !,
        projection_term_variables(ProjectionTerm,ProjectionTermVariables).

% compound terms
projection_term_variables(ProjectionTerm,ProjectionTermVariables):-
   ProjectionTerm =.. [Functor|ProjectionTermList],
   not (Functor = '$var$'),
   not (ProjectionTermList = []), % must be compound
   projection_list_vars(ProjectionTermList,ProjectionTermVariables).


projection_list_vars([],[]).
projection_list_vars(['$var$'(VarId)|RestArgs],Vars):-
	!,
        projection_list_vars(RestArgs,RestVars),
        H = dict(VarId,_,_,_,existential), % CJM - added the following to prevent dupes
        (   \+ member(H, RestVars)
        ->  Vars = [H|RestVars] % add new
        ;   Vars = RestVars).   % already have it
projection_list_vars(['$const$'(_)|RestArgs],Vars):-
	!,
   projection_list_vars(RestArgs,Vars).
% [CJM] : added this clause to allow for complex projection terms
projection_list_vars([H|RestArgs],Vars):-
	projection_term_variables(H,Vars1),
	projection_list_vars(RestArgs,Vars2),
	append(Vars1,Vars2,Vars). % TODO - uniqify






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

%% translate_projection(+ProjectionTerm, +Dict, ?SelectList) is det
translate_projection('$var$'(VarId),Dict,SelectList):-
   projection_arguments(['$var$'(VarId)],SelectList,Dict).

translate_projection('$const$'(Const),_,['$const$'(Const)]).

translate_projection(ProjectionTerm,Dict,SelectList):-
   ProjectionTerm =.. [Functor|Arguments],
   not (Functor = '$var$'),
   not (Functor = '$const$'),
   not (Arguments = []),
   projection_arguments(Arguments,SelectList,Dict).



%% projection_arguments(+Args,?SelectList,+Dict) is det

projection_arguments([],[],_).

projection_arguments([Arg|RestArgs],[Att|RestAtts],Dict):-
   retrieve_argument(Arg,Att,Dict),
   !, % [CJM]
   projection_arguments(RestArgs,RestAtts,Dict).

% [CJM]: extend to allow complex terms
projection_arguments([Arg|RestArgs],Atts,Dict):-
	translate_projection(Arg,Dict,Atts1),
	projection_arguments(RestArgs,Atts2,Dict),
	append(Atts1,Atts2,Atts).


%% retrieve_argument(Argument,SQLAttribute,Dictionary) 
%
% retrieves the mapping of an argument to the appropriate SQL construct, i.e.
%
%  - qualified attribute names for variables in base goals
%  - arithmetic expressions for variables in arithmetic goals
%  - constant values for constants

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
   (   Quant = all
   ->  true
   ;   nonvar(RangeVar),
       nonvar(Attribute)
   ).



%% add_to_dictionary(+Key,+RangeVar,+Attribute,+Quantifier,+Dict:list,?NewDict:list) is semidet
% e.g. add_to_dictionary(var1,feature_1,feature_id,all,[dict(var1,..)..],_)
% fails if Key is a member of the dictionary as Quantifier=all
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
        debug(sql_compiler,'Aggregrate ~w AggVar=~w AggGoal=~w',[AggFunctor,AggVar,AggGoal]),
        conjunction(AggGoal,AggConjunction),
        aggregate_query_generation(SQLFunction,AggVar,AggConjunction,Dict,AggregateFunctionExpression).


conjunction(Goal,Conjunction):-
   convert_to_disj_norm_form(Goal,[Conjunction]).




%% aggregate_query_generation(Function,FunctionVariable,AggGoal,Dict,AggregateQuery) 
%
% compiles the function variable (representing the attribute over which the aggregate 
% function is to be computed) and the aggregate goal (representing the selection and 
% join conditions for the computation of the aggregate function) to an SQL aggregate 
% function subquery.
% 
% ------------------------------------------------------------------------

%:- mode aggregate_query_generation(+,+,+,+,?) is det.
aggregate_query_generation(count,'$const$'('*'),AggGoal,Dict,AggregateQuery):-
   translate_conjunction(AggGoal,SQLFrom,SQLWhere,Dict,_TmpDict),
   % ATTENTION! It is assumed that in count(*) aggregate query terms there cannot be
   % free variables because '*' stands for "all arguments"
   AggregateQuery = agg_query(_Function,(count,['$const$'(*)]),SQLFrom,SQLWhere,[]).

% CJM: added for distinct
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
   debug(sql_compiler,'AggDict=~w',[AggDict]), % CJM: this appears not to be the correct thing to do
   % the set difference includes columns that are in the aggregate. Need to use '*'?
   
   translate_projection(FunctionVariable,AggDict,SQLSelect),
   translate_grouping(FunctionVariable,AggDict,SQLGroup), % ???
   debug(sql_compiler,'SQLGroup=~w',[SQLGroup]),
   % CJM NOTES:
   %  group_by is broken in original sql_compiler. The query is formed correctly, however,
   %  putting the grouping variable in the projection results in the variable being removed
   %  from the group clause!
   %  for now eliminate grouping entirely
%   AggregateQuery = agg_query(Function,SQLSelect,SQLFrom,SQLWhere,SQLGroup).
   AggregateQuery = agg_query(Function,SQLSelect,SQLFrom,SQLWhere,[]). 




%% translate_grouping(+FunctionVariable,+Dict,?SQLGroup) 
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
                (   member(dict(Var,Table,Attribute,_Type,all),Dict),
                    \+ member(dict(Var,_,_,_,_),FunctionVariableList)),
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


%% evaluable_expression(+ExpressionTerm,+Dictionary,?Expression,+Type) 
%
% evaluable_expression constructs SQL arithmetic expressions with qualified attribute names
% from the Prolog arithmetic expression term and the information stored in the dictionary.
%
% The type of an evaluable function is returned in the argument Type.
%
% The dictionary is not changed because it is used for lookup only. 
% 

evaluable_expression(AggregateFunctionTerm,Dictionary,AggregateFunctionExpression,Type):-
        debug(sql_compiler,'~w',[evaluable_expression(AggregateFunctionTerm,Dictionary,AggregateFunctionExpression,Type)]),
        fail.

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

% CJM: todo - generalize; try the code below. requires type_union
/* Ciao:
%% MH Unified all operators here. Also using type unions (previously was 
%% hardwired to 'num'). 
evaluable_expression(ArithExpr, Dictionary, SQLArithExpr, EType) :-
	ArithExpr =.. [PrologOp, LeftExp, RightExp],
	arithmetic_functor(PrologOp, SQLOp),
	evaluable_expression(LeftExp, Dictionary, LeftAr, LType),
	evaluable_expression(RightExp, Dictionary, RightAr, RType),
	check_type_union(LType, RType, EType),
	SQLArithExpr =.. [SQLOp, LeftAr, RightAr].
*/

evaluable_expression( log(LeftExp, RightExp), Dictionary, log(LeftAr / RightAr),number):-
   evaluable_expression(LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(RightExp,Dictionary,RightAr,number).

evaluable_expression('$var$'(VarId),Dictionary,att(RangeVar,Attribute),Type):-
   lookup(VarId,Dictionary,RangeVar,Attribute,Type),
   RangeVar \= is.

evaluable_expression('$var$'(VarId),Dictionary,ArithmeticExpression,Type):-
   lookup(VarId,Dictionary,is,ArithmeticExpression,Type).

evaluable_expression('$const$'(Const),_,'$const$'(Const),ConstType):-
   get_type('$const$'(Const),ConstType).

% CJM : TODO
evaluable_expression(case(TestExp,ThenExp,ElseExp),Dictionary,case(TestAr,ThenAr,ElseAr),number):-
        evaluable_expression(TestExp,Dictionary,TestAr,_),
        %check_translate_goal(TestExp,Dictionary,TestAr,_),
        %TestAr=TestExp,
        evaluable_expression(ThenExp,Dictionary,ThenAr,_),
        evaluable_expression(ElseExp,Dictionary,ElseAr,_).

% CJM 
evaluable_expression(concat(L,J,R),Dictionary,concat(LAr,J,RAr),varchar):-
        evaluable_expression(L,Dictionary,LAr,_),
        %check_translate_goal(TestExp,Dictionary,TestAr,_),
        %TestAr=TestExp,
        evaluable_expression(R,Dictionary,RAr,_).

% CJM 
evaluable_expression(substr(Str,From,To),Dictionary,substr(XStr,XFrom,XTo),varchar):-
        evaluable_expression(Str,Dictionary,XStr,_),
        evaluable_expression(From,Dictionary,XFrom,_),
        evaluable_expression(To,Dictionary,XTo,_).


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
	query_tokens(0,Query),
	[';'].
queries_tokens([Query|Queries]) -->
	query_tokens(0,Query),
	['\n UNION '],
	queries_tokens(Queries).

query_tokens(Tab,query([agg_query(Function, Select, From, Where, Group)], _, _)) -->
	% --- ugly rule here: aggregate function only in SELECT Part of query ----
	!,
	query_tokens(Tab,agg_query(Function, Select, From, Where, Group)).
query_tokens(Tab,query(Select, From, Where)) -->
	clause3_tokens(Tab,'SELECT DISTINCT', Select, ','),
	clause3_tokens(Tab,' FROM', From, ','),
	clause3_tokens(Tab,' WHERE', Where, 'AND').
query_tokens(Tab,agg_query('COUNT_DISTINCT', Select, From, Where, Group)) -->
	clause4_tokens_with_distinct(Tab,'SELECT', 'COUNT', Select, ','),
	clause3_tokens(Tab,' FROM', From, ','),
	clause3_tokens(Tab,' WHERE', Where, 'AND'),
	clause3_tokens(Tab,' GROUP BY', Group, ',').
query_tokens(Tab,agg_query(Function, Select, From, Where, Group)) -->
	clause4_tokens(Tab,'SELECT', Function, Select, ','),
	clause3_tokens(Tab,' FROM', From, ','),
	clause3_tokens(Tab,' WHERE', Where, 'AND'),
	clause3_tokens(Tab,' GROUP BY', Group, ',').
query_tokens(Tab,negated_existential_subquery(Select, From, Where)) -->
	['NOT EXISTS ('],
	{tabinc(Tab,NextTab)},
	query_tokens(NextTab,query(Select, From, Where)),
	[')'].

clause4_tokens(Tab,Keyword, Function, [Column], Separator) -->
	atom_tokens(Keyword),
	[' '],
	atom_tokens(Function),
	['('],
	{tabinc(Tab,NextTab)},
	clause2_tokens(NextTab,[Column], Separator),
	[')'].

% CJM: a bit hacky...
clause4_tokens_with_distinct(Tab,Keyword, Function, [Column], Separator) -->
	atom_tokens(Keyword),
	[' '],
	atom_tokens(Function),
	{tabinc(Tab,NextTab)},
	['(DISTINCT '],
	clause2_tokens(NextTab,[Column], Separator),
	[')'].

clause3_tokens(_Tab,_Keyword, [], _) --> [].
clause3_tokens(Tab,Keyword, [Column|RestColumns], Separator) -->
	['\n'],
	tabulate(' ',Tab),
	atom_tokens(Keyword),
	[' '],
	clause2_tokens(Tab,[Column|RestColumns], Separator).

clause2_tokens(Tab,[Item], _) -->
	['\n'],
	tabulate(' ',Tab),
	['  '],
	column_tokens(Tab,Item).
clause2_tokens(Tab,[Item, NextItem|RestItems], Separator) -->
	['\n'],
	tabulate(' ',Tab),
	['  '],
	column_tokens(Tab,Item),
	[' '],
	atom_tokens(Separator),
	[' '],
	clause2_tokens(Tab,[NextItem|RestItems], Separator).

column_tokens(_Tab,'*') -->
	['*'].
column_tokens(_Tab,att(rel1, Attribute)) --> !, % HACK FOR MySQL!!!
	atom_tokens(Attribute).
column_tokens(_Tab,att(RangeVar, Attribute)) -->
	atom_tokens(RangeVar),
	['.'],
	atom_tokens(Attribute).
column_tokens(_Tab,rel(Relation, rel1)) --> !, % HACK FOR MySQL!!!
        {expand_relation_name(Relation,RX)},
	atom_tokens(RX).
column_tokens(_Tab,rel(Relation, RangeVar)) -->
        {expand_relation_name(Relation,RX)},
	atom_tokens(RX),
	[' '],
	atom_tokens(RangeVar).
column_tokens(_Tab,'$const$'(String)) -->
	{ get_type('$const$'(String), string) }, !,
        esc_atom_tokens(String).
column_tokens(_Tab,'$const$'(Number)) -->
	{ get_type('$const$'(Number), NumType),
          debug(sql_compiler,'check_type_compat ~w ~w',[Number,NumType]),
	  %check_type_compatible(NumType, num) TODO!!
          true
	},
	atom_tokens(Number).
column_tokens(Tab,case(Test, Then, Else)) -->
        !,
	{tabinc(Tab,NextTab)},
        [' CASE WHEN '],
	column_tokens(NextTab,Test),
	[' THEN '],
	column_tokens(NextTab,Then),
	[' ELSE '],
	column_tokens(NextTab,Else),
        [' END CASE '].
column_tokens(Tab,substr(Str, From, Len)) -->
        !,
        [' SUBSTR( '],
	column_tokens(Tab,Str),
	[', '],
	column_tokens(Tab,From),
	[', '],
	column_tokens(Tab,Len),
        [' ) '].

/*
column_tokens(Tab,concat(Left, Join, Right)) -->
        !,
        [' CONCAT( '],
	column_tokens(Tab,Left),
	[', '],
	esc_atom_tokens(Join),
	[', '],
	column_tokens(Tab,Right).
        [' ) '].
*/
column_tokens(Tab,comp(LeftArg, in, RightArgs)) --> % CJM
        !,
	column_tokens(Tab,LeftArg),
	[' IN '],
	[' ( '],
        clause2_tokens(Tab,RightArgs, ','),
        [' ) '].
column_tokens(Tab,comp(LeftArg, Operator, RightArg)) -->
	column_tokens(Tab,LeftArg),
	[' '],
	atom_tokens(Operator),
	[' '],
	column_tokens(Tab,RightArg).
column_tokens(Tab,null(Arg)) --> % CJM
	column_tokens(Tab,Arg),
	[' IS NULL'].
column_tokens(Tab,not_null(Arg)) --> % CJM
	column_tokens(Tab,Arg),
	[' IS NOT NULL'].
% TODO: make this more generic
column_tokens(Tab,LeftExpr * RightExpr) -->
	column_tokens(Tab,LeftExpr),
	optnltab(Tab,RightExpr),
	[' * '],
	column_tokens(Tab,RightExpr).
column_tokens(Tab,LeftExpr / RightExpr) -->
	column_tokens(Tab,LeftExpr),
	optnltab(Tab,RightExpr),
	[' / '],
	column_tokens(Tab,RightExpr).
column_tokens(Tab,LeftExpr + RightExpr) -->
	column_tokens(Tab,LeftExpr),
	optnltab(Tab,RightExpr),
	[' + '],
	column_tokens(Tab,RightExpr).
column_tokens(Tab,LeftExpr - RightExpr) -->
	column_tokens(Tab,LeftExpr),
	optnltab(Tab,RightExpr),
	[' - '],
	column_tokens(Tab,RightExpr).
column_tokens(Tab,agg_query(Function, Select, From, Where, Group)) -->
	{tabinc(Tab,NextTab)},
	['('],
	query_tokens(NextTab,agg_query(Function, Select, From, Where, Group)),
	[')'].
column_tokens(Tab,negated_existential_subquery(Select, From, Where)) -->
	{tabinc(Tab,NextTab)},
	query_tokens(NextTab,negated_existential_subquery(Select, From, Where)).

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

% for complex expressions, start with a newline then tab
optnltab(Tab,agg_query(_,_,_,_,_)) --> !,['\n'],tabulate(' ',Tab),['  '].
optnltab(_,_) --> [].

tabinc(Tab,NextTab):- NextTab is Tab+8.
tabulate(_,0) --> !,[].
tabulate(A,N) --> {Nm1 is N-1},[A],tabulate(A,Nm1).


% ----------------------------------------------------------------
% Conversion of SQL term to string
% ----------------------------------------------------------------

%% sqlterm2atom(+SQLQueryTerm, ?SQLQueryAtom) is det
% translates an SQL Term to an atom that conforms to SQL syntax.
% see plterm_to_sqlterm/3
% [Ciao: sqlterm2string]
sqlterm2atom(SQLQueryTerm, SQLQueryAtom) :-
                                % Original code fails on some SQLQueryTerms for which queries_dstring/1 succeeds!
                                % Doing this serious kludge instead for now:
	queries_tokens(SQLQueryTerm, SQLQueryTokens, []),
        concat_atom(SQLQueryTokens,SQLQueryAtom),
        !.
sqlterm2atom(SQLQueryTerm, _) :-
        throw(cannot_translate_sqlterm(SQLQueryTerm)).



% Meta Database for schema definition of SQL DB in Prolog
%
% maps Prolog predicates to SQL table names, Prolog predicate argument positions to SQL
% attributes, and Prolog operators to SQL operators. 
%
% ATTENTION! It is assumed that the arithmetic operators in Prolog and SQL are the same,
% i.e. + is addition in Prolog and in SQL, etc. If this is not the case, then a mapping
% function for arithmetic operators is necessary too.

%% relation(+PrologFunctor,+Arity,SQLTableName) 

relation(F,A,F):- relation(F,A).
relation(F,A,F2):- \+ relation(F,A),relation(F2,A),downcase_atom(F2,Fp),downcase_atom(F,Fp).


% ------------------------------------------------------------------------
%
% Output to screen predicates - rather crude at the moment
%
% ------------------------------------------------------------------------

%:- mode print_sqlterm(+) is det.
print_sqlterm(T):-
        sqlterm2atom(T,S),
        writeln(S).

%% lgensym(Root,Symbol) 
lgensym(L,Symbol):-
        concat_atom(L,Root),
        gensym(Root,Symbol).




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


% Mapping of Prolog operators to SQL operators

comparison(=,=).
comparison(\=,'!=').
comparison('iz','IS').          % CJM
comparison(<,<).
comparison(>,>).
comparison(@<,<).
comparison(@>,>).
comparison(=<,'<=').            % CJM
comparison(>=,>=).              % CJM
comparison(in,in).              % CJM


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
aggregate_functor(stddev,'STDDEV'). % CJM
aggregate_functor(count,'COUNT').
aggregate_functor(count_distinct,'COUNT_DISTINCT'). % CJM: ugly, this is translated later




% type system 
%
% A rudimentary type system is provided for consistency checking during the
% translation and for output formatting
%
% The basic types are string and number. number has the subtypes integer and
% real.
%
% TODO: fix this so it's not a hack. see sqltypes

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

rsubtype(X,X).
rsubtype(SubType,SuperType):-
        subtype(SubType,SuperType).



subtype(SubType,SuperType):-
   is_subtype(SubType,SuperType).

subtype(SubType,SuperType):-
   is_subtype(SubType,InterType),
   subtype(InterType,SuperType).

check_type_compatible(TypeA, TypeB) :- 
	type_compatible(TypeA, TypeB),
	!.
check_type_compatible(TypeA, TypeB) :- 
	error_message("incompatible types ~w, ~w", [TypeA, TypeB]),
	fail.


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
is_subtype(tinyint,integer).
is_subtype(integer,number).
is_subtype('integer unsigned',integer).
is_subtype('integer unsigned',number).
is_subtype('smallint unsigned',number).
is_subtype('smallint unsigned',integer).
is_subtype('tinyint unsigned',integer).
is_subtype('tinyint unsigned',number).
is_subtype(real,number).
is_subtype(natural,integer).
is_subtype(bool,string).        % not quite right but seems to be required..


%% get_type(+Constant,?Type)
%
% Prolog implementation specific definition of type retrieval
% sepia Prolog version given here

get_type('$const$'(Constant),integer):-
   number(Constant).

get_type('$const$'(Constant),string):-
   atom(Constant).

error_message(Fmt,Args):-
        sformat(Msg,Fmt,Args),
        throw(sql(Msg)).

% for debugging
pp_prologterm(Term) :-
	pp_prologterm([],Term).

pp_prologterm(Tabs,L) :-
	is_list(L),
	!,
	maplist(write,Tabs),
	format('[~n'),
	maplist(pp_prologterm([' '|Tabs]),L),
	maplist(write,Tabs),
	format(']~n').
	      
pp_prologterm(Tabs,Term) :-
	var(Term),
	!,
	maplist(write,Tabs),
	writeln(Term).

pp_prologterm(Tabs,Term) :-
	Term =.. [_|Args],
	forall(member(A,Args),
	       (   atomic(A)
	       ;   A='$const$'(_)
	       ;   A='$var$'(_))),
	!,
	maplist(write,Tabs),
	format('~w~n',[Term]).

pp_prologterm(Tabs,Term) :-
	Term =.. [F|Args],
	maplist(write,Tabs),
	format('~w~n',[F]),
	maplist(pp_prologterm([' '|Tabs]),Args).

	
/** <module> Mapping of prolog terms to SQL

  ---+ Description

  This module rewrites prolog goals as SQL goals. It assumes the details of the schema are given using the following predicates:
  
  * relation/2 
  * attribute/4
  * unique/2 

  Some example schemas are bundled with blipkit:

  * sql_schema/schema_go.pro
  * sql_schema/schema_enscore44.pro

  There are also sqlmap modules that *bridge* between the schema and blipkit models. See for example

  * genomic/genome_sqlmap_enscore.pro
  * ontol/ontol_sqlmap_go.pro

  ---+ TODO

  problem with existentials being in the projection

  works:

  ==
  blip-sql -debug sql_compiler  -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature -r rdb/flybase prolog-to-sql -proj T "feature(G),seqfeature_db:feature_relationship(G,T,Type,Rank)"
  ==
  
  misses join:

  ==
  blip-sql -debug sql_compiler  -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature -r rdb/flybase prolog-to-sql  "feature(G),seqfeature_db:feature_relationship(G,T,Type,Rank)"
   ==
  
  ---++ Recursive SQL

  PostgreSQL8.4 allows WITH RECURSIVE; e.g.

==
tree(?ID,?Parent)

struct(ID,Parent) :- struct(ID,Parent).
struct(ID,A) :- struct(ID,Parent),struct(Parent,A).
==

should translate to:

==
WITH RECURSIVE struct AS (
SELECT t.* FROM tree t WHERE id = 890
UNION ALL
SELECT t.* FROM tree t, struct s WHERE t.id = s.parent_id
)
SELECT * FROM struct;
==
  
  ---+ Author

 This modules is an extension of version 1.1 (Dec 21st 1992) of the
 Prolog to SQL compiler written by Christoph Draxler of the Univeristy of Munich.

                    Christoph Draxler
                    CIS Centre for Information and Speech Processing
                    Ludwig-Maximilians-University Munich
                    Wagmuellerstr. 23 
                    D 80538 Munich
                    Tel : ++49 / +89 / 211 06 64 (-60)
                    Fax : ++49 / +89 / 211 06 74
                    Mail: draxler@cis.uni-muenchen.de

 It was modified by Chris Mungall to be compatible with SWI-Prolog and
 extended to support:

 * MySQL specific column types
 * SELECT DISTINCT
 * query rewriting for optimization
 * additional comparison operators

 Permission has been granted by the original author to distribute this
 software using the same license as SWI-Prolog.
 
 RELEASE INFORMATION
 Current version is v. 1.1 of Dec. 21st 1992.
 Version 1.0 Sept. 3 1992
 CJM mods 2005
*/

