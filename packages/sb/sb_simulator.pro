:- module(sb_simulator,[
                        evaluate_expr/3,
			simulate/0
		       ]).

:- use_module(bio(sb_db)).
:- use_module(bio(bioprolog_util)).

%% initialise_variables(?VarBindings)
% query sbentity_param/3 for initial amounts
initialise_variables(VarBindings):-
	findall(Var=Val,initial_amount(Var,Val),VarBindings).

initial_amount(Var,Val):-
        sbentity_param(Var,initialAmount,Val).
initial_amount(Var,Val):-
        sbentity_param(Var,initialConcentration,Val).


%% replace_var(+Var,+Val,+InVarBindings,?OutVarBindings) is det
% set a state variable
replace_var(Var,Val,VarBindingsPrev,[Var=Val|VarBindingsNew]):-
	select(Var=_,VarBindingsPrev,VarBindingsNew),
	!.

%% variable_value(+Var,?Val,?VarBindings:list) is semidet
% looks up Var in either (preferentially) VarBindings or parameter/4 
variable_value(Var,Val,VarBindings):-
	member(Var=Val,VarBindings),
	!.
variable_value(Var,Val,_VarBindings):-
	parameter(Var,_,_,Val),
	!.

%% execute_reaction(+Reaction,+InVarBindings,?OutVarBindings) is det
% @param Reaction must be named in kinetic_law/2. e.g. cyclin_1
execute_reaction(R,VarBindings1,VarBindings2):-
	!,
	kinetic_law(R,MathExpr),
	evaluate_expr(MathExpr,VarBindings1,Rate),
	debug(sb_simulate,'Reaction: ~w Expr: ~w -> ~w',[R,MathExpr,Rate]),
	apply_rate(R,Rate,VarBindings1,VarBindings2).

%% apply_rate(+Reaction,+Rate:float,+InVarBindings,?OutVarBindings) is det
apply_rate(R,Rate,VarBindings1,VarBindings2):-
	!,
	findall(Role-P,reaction_participant(R,Role,P),RPs),
	apply_rate_to_participants(Rate,RPs,VarBindings1,VarBindings2),
	debug(sb_simulate,'  applied rate: ~w to all. // ~w // ~w',[Rate,RPs,VarBindings2]).

%% apply_rate_to_participants(+Rate:float,+RoleParticipantPairs:list,+InVarBindings,?OutBindings) is det
% @param RoleParticipantPairs list of Role-Participant, where Role = modifier | reactant | product
apply_rate_to_participants(_,[],VarBindings,VarBindings):- !. % base case
apply_rate_to_participants(Rate,[Role-P|RPs],VarBindings1,VarBindings2):-
	!,
	debug(sb_simulate,'    applying: Rate: ~w to ~w ~w',[Rate,Role,P]),
	apply_rate_to_participant(Rate,Role,P,VarBindings1,VarBindingsX),
	apply_rate_to_participants(Rate,RPs,VarBindingsX,VarBindings2).

%% apply_rate_to_participant(+Rate:float,+Role,+Participant,+InVarBindings,?OutBindings) is det
% @param Role one of: modifier | reactant | product
% @param Participant name of variable representing species. e.g. RS_1
% nothing happens to modifiers. reactants decreased. products increased.
apply_rate_to_participant(_,modifier,_,VarBindings,VarBindings):- !.
apply_rate_to_participant(Rate,reactant,P,VarBindings1,VarBindings2):-
	!,
	variable_value(P,CurrVal,VarBindings1),
        %NewVal is CurrVal - CurrVal*Rate,
        NewVal is CurrVal - Rate,
	replace_var(P,NewVal,VarBindings1,VarBindings2).
apply_rate_to_participant(Rate,product,P,VarBindings1,VarBindings2):-
	!,
	variable_value(P,CurrVal,VarBindings1),
        % NewVal is CurrVal + CurrVal*Rate,
        NewVal is CurrVal + Rate,
	replace_var(P,NewVal,VarBindings1,VarBindings2).


%% evaluate_expr(+Expr,+VarBindings:list,?Val) is det
% @param Expr prolog term corresponding to MathML structure
evaluate_expr(Val,_,Val):-
	number(Val),
	!.
evaluate_expr(Var,VarBindings,Val):-
	atomic(Var),
	assignment_rule(Var,Math),
	!,
	evaluate_expr(Math,VarBindings,Val).
evaluate_expr(Var,VarBindings,Val):-
	atomic(Var),
	%debug(sb_simulate,'     Atom: ~w',[Var]),
	!,
	variable_value(Var,Val,VarBindings).
evaluate_expr(Math,VarBindings,Val):-
	compound(Math),
	!,
	Math =.. [F,[Accum|Args]],
	%debug(sb_simulate,'   Eval: ~w // ~w ~w ~w',[Math,F,Args,Accum]),
	evaluate_expr_accum(F,Args,Accum,VarBindings,Val).

evaluate_expr_accum(_,[],Accum,_,Accum):- !.
evaluate_expr_accum(F,[A|Args],Accum,VarBindings,Val):-
	!,
	evaluate_expr(A,VarBindings,AVal),
	evaluate_expr(Accum,VarBindings,AccumVal),
	Expr =.. [F,AccumVal,AVal],
	%debug(sb_simulate,'     Accum expr: ~w',[Expr]),
	NextVal is Expr,
	evaluate_expr_accum(F,Args,NextVal,VarBindings,Val).

%% simulate
% initialise_variables/1 then simulate/1
simulate:-
	initialise_variables(VarBindings),
	simulate(VarBindings).

%% simulate(+VarBindings:list)
% TODO: way to exist infinite loop
simulate(VarBindings):-
	!,
	maplist(writeln,VarBindings),
	forall(assignment_rule(Var,Math),
	       (   evaluate_expr(Math,VarBindings,Val),
		   writeln(Var=Val))),
	nl,
	solutions(R,reaction(R,_),Rs),
	simulate(VarBindings,Rs).
%% simulate(+VarBindings:list,+Reactions:list)
simulate(VarBindings,[]):-
	!,
	simulate(VarBindings).
simulate(VarBindings,[R|Rs]):-
	!,
	debug(sb_simulate,' Reaction: ~w',[R]),
	execute_reaction(R,VarBindings,VarBindingsNew),
	%debug(sb_simulate,' New bindings: ~w',[VarBindingsNew]),
	simulate(VarBindingsNew,Rs).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

% mitotic cell cycle
% easy one: only one compartment. no reversible reactions
unittest(load(bm168)=
      load_biofile(sbml,'BIOMD0000000168.xml')/[]).

unittest(test(load_file,
            [_=load(bm168)],
            (   ensure_loaded(bio(sb_db)),
                setof(ID,N^reaction(ID,N),IDs),
                length(IDs,NumIDs)),
             (NumIDs=47))).

unittest(test(sim,
            [_=load(bm168)],
            (   ensure_loaded(bio(sb_db)),
		simulate,
                setof(ID,N^reaction(ID,N),IDs),
                length(IDs,NumIDs)),
             (NumIDs=47))).

unittest:exprt(+([a,b]),[a=1,b=2],3).
unittest:exprt(+([a,b,c]),[a=1,b=2,c=7],10).
unittest:exprt(*([a,b,c]),[a=1,b=2,c=7],14).
unittest:exprt(-([a,*([b,c])]),[a=1,b=2,c=7],-13).
unittest(test(expr,
            [],
            (   ensure_loaded(bio(sb_simulator)),
		forall(unittest:exprt(E,Vs,TrueResult),
                       (   evaluate_expr(E,Vs,Result),
                           format('~w = ~w for ~w~n',[E,Result,Vs]),
                           Result=TrueResult)),
                nl),
            true)).


/** <module> run a simulation -- EXPERIMENTAL

  ---+ Synopsis

==
:- use_module(bio(sb_simulator)).

% 
demo:-
  nl.
  

==

---+ Details



@author  Chris Mungall
@version $Revision$
@see     README, sb_db.pro
@license License


*/
