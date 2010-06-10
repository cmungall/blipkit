:- module(ontol_bridge_to_rules,[
                                 ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(bioprolog_util),[solutions/3]).

% experimental long-forgotten module?

ontol_db:class_reified_rulebody(C,Xs,Conj):-
        class(C),
        \+genus(C,_),
        setof(X,differentium(C,card(has_part,1,1),X),Xs),
        findall(differentium(X,R,Y),(member(X,Xs),differentium(X,R,Y)),DiffTerms),
        findall(genus(X,G),(member(X,Xs),genus(X,G)),GenusTerms),
        append(DiffTerms,GenusTerms,Conj).

reified_rulebody_to_prolog(ReifiedConjTerms,Parts,PartVars,Body):-
        solutions(X-_,member(genus(X,_),ReifiedConjTerms),Dict),
        dictlookup(Parts,PartVars,Dict),
        debug(ontol_rules,'reified rule body: ~w',[ReifiedConjTerms]),
        reifconj_prolog(ReifiedConjTerms,Body,[],Dict),
        debug(ontol_rules,'prolog rule body: ~w',[Body]).

ontol_db:class_instrule(C,PartVars,Body):-
        class_reified_rulebody(C,Parts,Conj),
        reified_rulebody_to_prolog(Conj,Parts,PartVars,Body).


dictlookup([],[],_).
dictlookup([X|Xs],[V|Vs],Dict):-
        member(X-V,Dict),
        dictlookup(Xs,Vs,Dict).


%% reifconj_prolog(+Terms,?Body,+UnifiedVars,+Dict)
reifconj_prolog([Term],Rule,_,Dict):-
        !,
        reifterm_prolog(Term,Rule,_,Dict).
reifconj_prolog(Terms,(B1,B2),UnifiedVars,Dict):-
        select(Term,Terms,Rest),
        reifterm_prolog(Term,B1,Vars,Dict),
        member(Var,Vars),
        member(Var,UnifiedVars),
        !,
        debug(ontol_rules,'  using: ~w ~w',[Var,B1]),
        append(Vars,UnifiedVars,NewUnifiedVars),
        reifconj_prolog(Rest,B2,NewUnifiedVars,Dict).
reifconj_prolog([Term|L],(B1,B2),UnifiedVars,Dict):- % could not make optimal choice, or first term
        !,
        reifterm_prolog(Term,B1,Vars,Dict),
        debug(ontol_rules,'  [no opt] using: ~w',[B1]),
        append(Vars,UnifiedVars,NewUnifiedVars),
        reifconj_prolog(L,B2,NewUnifiedVars,Dict).

reifterm_prolog(differentium(X,R,Y),inst_rel(XV,R,YV),[X,Y],Dict):-
        member(X-XV,Dict),
        member(Y-YV,Dict),
        !.
reifterm_prolog(genus(X,G),inst_of(XV,G),[X],Dict):-
        member(X-XV,Dict),
        !.
reifterm_prolog(Term,_,_,Dict):-
        debug(ontol_rules,'  dict: ~w',[Dict]),        
        throw(cannot_unreify(Term)).

