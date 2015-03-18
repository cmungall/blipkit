:- module(tabmatcher,
          [
           show_tabmatches/1,
           show_tabmatches/2,
           show_tabmatches/3,
           show_tabmatches/4,
           tabmatch/5
           ]).

:- use_module(metadata_db).
:- use_module(metadata_nlp).
:- use_module(bio(simmatrix)).
:- use_module(bio(index_util)).
:- use_module(bio(ontol_db)). % consider moving the one dependency on this
:- use_module(library(porter_stem)).
:- use_module(bio(av_db)).  % obol dependency - consider moving this

show_tabmatches(Pred) :-
        show_tabmatches(Pred,_).
show_tabmatches(Pred,Arity) :-
        check_arity(Pred,Arity),
        findall(m,between(1,Arity,_),Cols),
        show_tabmatches(Pred,Arity,Cols).
show_tabmatches(Pred,Arity,Cols) :-
        show_tabmatches(Pred,Arity,Cols,[]).
show_tabmatches(Pred,Arity,Cols,Opts) :-
        check_arity(Pred,Arity),
        do_indexing(Opts),
        tabmatch(Pred,Arity,Cols,Result,Opts),
        show_factrow([isNoPred(1)|Opts],Result),
        fail.

do_indexing(Opts) :-
        option(no_index(true),Opts),
        !.
do_indexing(_) :- nlp_index_all,!.

tabmatch(Pred,Arity,Cols,Result,Opts) :-
        functor(G,Pred,Arity),
        G,
        G =.. [Pred|Args],
        lmatch(Args,Cols,RL,Opts),
        Result =.. [Pred|RL].

%% lmatch(+ArgsToProcess:list, +ColMatchTypes:list, ?ProcessedColVals:lsit ,+Opts:list)
lmatch([],_,[],_) :- !.
lmatch(Args,[],Args,_) :- !.  % no more cols to match
lmatch([Arg|Args],[Col|Cols],[Val,Arg|RL],Opts) :-
        matchtype(Col,MT),
        !,
        amatch(Arg,MT,Val),
        lmatch(Args,Cols,RL,Opts).
lmatch([Arg|Args],[_|Cols],[Arg|RL],Opts) :-
        % do not try to match Arg
        lmatch(Args,Cols,RL,Opts).

amatch(Arg,_,E) :-
        term_nlabel_stemmed(Arg,NLabel,false),
        entity_nlabel_scope_stemmed(E,NLabel,_,false),
        !.
amatch(Arg,MOpts,E) :-
        option(stemmed(true),MOpts),
        term_nlabel_stemmed(Arg,NLabel,true),
        entity_nlabel_scope_stemmed(E,NLabel,_,true),
        % TODO - take best
        !.
amatch(_,_,'').

matchtype(nostem,[stemmed(false)]).
matchtype(stem,[stemmed(true)]).
matchtype(m,[stemmed(true)]).

check_arity(_,Arity) :-
        ground(Arity),
        !.
check_arity(Pred,Arity) :-
        findarity(Pred,Arity).

findarity(Pred,Arity) :-
        findarity(Pred,1,Arity).
findarity(Pred,Arity,Arity) :-
        functor(G,Pred,Arity),
        clause(G,_),
        !.
findarity(Pred,Test,_) :-
        Test > 1000,
        !,
        throw(no_pred(Pred)).
findarity(Pred,Test,Arity) :-
        TestPlus1 is Test+1,
        findarity(Pred,TestPlus1,Arity).




