:- module(lego_db,
          [
           action/1,
           action_type/2,
           process/1,
           activates/2,
           inhibits/2,
           is_part_of/2,
           is_part_of_some/2,
           occurs_in/2,
           enabled_by/2,
           lego_symbol/2,
           
           is_part_of_some/2,
           action_frame/2,

           lego_summary/5
           ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util),[call_unique/1,solutions/3]).

:- extensional(action/1).
:- extensional(action_type/2).
:- extensional(process/1).
:- extensional(activates/2).
:- extensional(inhibits/2).
:- extensional(is_part_of/2).
:- extensional(occurs_in/2).
:- extensional(enabled_by/2).
:- extensional(lego_symbol/2).

action_frame(E,SVs) :-
        setof(S=V,action_sv(E,S,V),SVs).

action_sv(E,occurs_in,X) :-
        occurs_in(E,X).

action_sv(E,enabled_by,X) :-
        enabled_by(E,X).

action_sv(E,type,X) :-
        action_type(E,X).

action_sv(E,is_part_of_some,X) :-
        is_part_of_some(E,X).

action_sv(E,activates,X) :-
        activates(E,X).


is_part_of_some(E,C) :- is_part_of(E,X),action_type(X,C).

activates_or_inhibits(X,Y) :- activates_or_inhibits(X,Y,_).
activates_or_inhibits(X,Y,activate) :- activates(X,Y).
activates_or_inhibits(X,Y,inhibits) :- inhibits(X,Y).


:- multifile dbmeta:fact_chain_hook/2.
dbmeta:fact_chain_hook(process(X),
                       [is_part_of(_,X),
                        lego_symbol(X,_),
                        metadata_db:entity_label(X,_)]).
dbmeta:fact_chain_hook(is_part_of(P,_),
                       [is_part_of(_,P),
                        process(P),
                        action(P)]).
dbmeta:fact_chain_hook(action(X),
                       [action_type(X,_),
                        activates(X,_),
                        inhibits(X,_),
                        is_part_of(_,X),
                        occurs_in(X,_),
                        enabled_by(X,_),
                        lego_symbol(X,_),
                        metadata_db:entity_label(X,_)]).

dbmeta:fact_chain_hook(enabled_by(_,Y),
                       [
                        lego_symbol(Y,_),
                        metadata_db:entity_label(Y,_)]).

%dbmeta:fact_chain_hook(action_type(X,T),[]).
%dbmeta:fact_chain_hook(process(X),[]).
%dbmeta:fact_chain_hook(activates(X,Y),[]).
%dbmeta:fact_chain_hook(inhibits(X,Y),[]).
%dbmeta:fact_chain_hook(is_part_of(X,Y),[]).
%dbmeta:fact_chain_hook(occurs_in(X,Y),[]).

lego_summary(NumAction, NumTypedAction, PropActionTyped, PropActionEnabled, PropActionActivator) :-
        aggregate(count,X,action(X),NumAction),
        aggregate(count,X,T^action_type(X,T),NumTypedAction),
        PropActionTyped is NumTypedAction/NumAction,
        aggregate(count,X,T^enabled_by(X,T),NumEnabledAction),
        PropActionEnabled is NumEnabledAction/NumAction,
        aggregate(count,X,T^activates_or_inhibits(X,T),NumActivatorAction),
        PropActionActivator is NumActivatorAction/NumAction.
        
        
        


/** <module> represents biological legos

  ---+ Synopsis

==
:- use_module(bio(lego_db)).

% 
demo:-
  nl.
  

==

---+ Details

See exploring_legos.txt

---+ Additional Information



*/
