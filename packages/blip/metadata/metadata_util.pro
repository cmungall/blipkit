:- module(metadata_util,[map_identifiers/4]).
:- use_module(metadata_db).


%:- module_transparent map_identifiers/4.

map_identifiers(Module,P,KeyArg,NumArgs):-
        functor(Fact,P,NumArgs),
        Fact=..[P|Args],
        nth1(KeyArg,Args,ArgTemplate),
        findall(Fact-NewFact,
                (   Module:Fact,
                    entity_alternate_identifier(NewKeyVal,ArgTemplate),
                    replace_nth1(Args,KeyArg,NewKeyVal,Args2),
                    NewFact=..[P|Args2]),
                ReplaceList),
        maplist(replace_fact_with(Module),ReplaceList).

replace_nth1([_|Args],1,NewKeyVal,[NewKeyVal|Args]).
replace_nth1([Arg|Args],Index,NewKeyVal,[Arg|NewArgs]):-
        Index>1,
        NewIndex is Index-1,
        replace_nth1(Args,NewIndex,NewKeyVal,NewArgs).

replace_fact_with(Module,OldFact-NewFact):-
        debug(metadata,'Replacing ~w with ~w',[OldFact,NewFact]),
        Module:retract(OldFact),
        Module:assert(NewFact).

