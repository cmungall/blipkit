:- use_module(bio(dbmeta)).

write_extensional_preds(Schema):-
        dbmeta:datapred_spec(Schema,Spec),
        writespec(Spec),
        fail.

writespec(Spec-Desc):-
        !,
        writespec1(Spec),
        format('%    ~n'),
        format('%    ~w~n',Desc),
        writespec2(Spec).

writespec(Spec):-
        !,
        writespec1(Spec),
        writespec2(Spec).

writespec1(Spec):-
        Spec=..[P|Args],
        maplist(arg2spec,Args,Args2),
        maplist(atom_concat('?'),Args2,Args3),
        concat_atom(Args3,',',ArgSpec),
        format('%%  ~w(~w) is nondet~n',[P,ArgSpec]).

writespec2(Spec):-
        functor(Spec,P,A),
        format(':- extensional(~w/~w).~n~n',[P,A]).

arg2spec(A1,A2):-
        A1=..[_,A2|_],
        !.
arg2spec(A,A).



