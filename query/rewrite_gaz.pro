:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

all:-
        add_envo_is_a,
        rewrite_is_a.

% Lake Nyar
add_envo_is_a:-
        forall(   (restriction(C,located_in,_),
                   debug(gaz,'checking ~w',[C]),
                   \+ subclass(C,_),
                   \+ subclass(_,C),
                   type_from_name(C,SC)),
                  assert(ontol_db:subclass(C,SC))).

rewrite_is_a:-
        forall((subclass(Inst,Class),\+subclass(_,Inst)),
               (   
                   retractall(ontol_db:subclass(Inst,Class)),
                   assert(ontol_db:inst_of(Inst,Class)),
                   retractall(ontol_db:class(Inst)),
                   assert(ontol_db:inst(Inst)))).

                  
% Lake Nyar
type_from_name(C,SC):-
        entity_label(C,N),
        downcase_atom(N,Nd),
        concat_atom([SCN|_],' ',Nd),
        entity_label(SC,SCN),
        debug(gaz,'~w < ~w',[N,SCN]).

% 
type_from_name(C,SC):-
        entity_label(C,N),
        downcase_atom(N,Nd),
        concat_atom(Toks,' ',Nd),
        reverse(Toks,[SCN|_]),
        entity_label(SC,SCN),
        debug(gaz,'~w < ~w',[N,SCN]).


