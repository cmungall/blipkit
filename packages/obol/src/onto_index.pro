
:- use_module(library(porter_stem)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(mode)).

atom_markup(A,L,Opts):-
        atom_to_stem_list(A,Toks),
        tokens_markup(Toks,L,Opts).

tokens_markup(Toks,[class(C)|ML],Opts):-
        class_lindex(C,Toks,Len,Tail),
        \+ ( (class_lindex(_,Toks,Len2,_),Len2>Len)),
        !, % todo : longest
        tokens_markup(Tail,ML,Opts).
tokens_markup([Tok|Toks],[Tok|ML],Opts):-
        tokens_markup(Toks,ML,Opts).
tokens_markup([],[],_).

class_lindex(C,Toks,Len,Tail):-
        entity_label(C,Label),
        atom_to_stem_list(Label,HeadToks),
        length(HeadToks,Len),
        append(HeadToks,Tail,Toks).

:- begin_tests(grep).
:- use_module(library(lists)).

test(reverse) :-
        reverse([a,b], [b,a]).

:- end_tests(grep).

        