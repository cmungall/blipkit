/* -*- Mode: Prolog -*- */


:- module(seqfeature_writer_xsb,[
                                 write_all_as_xsb/0
                                 ]).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).

% semi-hack: indicates that this writes to stdout
io:redirect_stdout(seqfeature_xsb).

io:write_all(seqfeature_xsb,_,_):-
        write_all_as_xsb.
        
write_all_as_xsb:-
        forall(feature(F),
               write_feature(seqfeature_xsb,F)).

write_feature(seqfeature_xsb,F) :-
        feature_strand(F,-1),
        !. % do nothing for now! TODO

write_feature(seqfeature_xsb,F) :-
        feature_type(F,Type),
        type_pred(Type,Pred,F),
        feature_xsbid(F,ID),
        InstFact=..[Pred,ID],
        write_fact(InstFact),
        forall(featureloc(F,Src,S,E,Str),
               write_featureloc(ID,F,Src,S,E,Str)),
        forall(feature_relationship(F,P),
               write_feature_relationship(F,ID,P)).

write_featureloc(ID,F,Src.S,E, -1) :-
        !,
        write_featureloc(ID,F,S,E).
write_featureloc(ID,F,_,S,E, _) :-
        !,
        write_featureloc(ID,F,S,E).

write_featureloc(ID,F,S,E) :-
        feature_type(F,FT),
        subclassRTN(FT,transcript),
        !,
        write_fact(boundary_start_on_genome(ID,S)),
        write_fact(boundary_end_on_genome(ID,E)).
write_featureloc(ID,_,S,E) :-
        write_fact(on_genome(ID,S,E)).
%        write_fact(start_on_genome(ID,S)),
%        write_fact(end_on_genome(ID,E)).


write_feature_relationship(_F,ID,P) :-
        feature_xsbid(P,PID),
        feature_type(P,PT),
        class(PTID,PT),
        class(TTID,transcript),
        subclassRT(PTID,TTID),
        !,
        write_fact(transcribed_in(ID,PID)). % TODO
write_feature_relationship(_F,_,_) :- !.    % TODO

               

feature_xsbid(F,F):- atomic(F),!.
feature_xsbid(F,ID):-
        feature_type(F,T),
        feature_relationship(F,P),
        concat_atom([T,P],'_',ID),
        !.
feature_xsbid(_F,anon).




write_fact(Fact):-
        format('~q.~n',[Fact]).

xwrite_fact(Fact):-
        Fact=..[Pred|Args],
        xsbsafe(Pred,Pred2),
        maplist(xsbsafe,Args,Args2),
        concat_atom(Args2,', ',ArgsAtom),
        format('~w(~w).~n',[Pred2,ArgsAtom]).

xsbsafe(A,S) :-
        atom_chars(A,[C|Chars]),
        xsbsafec(C,C1),
        downcase_atom(C1,C2),
        maplist(xsbsafec,Chars,Chars2),
        atom_chars(S,[C2|Chars2]).

xsbsafec(X,X) :- X @>= 'a', X @=< 'z',!.
xsbsafec(X,X) :- X @>= 'A', X @=< 'Z',!.
xsbsafec(X,X) :- X @>= '0', X @=< '9',!. % '0
xsbsafec(_,'_').

        
type_pred('CDS',cds,F) :- featureloc(F,_,S,E,_),featureloc(F,_,S2,E2,_),S\=S2,E\=E2,!.
type_pred('CDS',cds_part,_) :- !.
type_pred(T,P,_):- type_pred(T,P),!.



type_pred('UTR',utr).
type_pred(X,X).
