/* -*- Mode: Prolog -*- */


:- module(seqfeature_writer_dlv,[
                                 write_all_as_dlv/0
                                 ]).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).

% semi-hack: indicates that this writes to stdout
io:redirect_stdout(seqfeature_dlv).

io:write_all(seqfeature_dlv,_,_):-
        write_all_as_dlv.
        
write_all_as_dlv:-
        forall(feature(F),
               write_feature(seqfeature_dlv,F)).

write_feature(seqfeature_dlv,F) :-
        feature_strand(F,-1),
        !. % do nothing for now! TODO

/*
write_feature(seqfeature_dlv,F) :-
        atomic(F),              % named feature
        !,
        feature_type(F,Type),
        type_pred(Type,Pred),
        %sformat(Pred2,'named_~w',[Pred]),
        atom_concat('named_',Pred,Pred2),
        forall(featureloc(F,_,S,E,_),
               (   Fact=..[Pred2,F,S,E],
                   write_fact(Fact))).
*/

write_feature(seqfeature_dlv,F) :-
        feature_type(F,Type),
        type_pred(Type,Pred),
        feature_dlvid(F,ID),
        forall(featureloc(F,_,S,E,_),
               (   Fact=..[Pred,ID,S,E],
                   write_fact(Fact))),
        forall(feature_relationship(F,P),
               write_feature_relationship(F,ID,P)).


write_feature_relationship(_F,ID,P) :-
        feature_dlvid(P,PID),
        feature_type(P,PT),
        class(PTID,PT),
        class(TTID,transcript),
        subclassRT(PTID,TTID),
        !,
        write_fact(transcribed_in(ID,PID)). % TODO
write_feature_relationship(_F,_,_) :- !.    % TODO

               

feature_dlvid(F,F):- atomic(F),!.
feature_dlvid(F,ID):-
        feature_type(F,T),
        feature_relationship(F,P),
        concat_atom([T,P],'_',ID),
        !.
feature_dlvid(_F,anon).




write_fact(Fact):-
        Fact=..[Pred|Args],
        dlvsafe(Pred,Pred2),
        maplist(dlvsafe,Args,Args2),
        concat_atom(Args2,', ',ArgsAtom),
        format('~w(~w).~n',[Pred2,ArgsAtom]).

dlvsafe(A,S) :-
        atom_chars(A,[C|Chars]),
        dlvsafec(C,C1),
        downcase_atom(C1,C2),
        maplist(dlvsafec,Chars,Chars2),
        atom_chars(S,[C2|Chars2]).

dlvsafec(X,X) :- X @>= 'a', X @=< 'z',!.
dlvsafec(X,X) :- X @>= 'A', X @=< 'Z',!.
dlvsafec(X,X) :- X @>= '0', X @=< '9',!. % '0
dlvsafec(_,'_').

        

type_pred('CDS',cds).
type_pred('UTR',utr).
type_pred(X,X).
