:- module(seqfeature_bridge_from_simple_graph,
          [
          ]).

:- use_module(bio(seqfeature_db)).

:- multifile user:edge/3,user:edge/6.


seqfeature_db:feature_type(F1,T1):- edge(F,a,T),idmap(F,F1),idmap(T,T1).
seqfeature_db:feature_relationship(X1,Y1,R1,0):- edge(X,R,Y),R\=a,R\=loc,idmap(X,X1),idmap(Y,Y1),idmap(R,R1).
seqfeature_db:feature(F1):-
        setof(F,feature_bag(F),Fs), % consider tabling this
        member(F,Fs),
        idmap(F,F1).

seqfeature_db:featureloc(F1,Src1,Beg,End,Strand,0,0,[]):-
        edge(F,loc,Src,Beg,End,Strand),
        idmap(F,F1),
        idmap(Src,Src1).


feature_bag(F):- feature_relationship(F,_,_).
feature_bag(F):- feature_relationship(_,F,_).
feature_bag(F):- edge(F,a,_).

idmap(In,Out):- concat_atom([_],':',In),!,atom_concat('test:',In,Out).
idmap(X,X):- !.

