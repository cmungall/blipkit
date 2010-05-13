

:- module(qc_obo,[xsensu/0]).
:- use_module(bio(ontol_db)).

%  finds W such that
%
%  X            Y
%  | \         /
%  W  X sensu S
%  
% "X"
% "W is_a X"
% "class<X sensu S>" R Y
% "Y != X"
%  W is not a sensu term
%  ??Y is not a sensu term
%  
% "I inst_of W"
% "I sensu S" 
%
% => I R Y
%
%  but queries on Y will not find I
%  
%  
xsensu(W,X,SensuTerm,Y):-
        class(W,WN),
        not(sensu_substr(WN,_,_)),
        %parent(W,X),
        subclass(W,X),
        class(X,XN),
        subclass(SensuTerm,X),
        W \= SensuTerm,
        class(SensuTerm,SensuTermN),
        sensu_substr(SensuTermN,XN,_S),
        %parent(SensuTerm,Y),
        subclass(SensuTerm,Y),
        Y \= X,
        class(Y,YN),
        not(sensu_substr(YN,_,_)).        

xsensu:-
        forall(xsensu(W,X,S,Y),
               showids([W,X,S,Y])).

showids([]):- nl.
showids([ID|L]):-
        class(ID,N),
        format('[~w] ~w; ',[ID,N]),
        showids(L).

%% sensu_substr(NS,N,S)
sensu_substr(NS,N,S):-
        atom_length(NS,Len),
        sub_atom(NS,Beg,_,After,' (sensu '),
        sub_atom(NS,0,Beg,_,N),
        SLen is (Len-After)-1,
        sub_atom(NS,After,SLen,_,S).
        
/** <module>

  ad hoc area for QC on OBO ontologies; not particularly generic*/