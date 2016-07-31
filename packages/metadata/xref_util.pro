:- module(xref_util,
	  [
           prefixset_stats/2
	   ]).

:- use_module(metadata_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

entity_xprefix(E,S) :- entity_xref_idspace(E,_,S).
class_xrefdbs(C,Xs) :- class(C),solutions(S,entity_xprefix(C,S),Xs).

prefixset_stats(vocabset,number_of_classes_mapped).
prefixset_stats(Xs,Num) :- aggregate(count,C,class_xrefdbs(C,Xs),Num).



