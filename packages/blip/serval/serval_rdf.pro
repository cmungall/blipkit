:- module(serval_rdf,
          [
           match_conditions/3,
           op(1000,xfy,(==>)),
           op(900,xfy,(::)),
           op(700,xfy,a)
          ]).

:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(bio(serval)).

:- op(1000,xfy,(==>)).
:- op(900,xfy,(::)).
:- op(700,xfy,a).

:- multifile
	system:term_expansion/2.
:- multifile user:cond_match/1.
:- discontiguous user:cond_match/1.

system:term_expansion( (Cond :: Match ==> Body),
                     [(sdefun(Head :: Match,Goals):- match_conditions(Head,Cond,Match)),
                      cond_match(Cond :: Match)
                     ]
                   ):-
        !,
        expand_term(Body,BodyExp), % rdf ns expansion
        tuple_to_list(BodyExp,Goals).
                     
% test for exact match in conditions
%xxxmatch_conditions(Resource,Resource a Class1):-
%        rdf_global_id(Resource,Exp1),
%        rdf_global_id(Class,Exp2),
%        Exp1=Exp2,
%        !.

match_conditions(Resource,Resource a ClassIn,Match):-
        rdf_global_id(ClassIn,Class),
        rdfs_individual_of(Resource,Class),
        \+ (   cond_match(_ a MoreSpecificClassIn :: Match),
               rdf_global_id(MoreSpecificClassIn,MoreSpecificClass),
               MoreSpecificClass \= Class,
               rdfs_individual_of(Resource,MoreSpecificClass),
               rdfs_subclass_of(MoreSpecificClass,Class),
               debug(serval_rdf,'Dropped ~w :: ~w as ~w is more specific',[ClassIn,Match,MoreSpecificClass])).


