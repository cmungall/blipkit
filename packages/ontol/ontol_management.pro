:- module(ontol_management,
	  [
           iterative_merge_equivalent_classes/0,
	   merge_equivalent_classes/0,
	   merge_class/2,
	   merge_class/3,
	   
           is_nondangling/1,
           remove_dangling_facts/0,
           remove_redundant_facts/0,
           remove_simple_redundant_facts/0,
           delete_all_non_gd_xpdefs/0,
           
           make_class_obsolete/1,
           delete_class/1,
           delete_relation_usage/1,
           delete_relation_usage_except/1,

           extract_slim/1
	   ]
	 ).

:- use_module(ontol_db).

:- use_module(bio(tabling)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)). 

%% merge_equivalent_classes
% assumes pre-reasoned ontology
merge_equivalent_classes :-
        merge_equivalent_classes_semidet,
        !.
merge_equivalent_classes.

% we assume reasoning has been done, but if not, infer symmetric relationship
ec_sym(A,B) :- equivalent_class(A,B).
ec_sym(A,B) :- equivalent_class(B,A).
ec_sym(A,B) :- class_cdef(A,D),class(A),class_cdef(B,D),class(B),A\=B.

% todo - group into equivsets
merge_equivalent_classes_semidet :-
	setof(A-B,
	      (	  ec_sym(A,B),
		  A@<B),
	      Pairs),
        length(Pairs,NumPairs),
        debug(merge,'number of pairs to merge: ~w',[NumPairs]),
	forall(member(A-B,Pairs),
	       merge_class(A,B)).

% note: this won't work on pre-reasoned db
iterative_merge_equivalent_classes :-
        debug(merge,'iterative merge...',[]),
        merge_equivalent_classes_semidet,
        !,
        iterative_merge_equivalent_classes.
iterative_merge_equivalent_classes.



%% merge_class(+Src,+Tgt) is det
merge_class(Src,Tgt) :-
	merge_class(Src,Tgt,[merge(all)]).
			     

%% merge_class(+Src,+Tgt,+Opts) is det
%
% merges Src into Tgt.
%
% def of Src becomes def of Tgt, unless Tgt has def
% 
% does not redirect existing axioms.
merge_class(Src,Tgt,Opts) :-
	debug(merge,'merging ~w -> ~w',[Src,Tgt]),
        
	% merging axioms ABOUT Src
	merge_class_axiom(subclass(Src,X),subclass(Tgt,X),Opts),
	merge_class_axiom(equivalent_class(Src,X),equivalent_class(Tgt,X),Opts),
	merge_class_axiom(restriction(Src,R,X),restriction(Tgt,R,X),Opts),
        % allow creation of new..
        (   \+class(Tgt)
        ->  assert(class(Tgt))
        ;   true),
	(   \+ genus(Tgt,_)
	->  merge_class_axiom(genus(Src,X),genus(Tgt,X),Opts),
	    merge_class_axiom(differentium(Src,R,X),differentium(Tgt,R,X),Opts)
	;   true), % don't merge definitions
	(   \+ def(Tgt,_)
	->  merge_class_axiom(def(Src,X),def(Tgt,X),Opts),
	    merge_class_axiom(def_xref(Src,X),def_xref(Tgt,X),Opts)
        ;   true), % keep existing def
	(   \+ entity_resource(Tgt,_)
	->  merge_class_axiom(entity_resource(Src,X),entity_resource(Tgt,X),Opts)
        ;   true), % keep existing 
        (   \+ entity_label(Tgt,_)
        ->  merge_class_axiom(entity_label(Src,X),entity_label(Tgt,X),Opts)
        ;   (   entity_label(Src,Label),
                entity_label_or_synonym(Tgt,Label)
            ->  true
            ;   merge_class_axiom(entity_label(Src,Label),entity_synonym_scope(Tgt,Label,exact),Opts))), % TODO
        (   memberchk(add_provenance(true),Opts)
        ->  forall(entity_synonym(Src,X),
                   assert(entity_synonym_xref(Tgt,X,Src)))
        ;   true),
	merge_class_axiom(entity_synonym_scope(Src,X,Sc),entity_synonym_scope(Tgt,X,Sc),Opts),
	merge_class_axiom(entity_synonym_type(Src,X,Sc),entity_synonym_type(Tgt,X,Sc),Opts),
	merge_class_axiom(entity_synonym(Src,X),entity_synonym(Tgt,X),Opts),
        merge_class_axiom(entity_alternate_identifier(Src,X),entity_alternate_identifier(Tgt,X),Opts),
	merge_class_axiom(entity_xref(Src,X),entity_xref(Tgt,X),Opts),
	retractall(class(Src)),
	%assert(entity_obsolete(Src,class)),
	assert(metadata_db:entity_alternate_identifier(Tgt,Src)),
        
	% merging axioms REFERENCING Src
	merge_class_axiom(subclass(X,Src),subclass(X,Tgt),Opts),
	merge_class_axiom(equivalent_class(X,Src),equivalent_class(X,Tgt),Opts),
	merge_class_axiom(genus(X,Src),genus(X,Tgt),Opts),
	merge_class_axiom(restriction(X,R,Src),restriction(X,R,Tgt),Opts),
	merge_class_axiom(differentium(X,R,Src),differentium(X,R,Tgt),Opts).

merge_class_axiom(Src,Tgt,Opts) :-
	Src =.. [P|_],
	(   member(merge(P),Opts)
	;   member(merge(all),Opts)),
	!,
	forall((Src,\+invalid(Tgt)),
	       assert(Tgt)),
	retractall(Src).
merge_class_axiom(_,_,_).

invalid(subclass(X,X)).
invalid(restriction(X,_,X)).

%%  is_nondangling(?Relation) is semidet
is_nondangling(X):- class(X).
is_nondangling(X):- property(X).
is_nondangling(X):- inst(X).

simple_redundant(G) :-
        G=subclass(A,B),
        G,
        subclassT(A,Z),
        subclassT(Z,B).
simple_redundant(G) :-
        G=restriction(A,R,B),
        G,
        parentT(A,R,Z),
        parentT(Z,R,B).
simple_redundant(G) :-
        G=restriction(A,R,B),
        G,
        subclassT(A,Z),
        parentT(Z,R,B).
simple_redundant(G) :-
        G=restriction(A,R,B),
        G,
        parentT(A,R,Z),
        subclassT(Z,B).
        
remove_redundant_facts :-
        is_redundant(F),
	retractall(F),
	fail.
remove_redundant_facts.        

remove_simple_redundant_facts :-
        simple_redundant(F),
        debug(ontol,'Removing: ~w',[F]),
	retractall(F),
	fail.
remove_simple_redundant_facts.        

remove_dangling_facts :-
	dangling_fact(F),
        debug(dangling,'removing ~w',[F]),
	retractall(F),
	fail.
remove_dangling_facts.

dangling_fact(F) :-
	F=subclass(A,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=disjoint_from(A,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=property_domain(A,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=property_range(A,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=restriction(A,R,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(R),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=gci_restriction(A,R,B,GR,Y),
	F,
	\+ ((is_nondangling(GR),
             is_nondangling(Y),
             is_nondangling(A),
	     is_nondangling(R),
	     is_nondangling(B))).
dangling_fact(F) :-
	class_cdef(C,cdef(G,DL)),
	\+ ((is_nondangling(G),
	     forall(member(R=X,DL),
		    (	is_nondangling(R),
			is_nondangling(X))))),
	class_cdef_fact(C,cdef(G,DL),F).

class_cdef_fact(C,cdef(G,_),genus(C,G)).
class_cdef_fact(C,cdef(_,DL),differentium(C,R,X)) :-
	member(R=X,DL).


make_class_obsolete(X) :-
        retractall(class(X)),
        assert(metadata_db:entity_obsolete(X,class)).

delete_entity(X) :-
        debug(edit,'deleting ~w',[X]),
        retractall(entity_resource(X,_)),
        retractall(entity_obsolete(X,_)).

delete_class(X) :-
        debug(edit,'deleting class ~w',[X]),
        forall_distinct(Y,
                        genus(Y,X),
                        delete_xpdef(Y)),
        forall_distinct(Y,
                        differentium(Y,_,X),
                        delete_xpdef(Y)),
        retractall(entity_resource(X,_)),
        retractall(class(X)),
        retractall(subclass(_,X)),
        retractall(subclass(X,_)),
        retractall(genus(_,X)), % redundant with above
        retractall(genus(X,_)),
        retractall(differentium(_,_,X)), % redundant with above
        retractall(differentium(X,_,_)), 
        retractall(restriction(_,_,X)),
        retractall(restriction(X,_,_)).

delete_relation_usage(R) :-
        debug(edit,'deleting restrictions for ~w',[R]),
        retractall(restriction(_,R,_)),
        debug(edit,'deleting xp defs for ~w',[R]),
        solutions(X,differentium(X,R,_),Xs),
        maplist(delete_xpdef,Xs).

delete_relation_usage_except(XRL) :-
        solutions(R,
                  (   parent(_,R,_),
                      R\=subclass,
                      \+ member(R,XRL),
                      \+ ((subclass(R,RP),
                           member(RP,XRL)))
                  ),
                  Rs),
        maplist(delete_relation_usage,Rs).


delete_xpdef(X) :-
        retractall(genus(X,_)),
        retractall(differentium(X,_,_)).

delete_all_non_gd_xpdefs :-
        forall(non_gd(X),
                delete_xpdef(X)).

non_gd(X) :-
        genus(X,_),
        \+ differentium(X,_,_).
non_gd(X) :-
        differentium(X,_,_),
        \+ genus(X,_).
non_gd(X) :-
        genus(X,G1),
        genus(X,G2),
        G1\=G2.



extract_slim(S) :-
        table_pred(ontol_db:parentT/3),
        findall(X-R-Y,
                slim_parent(S,X,R,Y),
                Links),
        forall((class(X),\+entity_partition(X,S)),
               delete_class(X)),
        forall((entity_obsolete(X,_),\+entity_partition(X,S)),
               delete_entity(X)),
        retractall(subclass(_,_)),
        retractall(restriction(_,_,_)),
        forall(member(X-subclass-Y,Links),
               assert(subclass(X,Y))),
        forall((member(X-R-Y,Links),R\=subclass),
               assert(restriction(X,R,Y))),
        remove_dangling_facts.


slim_parent(S,X,R,Y) :-
        entity_partition(X,S),
        parentT(X,R,Y),
        entity_partition(Y,S),
        \+ ((parentT(X,R1,Z),
             entity_partition(Z,S),
             parentT(Z,R2,Y),
             ontol_db:combine_relation_pair(R1,R2,R))).


                      
        
