:- module(ontol_management,
	  [
	   merge_equivalent_classes,
	   merge_class/2,
	   merge_class/3,
	   
           is_nondangling/1,
           remove_dangling_facts/0
	   ]
	 ).

:- use_module(ontol_db).

:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)). 

merge_equivalent_classes :-
	setof(A-B,
	      (	  subclass(A,B),
		  A@<B,
		  subclass(B,A)),
	      Pairs),
	forall(member(A-B,Pairs),
	       merge_class(A,B)).

%% merge_class(+Src,+Tgt) is det
merge_class(Src,Tgt) :-
	merge_class(Src,Tgt,[merge(all)]).
			     

%% merge_class(+Src,+Tgt,+Opts) is det
% does not redirect existing axioms
merge_class(Src,Tgt,Opts) :-
	debug(merge,'merging ~w -> ~w',[Src,Tgt]),
	% merging axioms about Src
	merge_class_axiom(subclass(Src,X),subclass(Tgt,X),Opts),
	merge_class_axiom(restriction(Src,R,X),restriction(Tgt,R,X),Opts),
	(   \+ genus(Tgt,_)
	->  merge_class_axiom(genus(Src,X),genus(Tgt,X),Opts),
	    merge_class_axiom(differentium(Src,R,X),differentium(Tgt,R,X),Opts)
	;   true), % don't merge definitions
	(   \+ def(Tgt,_)
	->  merge_class_axiom(def(Src,X),def(Tgt,X),Opts),
	    merge_class_axiom(def_xref(Src,X),def_xref(Tgt,X),Opts)),
	(   entity_label(Src,Label),
	    entity_label_or_synonym(Tgt,Label)
	->  true
	;   merge_class_axiom(entity_label(Src,Label),entity_synonym_scope(Tgt,Label,exact),Opts)),
	merge_class_axiom(entity_synonym_scope(Src,X,Sc),entity_synonym_scope(Tgt,X,Sc),Opts),
	merge_class_axiom(entity_synonym_scope(Src,X,Sc),entity_synonym_scope(Tgt,X,Sc),Opts),
	merge_class_axiom(entity_synonym(Src,X),entity_synonym(Tgt,X),Opts),
	merge_class_axiom(entity_xref(Src,X),entity_xref(Tgt,X),Opts),
	retractall(class(Src)),
	%assert(entity_obsolete(Src,class)),
	assert(metadata_db:entity_alternate_identifier(Tgt,Src)),
				% merging axioms referencing Src
	merge_class_axiom(subclass(X,Src),subclass(X,Tgt),Opts),
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

remove_dangling_facts :-
	dangling_fact(F),
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
	F=restriction(A,R,B),
	F,
	\+ ((is_nondangling(A),
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

