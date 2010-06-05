/* -*- Mode: Prolog -*- */

:- module(obolog_db,[

                     formula/1,
                     formula_comment/2,
                     formula_module/2,
                     assert_formula/1,
                     materialize_formulae/0,
                     named_entity/1,
                     named_entity/2,
                     atomic_predicate/1,
                     predicate_formula/2,
                     
                     uriprefix/2,
                     metarelation/1,
                     unary/1,
                     relation/1,
                     type/1,
                     variable_arity/1,

                     type_type/1,
                     instance_instance/1,
                     
                     label/2,
                     comment/2,
                     example/2,
                     example/3,
                     counterexample/2,
                     counterexample/3,
                     text_definition/2,

                     xref/2,
                     text_definition_xref/2,
                     image_xref/2,
                     
                     formal_label/2,
                     synonym/2,
                     scoped_synonym/3,
                     exact_synonym/2,
                     narrow_synonym/2,
                     broad_synonym/2,
                     related_synonym/2,

                     alternate_identifier/2,
                     exported_identifier/2,

                     holds_between/4,
                     holds_between/5,
                     holds_temporally_between/3,
                     holds_atemporally_between/3,

                     equivalent_to/2,
                     subrelation/2,
                     proper_subrelation/2,
                     has_subrelation/2,
                     holds_over_chain/3,
                     equivalent_to_chain/3,
                     transitive_over/2,
                     inverse_of/2,
                     normative_direction_for/2,
                     transitive/1,
                     functional/1,
                     symmetric/1,
                     asymmetric/1,
                     reflexive/1,
                     anti_symmetric/1,

                     domain/2,
                     range/2,
                     type_domain/2,
                     type_range/2,

                     all_some/2,
                     all_only/2,
                     all_some_all_times/2,
                     all_some_tr/2,
                     %all_some_in_reference_context/2,
                     all_some_in_reference_context/3,
                     class_instance_relation_pair/2,
                     reciprocal_relation/2,

		     disjoint_over/2,
		     disjoint_from/2,
		     relation_disjoint_from/2,
		     relation_complement_of/2,
                     
                     homeomorphic_for/2,

                     function_argument/3,
                     function_range/2,

                     '=>'/2,
                     '<=>'/2,
                     relation_axiom/3,
                     relation_axiom/4,
                     relation_axiom_direct/3,
                     relation_axiom_indirect/3,
                     formula_references/2,
                     
                     axiom_expansion/2,
                     axiom_expanded/2,
                     axiom_expanded_recursive/2,
                     
                     rewrite_identifiers/0,
                     map_identifiers/2,
                     map_identifiers/3
                    ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(sxpr_parser),[sxpr_prolog/2]).
:- use_module(bio(bioprolog_util)).

:- extensional(formula/1).
:- extensional(formula_comment/2).
:- extensional(formula_module/2).

assert_formula(X):- assert_formula(X,_,_).

assert_formula(module(M,F),_,C):- assert_formula(F,M,C).
assert_formula($sc(Comment,F),M,_):- assert_formula(F,M,Comment).
%assert_formula($sc(Comment,F),_,_):- assert(formula(F)),assert(formula_comment(F,Comment)).
assert_formula(F,M,C):-
	assert(formula(F)),
	(   nonvar(M)
	->  assert(formula_module(F,M))
	;   true),
	(   nonvar(C)
	->  assert(formula_comment(F,C))
	;   true).
%assert_formula(X):- assert(formula(X)).

%% reified_formula_clause(?PredSpec,?Clause)
% obolog predicates are reified; however, we want to provide direct unreified access for convenience.
% @param PredSpec Functor/Arity
% @param Clause FactGoal :- formula(FactGoal)
reified_formula_clause(P/A, (T :- formula(T)) ):-
  pred_to_unground_term(P/A,T). % asseretd
reified_iformula_clause(P/A, (T :- iformula(T)) ):-
  pred_to_unground_term(P/A,T). % inferred

named_entity(E):- formula(S),S=..[_|Args],member(E,Args),atom(E).
named_entity(E,PosM1):- formula(S),S=..Args,length(Args,Len),between(1,Len,Pos),nth1(Pos,Args,E),atom(E),PosM1 is Pos-1.

atomic_predicate(P):- formula(S),S=..[P|_].

predicate_formula(P,S):- formula(S),S=..[P|_].


% unreify formula
% e.g. label(A,B):- formula(label(A,B)).

:- discontiguous mypred/1.

% obolog predicates are reified; however, we want to provide direct unreified access for convenience.
% myext(label/2) ==> label(X,L):- formula(label(X,L)).
system:term_expansion((:- myext(Pred)),
                    [FT,IFT,mypred(Pred)]):- reified_formula_clause(Pred,FT),reified_iformula_clause(Pred,IFT).

materialize_formulae:-
        forall(mypred(Pred),
               materialize_view(obolog_db:Pred)).

:- myext(uriprefix/2).
:- myext(idspace/2).  % redundant?
:- myext(relation/1).
:- myext(metarelation/1).
:- myext(unary/1).
:- myext(type/1).

:- myext(type_type/1).
:- myext(instance_instance/1).

:- myext(label/2).
%:- myext(comment/2). clash
:- myext(example/2).
:- myext(example/3).
:- myext(counterexample/2).
:- myext(counterexample/3).
:- myext(text_definition/2).
:- myext(text_definition_xref/2).
:- myext(xref/2).
:- myext(image_xref/2).

:- myext(formal_label/2).
:- myext(exact_synonym/2).
:- myext(narrow_synonym/2).
:- myext(broad_synonym/2).
:- myext(related_synonym/2).
%synonym(E,S):- exact_synonym(E,S).
%synonym(E,S):- broad_synonym(E,S).
%synonym(E,S):- narrow_synonym(E,S).
%synonym(E,S):- related_synonym(E,S).

:- myext(synonym/2).

% inference of synonyms
iformula(synonym(E,S)):- formula(exact_synonym(E,S)).
iformula(synonym(E,S)):- formula(broad_synonym(E,S)).
iformula(synonym(E,S)):- formula(narrow_synonym(E,S)).
iformula(synonym(E,S)):- formula(related_synonym(E,S)).

scoped_synonym(exact,E,S):- exact_synonym(E,S).
scoped_synonym(broad,E,S):- broad_synonym(E,S).
scoped_synonym(narrow,E,S):- narrow_synonym(E,S).
scoped_synonym(related,E,S):- related_synonym(E,S).

:- myext(exported_identifier/2).
:- myext(alternate_identifier/2).

:- myext(equivalent_to/2).
:- myext(subrelation/2).
has_subrelation(X,Y):- subrelation(Y,X).

:- myext(proper_subrelation/2).
:- myext(holds_over_chain/3).
:- myext(equivalent_to_chain/3).

:- myext(inverse_of/2).
:- myext(normative_direction_for/2).
:- myext(transitive/1).
:- myext(transitive_over/2).
:- myext(symmetric/1).
:- myext(asymmetric/1).
:- myext(anti_symmetric/1).
:- myext(reflexive/1).
:- myext(functional/1).

:- myext(homeomorphic_for/2).
:- myext(domain/2).
:- myext(range/2).
:- myext(type_domain/2).
:- myext(type_range/2).

:- myext(function_argument/3).
:- myext(function_range/2).

:- myext(all_some/2).
:- myext(all_only/2).
:- myext(all_some_all_times/2).
:- myext(all_some_tr/2).
%:- myext(all_some_in_reference_context/2).
:- myext(all_some_in_reference_context/3).
class_instance_relation_pair(X,Y):-        all_some(X,Y).
class_instance_relation_pair(X,Y):-        all_some_all_times(X,Y).
%class_instance_relation_pair(X,Y):-        all_some_in_reference_context(X,Y).
class_instance_relation_pair(X,Y):-        all_some_in_reference_context(X,Y,_).

:- myext(holds_temporally_between/3).
:- myext(holds_atemporally_between/3).
%holds_temporally_between(R,X,Y):- formula(holds_temporally_between(R,X,Y)).
%holds_atemporally_between(R,X,Y):- formula(holds_temporally_between(R,X,Y)).

holds_between(R,X,Y,Temporal,instance):- holds_between(R,X,Y,Temporal).
holds_between(R,X,Y,Temporal,type):- class_instance_relation_pair(R,PR),holds_between(PR,X,Y,Temporal).


holds_between(R,X,Y,temporal):-        holds_temporally_between(R,X,Y).
holds_between(R,X,Y,atemporal):-        holds_atemporally_between(R,X,Y).
holds_between(R,X,Y,''):-
        \+ holds_temporally_between(R,X,Y),
        \+ holds_atemporally_between(R,X,Y),
        (   domain(R,X) ;  relation(R),\+ domain(R,X),X=''), 
        (   range(R,Y) ;  relation(R),\+ range(R,Y),Y=''),
        \+ ( (X='',Y='')).

:- myext(disjoint_over/2).
:- myext(disjoint_from/2).
:- myext(relation_disjoint_from/2).
:- myext(relation_complement_of/2).

:- myext('=>'/2).
:- myext('<=>'/2).

implication_type('=>').
implication_type('<=>').

variable_arity(R):-
        holds_temporally_between(R,_,_),
        holds_atemporally_between(R,_,_).

relation_axiom_direct(R,T,Ax):-
        relation_axiom(R,T,Ax,direct).
relation_axiom_indirect(R,T,Ax):-
        relation_axiom(R,T,Ax,indirect).

%% relation_axiom(?Relation,?ImplicationType,?Axiom)
% @param ImplicationType either => or <=>
relation_axiom(R,T,Ax):-
        relation_axiom(R,T,Ax,_).


relation_axiom(R,T,Ax,direct):-
        implication_type(T),
        Ax=..[T,X,_],
        %Ax='<=>'(X,_),
        Ax,
        X=..[R|_].
relation_axiom(R,T,Ax,indirect):-
        implication_type(T),
        Ax=..[T,X,_],
        %Ax='<=>'(X,_),
        Ax,
        \+ (X=..[R|_]),
        formula_references(Ax,R).
        
formula_references(Ax,R):-
        Ax=..L,
        member(R,L).
formula_references(Ax,R):-
        Ax=..[_|L],
        member(X,L),
        formula_references(X,R).

        

reciprocal_relation(X,Y):-
        class_instance_relation_pair(X,PX),
        inverse_of(PX,PY),
        class_instance_relation_pair(Y,PY).

relation_quad(PR,PRI,TR,TRI):-
        type_type(TR),
        class_instance_relation_pair(TR,PR),
        inverse_of(PR,PRI),
        class_instance_relation_pair(PRI,TRI).

rewrite_identifiers:-
        debug(obolog,'Rewriting identifiers',[]),
        findall(ID->Label,label(ID,Label),Mappings),
        debug(obolog,'  got ID->Label mappings',[]),
        findall(Fact,formula(Fact),Facts),
        map_identifiers(Facts,IDMappedFacts,Mappings),
        debug(obolog,'   Retracting and asserting',[]),
        retractall(formula(_)),
        maplist(assert_formula,IDMappedFacts),
        debug(obolog,'Asserted with new IDs',[]).

%% map_identifiers(+Sentences,?MappedSentences) is det
% map local symbols to OBO IDs via exported_identifier/2
map_identifiers(InSL,OutSL):-
        solutions(In->Out,member(exported_identifier(In,Out),InSL),Map1),
        %solutions(uriprefix(In,Out),member(uriprefix(In,Out),InSL),Map2),
        solutions(uriprefix(In,Out),uriprefix(In,Out),Map2),
        append(Map1,Map2,Map),
        map_identifiers(InSL,OutSL,Map).

%% map_identifiers(+Sentences,?MappedSentences,+Mappings) is det
% Mappings=[Mapping,...]
% Mapping= In->Out ; uriprefix(Pre,Full)
map_identifiers([],[],_):- !.
map_identifiers([S|InSL],[S2|OutSL],Map):-
        !,
        (   S=label(Local,Label) % special case, as label may equal local ID
        ->  map_identifiers(Map,Local,Public),
            S2=label(Public,Label)
        ;   S=..Args,
            maplist(map_identifiers(Map),Args,Args2),
            S2=..Args2),
        map_identifiers(InSL,OutSL,Map).

map_identifiers(Map,In,Out2):-
        atomic(In),
        !,
        (   member(In->Out,Map),
            !
        ->  true
        ;   Out=In),
        (   member(uriprefix(Prefix,Full),Map),
            concat_atom([Prefix,Local],':',Out)
        ->  concat_atom([Full,Local],Out2)
        ;   Out2=Out).


map_identifiers(Map,In,Out):-
        !,
        In=..Args,
        maplist(map_identifiers(Map),Args,Args2),
        Out=..Args2.

% AXIOM EXPANSION
% TODO: get these from KIF

%% axiom_expansion(+FactAxiom,?RuleAxiom) nondet
% axioms of the form symmetric(R) are expanded to forms that can be used in a FOL reasoner
% e.g. (=> (R ?x ?y) (R ?y ?x)) 
axiom_expansion(A,X):-
        axiom_expandsxpr(A,SX),
        sxpr_prolog(SX,X).

axiom_expanded(A,X):-
        axiom_expandsxpr(A,SX),
        (   ground(A)->true ; formula(A)),
        sxpr_prolog(SX,X).
axiom_expanded(A,X):-
        axiom_expandsxpr(A,SX,Cond),
        (   ground(A)->true ; formula(A)),
        Cond,
        sxpr_prolog(SX,X).

axiom_expanded_recursive(A,Ax):-
        axiom_expanded(A,Ax1),
        recursive_expand(Ax1,Ax).

recursive_expand(A,X):-
        axiom_expandsxpr(A,SX,Cond),
        Cond,
        !,
        sxpr_prolog(SX,X1),
        recursive_expand(X1,X).
recursive_expand(A,A).

% TODO - only expland time-indexed if declared
axiom_expandsxpr(functional(R),['=>',[and,
				      [R,'?x','?y'],
				      [R,'?x','?z']],
				['=','?y','?z']]).
axiom_expandsxpr(functional(R),['=>',[and,
				      [R,'?x','?y','?t'],
				      [R,'?x','?z','?t']],
				['=','?y','?z']]).
axiom_expandsxpr(transitive(R),['=>',[and,
                             [R,'?x','?y'],
                             [R,'?y','?z']],
                       [R,'?x','?z']]).
axiom_expandsxpr(transitive(R),['=>',[and,
                             [R,'?x','?y','?t'],
                             [R,'?y','?z','?t']],
                       [R,'?x','?z','?t']]).
axiom_expandsxpr(subrelation(R,R2),['=>',
                                    [R,'?x','?y','?t'],
                                    [R2,'?x','?y','?t']]).
axiom_expandsxpr(subrelation(R,R2),['=>',
                                    [R,'?x','?y'],
                                    [R2,'?x','?y']]).
axiom_expandsxpr(proper_subrelation(ProperR,R),['=>',
                                                [ProperR,'?x','?y'],
                                                [and,
                                                 [R,'?x','?y'],
                                                 [not,['=','?x','?y']]]]).
axiom_expandsxpr(relation_disjoint_from(R,R2),['=>',
                                               [R,'?x','?y'],
                                               [not,[R2,'?x','?y']]]).
axiom_expandsxpr(relation_disjoint_from(R,R2),['=>',
                                               [R2,'?x','?y'],
                                               [not,[R,'?x','?y']]]).
axiom_expandsxpr(holds_over_chain(R,R1,R2),['=>',[and,
                                                  [R1,'?x','?y','?t'],
                                                  [R2,'?y','?z','?t']],
                                            [R,'?x','?z','?t']]).
axiom_expandsxpr(holds_over_chain(R,R1,R2),['=>',[and,
                                                  [R1,'?x','?y'],
                                                  [R2,'?y','?z']],
                                            [R,'?x','?z']]).
% TODO: DRY
axiom_expandsxpr(equivalent_to_chain(R,R1,R2),['=>',[and,
                                                      [R1,'?x','?y','?t'],
                                                      [R2,'?y','?z','?t']],
                                               [R,'?x','?z','?t']]).
axiom_expandsxpr(equivalent_to_chain(R,R1,R2),['=>',[and,
                                                      [R1,'?x','?y'],
                                                      [R2,'?y','?z']],
                                               [R,'?x','?z']]).
axiom_expandsxpr(equivalent_to_chain(R,R1,R2),['=>',[R,'?x','?z'],
                                               [exists,'?y',
                                                [and,
                                                 [R1,'?x','?y'],
                                                 [R2,'?y','?z']]]]).
axiom_expandsxpr(transitive_over(R,R1),['=>',[and,
                                              [R,'?x','?y','?t'],
                                              [R1,'?y','?z','?t']],
                                        [R,'?x','?z','?t']]).
axiom_expandsxpr(transitive_over(R,R1),['=>',[and,
                                              [R,'?x','?y'],
                                              [R1,'?y','?z']],
                                        [R,'?x','?z']]).
axiom_expandsxpr(symmetric(R),['=>',
                      [R,'?x','?y'],
                      [R,'?y','?x']]).
axiom_expandsxpr(symmetric(R),['=>',
                      [R,'?x','?y','?t'],
                      [R,'?y','?x','?t']]).
axiom_expandsxpr(asymmetric(R),['=>',
                                [R,'?x','?y'],
                                [not, [R,'?y','?x']]]).
axiom_expandsxpr(asymmetric(R),['=>',
                                [R,'?x','?y','?t'],
                                [not, [R,'?y','?x','?t']]]).

axiom_expandsxpr(inverse_of(R,RI),['<=>',
                                   [R,'?x','?y'],
                                   [RI,'?y','?x']]).
axiom_expandsxpr(inverse_of(R,RI),['<=>',
                                   [R,'?x','?y','?t'],
                                   [RI,'?y','?x','?t']]).
axiom_expandsxpr(formula(transitive_over(R,R2)),['<=>',
                                        [and,
                                         [R,'?x','?y'],
                                         [R2,'?y','?z']],
                                        [R,'?x','?z']]).
axiom_expandsxpr(formula(transitive_over(R,R2)),['<=>',
                                        [and,
                                         [R,'?x','?y','?t'],
                                         [R2,'?y','?z','?t']],
                                        [R,'?x','?z','?t']]).
axiom_expandsxpr(homeomorphic_for(R,U),['=>',
                                        [and,
                                         [R,'?x','?y','?t'],
                                         [instance_of,'?x',U,'?t']],
                                        [instance_of,'?y',U,'?t']]).
axiom_expandsxpr(homeomorphic_for(R,U),['=>',
                                        [and,
                                         [R,'?x','?y'],
                                         [instance_of,'?x',U]],
                                        [instance_of,'?y',U]]).
% TODO: need to know if domain/range variables are temporally instantiated
axiom_expandsxpr(holds_atemporally_between(R,X,Y),['=>',
                                               [R,'?x','?y'],
                                               [and,
                                                [instance_of,'?x',X],
                                                [instance_of,'?y',Y]]]).
axiom_expandsxpr(domain(R,U),['=>',
                              [R,'?x','?_'],
                              [instance_of,'?x',U]]).
axiom_expandsxpr(range(R,U),['=>',
                             [R,'?_','?x'],
                             [instance_of,'?x',U]]).


axiom_expandsxpr(all_some_all_times(TR,PR),['<=>',
                                            [TR,'?X','?Y'],
                                            [forall,['?x','?t'],
                                             ['=>',
                                              [instance_of,'?x','?X','?t'],
                                              [exists,'?y',[and,
                                                            [PR,'?x','?y','?t'],
                                                            [instance_of, '?y','?Y','?t']]]]]]).
axiom_expandsxpr(all_some_tr(TR,PR),['<=>',
                                     [TR,'?X','?Y'],
                                     [forall,['?x','?t'],
                                      ['=>',
                                       [instance_of,'?x','?X','?t'],
                                       [exists,['?y'],[and,
                                                       [exists,['?t1'],
                                                        [and,
                                                         [PR,'?x','?y','?t1'],
                                                         [instance_of, '?y','?Y','?t1']]],
                                                       [forall,['?t2'],
                                                        ['=>',[and, [exists_at,'?x','?t2'],
                                                               [exists_at,'?y','?t2']],
                                                         [PR,'?x','?y','?t2']]]]]]]]).
                                                        
axiom_expandsxpr(all_some(TR,PR),['<=>',
                                  [TR,'?X','?Y'],
                                  [forall,['?x'],
                                   ['=>',
                                    [instance_of,'?x','?X'],
                                    [exists,'?y',[and,
                                                  [PR,'?x','?y'],
                                                  [instance_of,'?y','?Y']]]]]]).

% WITH PRECONDITIONS
axiom_expandsxpr(equivalent_to(U,Union),
                 ['<=>',
                  [instance_of,'?x',U],
                  [or|Axs]],
                 (   Union=..[disjoint_union|L],
                     findall([instance_of,'?x',X],member(X,L),Axs))).

axiom_expandsxpr(equivalent_to(R,Union),
                 ['<=>',
                  [R,'?x','?y'],
                  [or|Axs]],
                 (   Union=..[union_of|L],
                     relation(R),
                     findall([R2,'?x','?y'],member(R2,L),Axs))).
axiom_expandsxpr(equivalent_to(R,Intersection),
                 ['<=>',
                  [R,'?x','?y'],
                  [and|Axs]],
                 (   Intersection=..[intersection_of|L],
                     relation(R),
                     findall([R2,'?x','?y'],member(R2,L),Axs))).

% LOGIC GRAMMAR
% nlformula(all_some_all_times(R,PR)) --> 


new_axiom(R,A):- relation(R),autoaxiom(R,A).

autoaxiom(R,label(R,X)):- \+ label(R,_),formal_label(R,X).
autoaxiom(R,subrelation(R,R2)):- class_instance_relation_pair(R,PR),subrelation(PR,PR2),class_instance_relation_pair(PR2,R2).
autoaxiom(R,transitive_over(R,R2)):- class_instance_relation_pair(R,PR),transitive_over(PR,PR2),class_instance_relation_pair(PR2,R2).
autoaxiom(R,domain(R,X)):- subrelation(R,R2),domain(R2,X).
autoaxiom(R,range(R,X)):- subrelation(R,R2),range(R2,X).
autoaxiom(R,type_type(R)):- class_instance_relation_pair(R,_).
autoaxiom(R,exported_identifier(R,X)):- class_instance_relation_pair(R,PR),exported_identifier(PR,PX),atom_concat('OBO_REL_I:',Local,PX),atom_concat('OBO_REL_C:',Local,X).

missing_axiom(R,exported_identifier):- \+ exported_identifier(R,_).

/** <module> prolog model for obolog language

  ---+ Synopsis

==
:- use_module(bio(obolog_db)).

% 
demo:-
  nl.
  

==

---+ Details

TODO: split this into a generic commonlogic component and obolog unreified clauses

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
