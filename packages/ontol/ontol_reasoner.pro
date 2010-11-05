/* -*- Mode: Prolog -*- */
:- module(ontol_reasoner,
          [asserted_fact/1,
           find_all_entailments/0,
	   show_explanation/3,
	   show_all_explanations/0,
           show_all_explanations/1,
           show_all_explanations/2,
           show_all_explanations/3,
           is_redundant/1,
	   retractall_redundant/0,
           entailable/1,
           entailable/2,
           explanation/3,
           explanation/4
          ]).
user:dynamic_db(ontol_db).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

:- op(1000,xfy,(<-)).
:- op(1200,xfy,(::)).

/* ----------------------------------------
   FORWARD CHAINING ENGINE
   ----------------------------------------*/

% we use a meta-circular interpreter to iteratively forward chain through all rules+facts,
% adding facts until no more facts are to be added
% this involves lots of messy prolog-nonos such as repeat/0 and global variables..


%% find_all_entailments/0 is det
% forward chain until all facts are added
find_all_entailments:-
        statistics(cputime,T1),
        % see inductive_isa
        forall(temporary_fact(Fact,S),
               add_fact(S,Fact)),
        findall((Head <- Body :: RuleName - Reason),
                (   Head <- Body :: RuleName - Reason),
                Rules),
        %findall(Rule,(member(Rule,Rules),\+ negrule(Rule)),Rules1),
        nb_setval(negation,false),
        sweep_loop(Rules),
        (   \+ \+ complement_of(_,_)
        ->  nb_setval(negation,true),
            sweep_loop(Rules)
        ;   true),
        retract_temporary_facts,
        nb_getval(num_sweeps,NumSweeps),
        debug(reasoner,'num sweeps final: ~w',NumSweeps),
        statistics(cputime,T2),
        TotalTime is T2-T1,
        debug(timing,'Total reasoner time = ~w',[TotalTime]).

sweep_loop(Rules):-
        debug(reasoner,'Rules: ~w',[Rules]),
        nb_setval(num_sweeps,0),
        repeat,
        nb_getval(num_sweeps,NumSweeps),
        debug(reasoner,'sweeps: ~w',NumSweeps),
        sweep(Rules),
        (   nb_getval(facts_added,0 )
        ->  !
        ;   NumSweepsPlus1 is NumSweeps +1,
            nb_setval(num_sweeps,NumSweepsPlus1),
            fail).


negrule(_ :: neg(_)-_).


% see inductive_isa
retract_temporary_facts:-
        % ----------------------------------------
        % retract all temp facts
        % ----------------------------------------
        % find all temporary instances
        solutions(I,
                  (   entailed_by(inst_of(I,_),temp(_))
                  ;   entailed_by(_,temp(inst_of(I,_)))),
                  TempInsts),
        TempInsts\=[],
        !,
        % this actually fails to do a full retract
        forall(entailed_by(Fact,temp(_)),
               cascading_retract(Fact)),
        % remove any fact with a temp inst in the subject position
        solutions(Fact,
                  (   entailed_by(Fact,_),
                      (   Fact =.. [_,I|_]
                      ;   Fact =.. [_,_,I|_]),
                      member(I,TempInsts)),
                  FactsToGo),
        maplist(cascading_retract,FactsToGo).
retract_temporary_facts:- !.


%% sweep(+Rules) is sdet
% perform a single sweep through all rules
sweep(Rules):-
        debug(reasoner,'sweep: ~w',[Rules]),
        nb_setval(facts_added,0),
        forall(member(Rule,Rules),
               apply_rule(Rule)).

%threaded_sweep(Rules):-
%        forall(member(Rule,Rules),
%               thread_create(sweep(Rule),Thread,[alias(Rule)])),

% this way is often faster, but has stack problems with FMA
foo___apply_rule((Head <- Body :: RuleName - _)):-
        Rule = (Body,\+ Head), % exclude facts that have already been prolog-asserted
        debug(reasoner,'applying: ~w ',[RuleName]),
        % todo - experiment with iterating through using forall/2
        (   findall(Head,Rule,Heads),
            Heads\=[]
        ->  %statistics,
            length(Heads,NumNewFactsWithDupes),
            debug(reasoner,'num new facts with dupes: ~w',[NumNewFactsWithDupes]),
            %sort(Heads,NewFacts), % TOO MUCH STACK
            NewFacts=Heads,
            %statistics,
            %length(NewFacts,NumNewFacts),
            %debug(reasoner,'num new facts, sorted: ~w',[NumNewFacts]),
            forall(member(NewFact,NewFacts),
                   add_fact(RuleName,NewFact)),
            nb_setval(facts_added,1)
        ;   true).

apply_rule((Head <- Body :: RuleName - _)):-
        Rule = (Body,\+ Head), % exclude facts that have already been prolog-asserted
        debug(reasoner,'applying: ~w ',[RuleName]),
        forall(Rule,
               add_fact(RuleName,Head)).

add_fact(RuleName,(Fact,Facts)):-
        !,
        add_fact(RuleName,Fact),
        add_fact(RuleName,Facts).
add_fact(RuleName,Fact):-
        \+ Fact,
        !,
        nb_setval(facts_added,1),
        debug(reasoner_detail,'adding: ~w',[Fact]),
        assert(ontol_db:Fact),
        assert(ontol_db:entailed_by(Fact,RuleName)).
add_fact(_,_). % exists


/* ----------------------------------------
   UTIL
   ----------------------------------------*/

entailable(Fact):-
        entailable(Fact,_).
entailable(Fact,RuleName-Reason):-
        (   Fact <- Body :: RuleName - Reason),
        Body.
entailable(Fact,RuleName-Reason,Body):-
        (   Fact <- Body :: RuleName - Reason),
        Body.

show_all_redundant:-
        Fact=ontol_db:subclass(X,Y),
        Fact,
        \+ entailed_by(Fact,_),
        (   test_noncircular_entailable_nb(Fact)
        ->  write('  --RD: ')
        ;   write('  ++NR: ')),
        class(X,XN),
        class(Y,YN),
        format('  ~w "~w" subClassOf ~w "~w"~n',[X,XN,Y,YN]),
        fail.



test_noncircular_entailable_nb(Fact):-
        build_evidence_chain,
        Fact,
        cascading_retract(Fact),
        (   entailable(Fact)
        ->  Pass=true
        ;   Pass=fail),
        assert(Fact),
        find_all_entailments,
        build_evidence_chain,
        Pass.

test_noncircular_entailable(Fact):-
        Fact,
        retractall(Fact),
        % TODO: proper triggering
        forall(entailed_by(EFact,_),
               retractall(EFact)),
        find_all_entailments,
        (   Fact
        ->  true
        ;   assert(Fact),
            fail).

%% build_evidence_chain is det
% only facts not in asserted db
build_evidence_chain:-
        forall(entailed_by(Fact,Rule),
               build_evidence_chain(Fact,Rule)).

% warning: if this is called, cascading deletes may end up deleting given links
build_evidence_chain_all:-
        forall((assertable(Fact),Fact),
               build_evidence_chain(Fact,_Rule)).

build_evidence_chain(Fact):-
        build_evidence_chain(Fact,_).
build_evidence_chain(Fact,Rule):-
        (   Fact <- Body :: Rule - _),
        Body,
        !,
        evidence_facts(Body,EvFacts),
        forall(member(EvFact,EvFacts),
               store_evidence(Fact,Rule,EvFact)).
build_evidence_chain(Fact,Rule):-
        writeln(cannot(Fact,Rule)).

:- dynamic fact_evidence/2.
build_full_evidence_chain:-
        forall((assertable(Fact),Fact),
               build_full_evidence_chain(Fact,_)).

build_full_evidence_chain(Fact,_Rule):-
        (   entailed_by(Fact,_)
        ->  true
        ;   assert(fact_evidence(Fact,[given]))),
        forall( (entailable(Fact,_,Body),evidence_facts(Body,EvFacts)),
                (   assert(fact_evidence(Fact,EvFacts)))).

/*
% requires build_full_evidence_chain/0
noncircular_entailable(Fact):-
        fact_evidence(Fact,EvFacts),
        EvFacts\=[given],
        forall(member(EvFact,EvFacts),
               noncircular_entailable(EvFact,Fact,[])).
noncircular_entailable(given,_,_):- !.
noncircular_entailable(Fact,OrigFact,Seen):-
        \+ member(Fact,Seen),   % no circularities
        Fact\=OrigFact,
        writeln(noncircular_entailable(Fact,OrigFact,Seen)),
        fact_evidence(Fact,EvFacts),
        forall(member(EvFact,EvFacts),
               noncircular_entailable(EvFact,OrigFact,[Fact|Seen])).
*/

store_evidence(_,_,_\=_):- !.
%store_evidence(Fact,_,_):- Fact,!.
store_evidence(Fact,Rule,EvFact):-
        assert(ontol_db:entailed_by(Fact,Rule,EvFact)).

cascading_retract(Fact):-
        retractall(Fact),
        debug(reasoner,'Cascading delete: ~w',[Fact]),
        forall(entailed_by(SupportedFact,_,Fact),
               cascading_retract(SupportedFact)).

        
        

%entailment_chain(Fact,Chain):-
%        entailed_by(Fact,RuleName),
%        (   Fact <- Body :: RuleName - _),
%       Body,
        
possible_entailment_chain(Fact,Chain):-
        possible_entailment_chain(Fact,Chain,Fact,[]).
possible_entailment_chain(Fact,rule(RuleName,SubChains),OrigFact,CheckedList):-
        \+ member(Fact,CheckedList),
        % writeln(CheckedList),
        (   Fact <- Body :: RuleName - _),
        Body,
        % format(' ~w <- ~w~n',[Fact,Body]),
        evidence_facts(Body,EvFacts), % convert prolog goal to list of facts
        % test for circularity:
        forall(member(EvFact,EvFacts),
               EvFact\=OrigFact),
        %format(' :: ~w <- ~w~n',[Fact,EvFacts]),
        findall(SubChain,
                (   member(EvFact,EvFacts),
                    \+ member(EvFact,CheckedList),
                    possible_entailment_chain(EvFact,SubChain,OrigFact,[Fact|CheckedList])),SubChains),
        length(SubChains,Len),
        length(EvFacts,Len), % all must be accounted for
        !.                      % alternate solutions...?
possible_entailment_chain(Fact,asserted(Fact),OrigFact,CheckedList):-
        Fact\=OrigFact, % no circularity allowed
        \+ member(Fact,CheckedList),
        \+ entailed_by(Fact,_),
        !.

nontrivial(genus_differentia).
nontrivial(differentia).
nontrivial(genus).

redundant_fact(Fact) :-
        assertable(Fact),
        Fact,
        debug(reasoner,'testing_for_redundancy: ~w',[Fact]),
                                %entailable_acyclic(Fact),
        entailed_by(Fact,Rule),
        \+ nontrivial(Rule),
        debug(reasoner,'** redundant[~w]: ~w',[Rule,Fact]).

% deprecated - use ontol_management
retractall_redundant:-
        forall( redundant_fact(Fact),
                retractall(Fact)).

retractall_entailed:-
        forall(entailed_by(Fact,_),
               retractall(Fact)).

listtab(L):-
        forall(member(_,L),write(' ')).



% true if Fact is given, can be entailed, and the entailment does not depend on the fact itself
entailable_acyclic(Fact):-
        entailable_acyclic(Fact,Fact,[]).
%entailable_acyclic(Fact,OrigFact,CheckedList):-
%        nl,listtab(CheckedList),write('trying '),wfact(Fact),fail.
entailable_acyclic(Fact,OrigFact,_L):-
        Fact\=OrigFact, % do not do this check in the top level recursive call
        \+ entailed_by(Fact,_),
        %nl,listtab(L),write('given '),wfact(Fact),
        !.                      % GIVEN
entailable_acyclic(Fact,OrigFact,CheckedList):-
        %nl,listtab(CheckedList),write('fact '),wfact(Fact),
        \+ member(Fact,CheckedList),
        % a single explanation will suffice
        %(   Fact <- Body :: _), % TODO: use cached version
        %Body,
        %evidence_facts(Body,EvFacts), % convert prolog goal to list of facts
        %nl,listtab(CheckedList),write(evfacts(EvFacts)),
        %format('~q.~n',[Fact]),
        %\+ \+ entailed_by(Fact,_,_),
        fact_evidence(Fact,EvFacts),
        EvFacts\=[given],
        forall(member(EvFact,EvFacts),
                                %forall(entailed_by(Fact,_,EvFact),
               (   EvFact\=OrigFact,
                                %\+ member(EvFact,CheckedList),
                   entailable_acyclic(EvFact,OrigFact,[Fact|CheckedList]))).
xxentailable_acyclic(Fact,_OrigFact,CheckedList):-
        nl,listtab(CheckedList),
        write('FAIL '),wfact(Fact),
        fail.

wfact(subclass(X,Y)):- !,wfact(restriction(X,is_a,Y)).
wfact(restriction(X,R,Y)):-
        class(X,XN),
        class(Y,YN),
        format('~w "~w" ~w ~w "~w"',[X,XN,R,Y,YN]),
        !.
wfact(X):- write(X).

doall:-
        find_all_entailments,
        build_full_evidence_chain,
        show_all_entailable_acyclic.

show_all_entailable_acyclic:-
        Fact=subclass(X,Y),
        Fact,
        \+ entailed_by(Fact,_),
        (   entailable_acyclic(Fact)
        ->  write('  --RD: ')
        ;   write('  ++NR: ')),
        class(X,XN),
        class(Y,YN),
        format('  ~w "~w" subClassOf ~w "~w"~n',[X,XN,Y,YN]),
        fail.

        

write_entailment_chain(Chain):-
        write_entailment_chain(Chain,'  ','').
write_entailment_chain(Chain,Del,Tab):-
        atom_concat(Del,Tab,Tab2),
        nl,
        write(Tab),
        write_entailment_chain_detail(Chain,Del,Tab2).
write_entailment_chain_detail(rule(Rule,L),Del,Tab):-
        write(Rule),
        forall(member(Chain,L),
               write_entailment_chain(Chain,Del,Tab)).
write_entailment_chain_detail(asserted(_ \= _),_,_):- !.
write_entailment_chain_detail(asserted(Fact),_,_):-
        Fact =.. [F|Args],
        format('~w(',[F]),
        forall(member(Arg,Args),
               (   (   entity_label(Arg,Label)
                   ->  write(Label)
                   ;   write(Arg)),
                   write(', '))),
        write(')'),
        nl.



evidence_facts((X,L),[X|L2]):-
        !,
        evidence_facts(L,L2).
evidence_facts([],[]):- !.
evidence_facts([X|L],[X|L2]):-
        !,
        evidence_facts(L,L2).
evidence_facts(forall(X,Y),L):-
        !,
        findall(Y,X,L1),
        evidence_facts(L1,L).
evidence_facts(X,[X]).



assertable(subclass(_,_)).
assertable(restriction(_,_,_)).
unentailed_fact(Fact):-
        assertable(Fact),
        Fact,
        \+ entailed_by(Fact,_).

% DOES NOT WORK :
% currently there is no way to tell if a fact was pre-reasoned
asserted_fact(Fact):-
        assertable(Fact),
        Fact,
        \+ (clause_source_short(Fact,'ontol_reasoner.pro')).
asserted_entailable_fact(Fact):-
        asserted_fact(Fact),
        entailed_by(Fact,_).


/* ----------------------------------------
   EXPLANATIONS
   ----------------------------------------*/
%% show_all_explanations
show_all_explanations:-
        show_all_explanations(text,false).
%% show_all_explanations(+Format)
show_all_explanations(Format):-
        show_all_explanations(Format,false).
%% show_all_explanations(+Format,+IsRecursive)
show_all_explanations(Format,IsRecursive):-
        findall(Name,(_ <- _ :: Name/_),Names),
        maplist(show_all_explanations(Format,IsRecursive),Names).
%% show_all_explanations(+Format,+IsRecursive,+RuleName)
show_all_explanations(Format,IsRecursive,RuleName):-
        forall((entailed_by(Fact,RuleName),\+is_redundant(Fact)),
               show_explanation(Format,IsRecursive,Fact,RuleName)).

show_explanation(Format,Fact,RuleName):-
        show_explanation(Format,false,Fact,RuleName).
show_explanation(obo,_IsRecursive,subclass(A,B),_):- % TODO
        !,
	ensure_loaded(bio(ontol_writer_obo)),
	write_axiom(obo,subclass(A,B)).
show_explanation(obo,_,_,_).


% matches any other format
show_explanation(_Format,IsRecursive,Fact,RuleName):-
        explanation(Fact,RuleName,Text,IsRecursive),
        !,
        writeln(Text),
        nl.

is_redundant(equivalent_class(X,X)).
is_redundant(subclass(X,X)).
is_redundant(subclass(X,Y)):-
        subclass(X,Z),X\=Z,subclass(Z,Y),Z\=Y.
is_redundant(restriction(X,R,Y)):-
        restriction(X,R,Z),X\=Z,restriction(Z,R,Y),Z\=Y.
is_redundant(restriction(X,R,Y)):- % under
        restriction(X,R,Z),X\=Z,subclass(Z,Y),Z\=Y.
is_redundant(restriction(X,R,Y)):- % over
        subclass(X,Z),X\=Z,restriction(Z,R,Y),Z\=Y.

%% explanation(+Fact,?RuleName,?Text) is nondet
explanation(Fact,RuleName,Text):-
        explanation(Fact,RuleName,Text,false).

%% explanation(+Fact,?RuleName,?Text,+IsRecursive) is nondet
% true if Text is a RuleName-based natural language explanation for Fact
explanation(Fact,RuleName,Text,IsRecursive):-
        debug(explain,'Explaining: ~w',[Fact]),
        explain_fact_body_rule_reasoner(Fact,Body,RuleName,Reason),
        %(   Fact <- Body :: RuleName - Reason),
        debug(explain,'  Reason for: ~w is: ~w',[Fact,Reason]),
        Body,
        debug(explain,'  Satisfies: ~w',[Body]),
        explain(0-IsRecursive,because(Fact,RuleName,Reason),Tokens,[]),
        concat_atom(Tokens,' ',Text).

explain_fact_body_rule_reasoner(Fact,Body,RuleName,Reason):-
        (   Fact <- Body :: RuleName - Reason).
explain_fact_body_rule_reasoner(Fact,Body,RuleName,Reason):-
        (   Facts <- Body :: RuleName - Reason),
        Facts=(_,_),
        debug(explain,'  Checking facts: ~w',[Facts]),
        member_terms(Fact,Facts).

member_terms(Fact,Fact):- !.
member_terms(Fact,(Fact,_)):- !.
member_terms(Fact,(_,Facts)):- !, member_terms(Fact,Facts).



incr(X,XPlus1):- XPlus1 is X+1.

tabul(I,I1) --> {I1 is I+1,ncat(' ',I,TabAtom)},['\n'],[TabAtom],[' '].

ncat(A,N,Atom):- ncat1(A,N,L),concat_atom(L,Atom).
ncat1(_,N,[]):- N =< 0,!.
ncat1(A,N,[A|AL]):- N2 is N-1,ncat1(A,N2,AL).

/* ----------------------------------------
   EXPLANATION GRAMMAR
   ----------------------------------------*/

%% Grammar: explain
% generates a natural language list of tokens from a Reason term
explain(_,true) --> [].
explain(I-_,because(Fact,RuleName,Reason)) --> !, tabul(I,I1), explain(I1-_,Fact),anciliary_facts(Fact),['\n  ',because,of,'rule:',RuleName,':\n    '],explain(I1-_,Reason).
explain(I-_,(Fact,Facts)) --> {trivial(Fact)},!, explain(I-_,Facts).
explain(I-_,(Fact,Facts)) --> !, tabul(I,I1), explain(I1-_,Fact),tabul(I,_),[and],explain(I1-_,Facts).
explain(_,_\=_) --> !,[].
explain(_,class(_)) --> [].
explain(I-_,subclass(X,X)) --> tabul(I,_),label(X),['='],label(X).
explain(I-_,subclass(X,Y)) --> tabul(I,_),label(X),[is_a],label(Y).
explain(_,disjoint_from(X,Y)) --> label(X),[disjoint_from],label(Y).
explain(_,holds_over_chain(R,Rs)) --> label(R),[holds_over_chain],[Rs].
explain(_,genus(X,Y)) --> label(X),[genus],label(Y).
explain(I-_,forall(Fact,Goal)) --> tabul(I,I1), {findall(Goal,(Fact,Goal),Goals)},explain(I1-_,Goals).
explain(I-_,\+ X) --> tabul(I,I1), ['not:('],explain(I1-_,X),[')'].
explain(I-_,X+Y) --> tabul(I,I1), explain(I1-_,X),[','],tabul(I,_),[and,it,is,the,case,that],explain(I1-_,Y).
explain(_,[]) --> [].
explain(I-_,[X|L]) --> {trivial(X)},!,tabul(I,I1), explain(I1-_,L).
explain(I-_,[X]) --> !, tabul(I,I1), explain(I1-_,X).
explain(I-_,[X|L]) --> tabul(I,I1), explain(I1-_,X),[and],explain(I1-_,L).
explain(_,differentium(_,R,X)) --> label(R),label(X).
explain(_,restriction(X,R,Y)) --> label(X),label(R),label(Y).
explain(_,is_unsatisfiable(X)) --> label(X),[is,unsatisfiable].
explain(_,is_reflexive(R)) --> label(R),[is,reflexive].
explain(_,is_transitive(R)) --> label(R),[is,transitive].
explain(_,holds_over_chain(R,RL)) --> label(R),['holds over chain: '],labellist(RL).
explain(I-_,cdef(C,G,Diffs)) --> tabul(I,I1), label(C),[is,defined,as,'{'],tabul(I,_),[a],label(G),[that],explain(I1-_,Diffs),['}'].
explain(I-_,rdef(R,RL)) --> tabul(I,_), label(R),[is,the,intersection,of,'['],labellist(RL),[']'].
%explain(I-_,and(Template)) --> tabul(I,I1), {findall(Template,Template,Templates)},explain(I1-_,Templates).
explain(I-_,and(Template)) --> {findall(Template,Template,Templates)},explain(I-_,Templates).
explain(_,T) --> {T=..[Pred,A1,A2]},label(A1),[Pred],label(A2).
explain(_,T) --> {T=..[Pred,A]},label(A),[Pred].
%explain(_,atom_concat(_,_,_))--> !, [].
explain(_,rr(_,_,_))--> !, [].
label(X) --> {entity_label(X,N),property(X)},!,[N].
label(X) --> {entity_label(X,N)},!,['[',X,' ! ',N,']'].
label(X) --> [X].
quote(X) --> {sformat(S,'"~w"',[X])},[S].
labellist([X]) --> label(X).
labellist([X|L]) --> label(X),[' & '],labellist(L).

% the reasoner may infer X is_a Y when X part_of Y is stated: we want to not the discrepancy
anciliary_facts(subclass(X,Y)) --> {restriction(X,R,Y),\+ entailed_by(restriction(X,R,Y),_)},!,[' asserted:'],explain(_,restriction(X,R,Y)).
anciliary_facts(subclass(X,Y)) --> {restriction(X,R,Y)},!,[' entailed:'],explain(_,restriction(X,R,Y)).
anciliary_facts(_) --> [].

%% trivial(?Fact)
% true if Fact does not need explaining - it is given, or it is inessential to the explanation
trivial(_\=_).                  % negation: not an essential part of the body, only there for optimization
trivial(class(_)).              % given
trivial(property(_)).              % given
trivial(instance(_)).              % given

% see inductive_isa
% we are overloading entailed_by here to store the instantiation fact
temporary_fact(Fact,temp(inst_of(has_part(Vars),Sub))):-
        class_instrule(Sub,Vars,Body),
        atom_concat('TEMP__',Sub,F),
        groundvars(Vars,F),
        tuple_to_list(Body,Facts),
        member(Fact,Facts).

groundvars([],_).
groundvars([H|T],S):-
        gensym(S,Sym),
        H=Sym,
        groundvars(T,S).

/* ----------------------------------------
   RULES
   ----------------------------------------*/
% TODO: check for cycles
% TODO: non-dynamic db, write to a file
% note: we include some negations for efficiency        

%% Head <- Rule :: NameAndReason
% NameAndReason may be of form Name-Reason or Name/given
% most of the time the Body suffices as the reason, except in more complex rules
% where a tailored explanation is required

% default
(   Head <- Rule :: Name - Rule ):- (Head <- Rule :: Name/given). % default reason is Body

subclass(X,X) <- class(X) :: isa_reflexivity/given.
subclass(X,X) <- property(X) :: subproperty_reflexivity/given.
%restriction(X,R,X) <- is_reflexive(R),class(X) :: reflexivity/given.
subclass(X,Z) <- subclass(X,Y),X\=Y,subclass(Y,Z),Y\=Z :: isa_transitivity/given.
subclass(X,Y) <- class_union_element(Y,X) :: isa_from_union/given.  % also covers relations
%restriction(X,R,Y) <- subclass(X,X1),restriction(X1,R,Y1),X1\=Y1,subclass(Y1,Y),all_some(R) :: transitivity_over_isa/given.
restriction(X,R,Y) <- restriction(X,R,Y1),X\=Y1,subclass(Y1,Y),Y1\=Y,all_some(R) :: transitivity_over_isa/given.
restriction(X,R,Y) <- subclass(X,X1),X\=X1,restriction(X1,R,Y),all_some(R) :: transitivity_under_isa/given.
restriction(X,R,Z) <- is_transitive(R),restriction(X,R,Y),X\=Y,restriction(Y,R,Z),Y\=Z :: transitivity/given.
restriction(X,R,Y) <- holds_bidirectionally_for(SR,R,_),restriction(X,SR,Y),X\=Y :: holds_bidirectionally_for/given. 
restriction(Y,IR,X) <- holds_bidirectionally_for(SR,_,IR),restriction(X,SR,Y),X\=Y :: holds_bidirectionally_for/given. 
restriction(X,R,Y) <- holds_bidirectionally_for(SR,R),restriction(X,SR,Y),X\=Y :: holds_bidirectionally_for/given. % DEPRECATED
restriction(X,RI,Y) <- holds_bidirectionally_for(SR,R),inverse_of(R,RI),restriction(Y,SR,X),X\=Y :: holds_bidirectionally_for/given.  % DEPRECATED
%restriction(X,R,Y) <- inverse_of(R,RI),restriction(Y,RI,X),X\=Y :: inverse/given.

% note: we don't need to check the all-some condition here
subclass(X,Y) <- subclass(X,YG),genus(Y,YG),forall(differentium(Y,R,To),restriction(X,R,To)) :: genus_differentia
 - (cdef(Y,YG,and(differentium(Y,_,_)))+(subclass(X,YG),forall(differentium(Y,R,To),restriction(X,R,To)))).
subclass(X,Y) <- class(Y), differentium(Y,R1,To1), \+ genus(Y,_), restriction(X,R1,To1),forall(differentium(Y,R,To),restriction(X,R,To)) :: differentia_nogenus
 - (cdef(Y,entity,and(differentium(Y,_,_)))+(subclass(X,entity),forall(differentium(Y,R,To),restriction(X,R,To)))).

%subclass(X,Y) <- \+ \+ intersection_of(Y,_,_),forall(intersection_of(Y,R,To),restriction(X,R,To)) :: intersection/given.
subclass(X,Y) <- genus(X,Y) :: genus/given.
restriction(X,R,Y) <- differentium(X,R,Y) :: differentium/given.

equivalent_class(X,Y) <- equivalent_class(Y,X) :: equiv_class/equiv_sym.
equivalent_class(X,Y) <- subclass(X,Y),subclass(Y,X),Y\=X :: equiv_class/given.
subclass(X,Y) <- equivalent_class(X,Y), Y\=X :: equiv_class_inv/given.
equivalent_class(X,Y) <- is_functional(R),restriction(A,R,X),restriction(A,R,Y) :: functional/given.


restriction(X,R,Z) <- restriction(X,R1,Z),subclass(R1,R),R1\=R :: subrelations/given.

restriction(X,R,Z) <- transitive_over(R,R2),restriction(X,R,Y),X\=Y,restriction(Y,R2,Z),Y\=Z :: transitive_over/given.
%restriction(X,R,Z) <- transitive_over(R,R2),restriction(X,R2,Y),X\=Y,restriction(Y,R,Z),Y\=Z :: transitive_over/given.

% a macro would be cleaner... max out at 4 for now
restriction(X,R,Y) <- holds_over_chain(R,[R1,R2]),restriction(X,R1,Z1),X\=Z1,restriction(Z1,R2,Y),Z1\=Y :: holds_over_chain/given.
restriction(X,R,Y) <- holds_over_chain(R,[R1,R2,R3]),restriction(X,R1,Z1),X\=Z1,restriction(Z1,R2,Z2),Z1\=Z2,restriction(Z2,R3,Y),Z2\=Y :: holds_over_chain/given.
restriction(X,R,Y) <- holds_over_chain(R,[R1,R2,R3,R4]),restriction(X,R1,Z1),X\=Z1,restriction(Z1,R2,Z2),Z1\=Z2,restriction(Z2,R3,Z3),Z2\=Z3,restriction(Z3,R4,Y),Z3\=Y :: holds_over_chain/given.

% relation intersections, type level. Take one at random and check it holds for the rest.
% TODO: is this valid for type-level relations?
restriction(X,R,Y) <- property_intersection_elements(R,RL),property(R),select(R1,RL,RLRest),restriction(X,R1,Y),forall(member(Rx,RLRest),restriction(X,Rx,Y)) :: relation_intersection
 - (rdef(R,RL)+forall(property_intersection_element(R,Rx),restriction(X,Rx,Y))).
% TODO: property_union

subclass(R1,R2) <- property_intersection_elements(R1,RL),member(R2,RL) :: subrelation_from_relation_intersection/given.

disjoint_from(X,Y) <- disjoint_from(Y,X) :: disjoint_symmetrical/given.
is_cyclic(R) <- restriction(X,R,Y),X\=Y,restriction(Y,R,X) :: cyclic/given.
%is_unsatisfiable(X) <- is_cyclic(R),is_acyclic(R).
is_unsatisfiable(X) <- subclass(X,P1),disjoint_from(P1,P2),subclass(X,P2) :: disjointness/given.
inst(I) <- inst_of(I,_) :: inst/given.
inst_of(I,C) <- inst_of(I,C1),subclass(C1,C) :: instantiation/given.
inst_rel(I,R,I2) <- is_transitive(R),inst_rel(I,R,Ix),inst_rel(Ix,R,I2) :: instance_relation_transitivity/given.
inst_rel(I,R,I2) <- inverse_of(R,R2),inst_rel(I2,R2,I) :: instance_level_inverse/given.
inst_rel(I,R,I2) <- is_symmetric(R),inst_rel(I2,R,I) :: instance_level_symmetry/given.
inst_rel(I,R,I2) <- inst_rel(I,R2,I2),subclass(R2,R) :: instrel_subrelation/given.
class_level_inverse_of(X,Y) <- class_level_inverse_of(Y,X) :: inverse_symmetry/given.
inverse_of_on_instance_level(X,Y) <- inverse_of_on_instance_level(Y,X) :: inst_inverse_symmetry/given.
inverse_of(X,Y) <- inverse_of(Y,X) :: inverse_symmetry/given.

% TODO: check why this is so slow
is_unsatisfiable(A) <- disjoint_over(DR,R),restriction(X,DR,Y),restriction(A,R,X),restriction(A,R,Y) :: disjoint_over_unsat/given.


% arbitrary rules
inst_of(has_part(SVars),C) <- var(SVars),class_instrule(C,Vars,Body),Body,sort(Vars,SVars) :: instrule/given.
inst_rel(I,has_part,X) <- I=has_part(Parts),inst(I),nonvar(Parts),member(X,Parts) :: instrel_skolem/given.

% meta-level reasoning step. we test subsumption between rules by creating a canonical instance of the subclass,
% then testing the instantiation rule for the superclass. Requires test instances to be in place. 
subclass(Sub,Super) <- entailed_by(_,temp(inst_of(I,Sub))),inst_of(I,Super),Super\=Sub :: inductive_isa/given.

/*
subclass(Sub,Super) <-
        class_reified_rulebody(Sub,_,ConjSub),
        class_reified_rulebody(Super,_,ConjSuper),
        Sub\=Super,
        matchall(ConjSuper,ConjSub) :: subclass_rulesub/given.

matchall([],_):- !.
matchall([Term|Terms],SubClassTerms):-
        select(SubTerm,SubClassTerms,SubRest),
        trace,
        matchterm(Term,SubTerm),
        matchall(Terms,SubRest).

matchterm(differentium(X,R,Y),differentium(X2,R2,Y2)):-
        subclass(X2,X),
        subclass(R2,R),
        subclass(Y2,Y).
matchterm(genus(X,G),genus(X2,G2)):-
        subclass(X2,X),
        subclass(G2,G).
*/

%inst_of(I,C) <- var(I),class_instrule(C,Body),Body,atom_concat(C,'-anon',S),gensym(S,I) :: instrule/given.

inst_rel(I,TR,I2) <- transitive_form_of(TR,R),inst_rel(I,R,I2) :: transitive_form_of/given.
is_transitive(R) <-  transitive_form_of(R,_) :: is_transitive/given.

inst_rel(I,CR,I2) <- cyclic_form_of(CR,R),inst_rel(I,R,I2),inst_rel(I2,R,I),I\=I2 :: cyclic_form_of/given.
is_cyclic(R) <-  cyclic_form_of(R,_) :: is_cyclic/given.

% forms directed cycle over
%inst_rel(I,CR,I) <- cyclic_over(CR,R),inst_rel_cycle(I,R) :: cyclic_over/given.
inst_rel(X,CR,Y) <- cyclic_over(CR,R),inst_rel_directed_simple_cycle_involving(X,R,Y) :: cyclic_over/given.

inst_rel(X,CR,Y) <- directed_simple_path_over(CR,R),inst_rel_directed_simple_path(X,R,Y) :: cyclic_over/given.

inst_rel(X,CR,X) <- reflexive_over(CR,R),inst_rel(X,R,X) :: cyclic_over/given.

inst_rel(X,R,Y) <- holds_over_chain(R,[R1,R2]),inst_rel(X,R1,Z1),X\=Z1,inst_rel(Z1,R2,Y),Z1\=Y :: holds_over_chain_inst/given.

% todo - class vs instance level relations
inst_of(I,C) <- inst_of(I,G),genus(C,G),forall(differentium(C,R,Filler),inst_satisfies_differentium(I,R,Filler)) :: genus_differentia_inst/given.



% CARDINALITY
% note: there will be redundant entailments X minc 2 Y is not inconsistent with X minc 4 Y
cardinality_restriction(X,R,Q,Y) <- subclass(X,X1),cardinality_restriction(X1,R,Q,Y),all_some(R) :: ctransitivity_over_isa/given.
min_cardinality_restriction(X,R,Q,Y) <- cardinality_restriction(X,R,Q,Y) :: minc/given.
max_cardinality_restriction(X,R,Q,Y) <- cardinality_restriction(X,R,Q,Y) :: maxc/given.
max_cardinality_restriction(X,R,Q,Y) <- min_cardinality_restriction(X,R,Q,Y),max_cardinality_restriction(X,R,Q,Y) :: cmin_eq_cmax/given.

min_cardinality_restriction(X,R,Q,Y) <- subclass(X,X1),min_cardinality_restriction(X1,R,Q,Y),all_some(R) :: cmintransitivity_over_isa/given.
max_cardinality_restriction(X,R,Q,Y) <- subclass(X,X1),max_cardinality_restriction(X1,R,Q,Y),all_some(R) :: cmaxtransitivity_over_isa/given.
% we can infer minimum cardinality over restriction, but not maximum, as there may be unspecified other paths.
min_cardinality_restriction(X,R,Q,Z) <- is_transitive(R),min_cardinality_restriction(X,R,Q1,Y),X\=Y,min_cardinality_restriction(Y,R,Q2,Z),Y\=Z,Q is Q1*Q2 :: ctransitivity/given.

% we need the inv link to disallow e.g. body hp 2 arm hp 5 digit -> body has_part max 10 digit
max_cardinality_restriction(X,R,Q,Z) <- is_transitive(R),max_cardinality_restriction(X,R,Q1,Y),X\=Y,restriction(Y,RI,X),inverse_of_on_instance_level(RI,R),max_cardinality_restriction(Y,R,Q2,Z),restriction(Z,RI,Y),Y\=Z,Q is Q1*Q2 :: maxctransitivity/given.

% cardinality in differentia handled differentlu
cardinality_restriction(X,R,Q,Y) <- differentium(X,card(R,Q,Q),Y) :: ctransitivity_diff/given.
min_cardinality_restriction(X,R,Q,Y) <- differentium(X,card(R,Q,_),Y) :: cmintransitivity_diff/given.
max_cardinality_restriction(X,R,Q,Y) <- differentium(X,card(R,_,Q),Y) :: cmaxtransitivity_diff/given.


% e.g. has_input . has_part -> has_indirect_input
min_cardinality_restriction(X,R,Q,Z) <- holds_over_chain(R,[R1,R2]),restriction(X,R1,Y),min_cardinality_restriction(Y,R2,Q,Z) :: cmintransitivity_chain/given.
restriction(X,R,Y) <- min_cardinality_restriction(X,R,Q,Y), Q>=1 :: somevalues_card/given.


% e.g. has_input . has_part -> has_indirect_input
% TODO: do this at end
cardinality_restriction(X,R,QSum,Z) <- holds_over_chain(R,[R1,R2]),
   \+ \+ ((restriction(X,R1,Y),cardinality_restriction(Y,R2,_,_))),
   setof(Y-Q,(restriction(X,R1,Y),cardinality_restriction(Y,R2,Q,Z)),YQs), % TODO
   format(user_error,'~w~n',[X-R-Z-YQs]),
   findall(Q,member(_-Q,YQs),Qs),
   sumlist(Qs,QSum)                                                                       :: ctransitivity_chain/given.

% relation inference
is_transitive(R) <- is_reflexive(R) :: reflexivity_implies_transitivity/given.

holds_over_chain(R,[R1,R2]) <- equivalent_to_chain(R,[R1,R2]) :: ec2/given.
holds_over_chain(R,[R1,R2,R3]) <- equivalent_to_chain(R,[R1,R2,R3]) :: ec2/given.
holds_over_chain(R,[R1,R2,R3,R4]) <- equivalent_to_chain(R,[R1,R2,R3,R4]) :: ec2/given.


is_unsatisfiable(X) <- min_cardinality_restriction(X,R,Min,Y),max_cardinality_restriction(X,R,Max,Y),Min>Max :: inconsistent_cardinality/given.


%% inst_satisfies_differentium(+I,+R,+Filler)
inst_satisfies_differentium(I,R,Filler):-
        inst_rel(I,R,X),
        inst_of(X,Filler).
inst_satisfies_differentium(I,R,Filler):-
        complement_of(R,RPos),
        nb_getval(negation,true),
        \+ ((inst_rel(I,RPos,X),
             inst_of(X,Filler))).


inst_satisfies_differentium(I,card(R,Min,Max),Filler):-
        setof_count(X,
                    (   inst_rel(I,R,X),
                        inst_of(X,Filler)),
                    Num),
        Num >= Min,
        Num =< Max.

% remember, here variables unify with terms
% clp( VX #< VY ) <- rule(X < Y), termvar(X,VX), termvar(Y,VY).

inst_rel_cycle(Init,Rel):-
        inst_rel_cycle(Init,Init,Rel,[]).
inst_rel_cycle(X,X,_,[_|_]).
inst_rel_cycle(Init,X,Rel,Path):-
        inst_rel(X,Rel,Y),
        \+ member(Y-X,Path),
        \+ member(X-Y,Path),  % to avoid cycles in reasoning
        inst_rel_cycle(Init,Y,Rel,[X-Y|Path]).

inst_rel_directed_simple_cycle_involving(X,Rel,Y):-
        inst_rel_directed_simple_path(X,X,X,Rel,[],Path),
        member(Y-_,Path).

inst_rel_directed_simple_path(Init,Rel,End):-
        inst_rel_directed_simple_path(Init,End,Init,Rel,[],_).
inst_rel_directed_simple_path(_,End,X,Rel,Path,Path):-
        inst_rel(X,Rel,End),
        \+ member(End-X,Path).
inst_rel_directed_simple_path(Init,End,X,Rel,Path,PathOut):-
        inst_rel(X,Rel,Y),
        \+ member(Y-_,Path),
        \+ member(_-Y,Path),
        inst_rel_directed_simple_path(Init,End,Y,Rel,[X-Y|Path],PathOut).


/* ----------------------------------------
   TESTS
   ----------------------------------------*/


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(po,
            [],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_reasoner)),
                ensure_loaded(bio(bioprolog_util)),
                ensure_loaded(bio(io)),
                load_biofile('parent_over_test.go'),
                find_all_entailments),
            true)).

/** <module> Forward chaining reasoner for OBO style ontologies

  ---+ Synopsis

==
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(io)).

% 
demo:-
  load_bioresource(obo(fly_anatomy)),
  find_all_entailments,
  class(Eye,eye),
  forall((restriction(Eye,develops_from,P),class(P,PN)),
         format('  Develops-from: ~w ~w~n',[P,PN])).
==

---+ Details

This module combines

 * A general purpose forward chaining prolog engine
 * A collection of horn clause rules for OBO style ontologies (view source to see these)
 * A grammar for inference explanations

In future, these may be split into 3 separate modules. This would allow mixing and matching of components.


 
@author  Chris Mungall
@version $Revision$
@see     README, ontol_db.pro, subclass/2, restriction/3, class/1, property/1, is_transitive/1
@license License


*/
