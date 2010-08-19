/* -*- Mode: Prolog -*- */

:- module(dbmeta,
          [
           fact_source/2,
           fact_clausesource/2,
           datapred/2,
           datapreds/1,
           datapreds/2,
           materialize_views/1,
           materialize_view/2,
           materialize_view/1,
           merge_facts/2,
           remove_duplicates/1,
           remove_duplicates/2,
           pred_query/3,
           pred_query_setof/5,
           pred_query_bagof/5,
           pred_query_findall/5,
           db_fact/2,
           db_facts/2,
           delete_all_facts/1,
           insert_facts/1,
           insert_facts/2,
           insert_fact/1,
           insert_fact_with_source/2,
           lookup_or_insert/3,
           write_db_fact/1,
           write_db_facts/1,
           write_db_facts/2,
           write_db_facts/3,
           write_dbsubset_facts/3,
	   write_db_fact_chain/2,
	   write_db_fact_chain/3,
           write_db_summary/0,
           write_db_summary/1,
           write_db_summary/2,
           (pred)/1,
           (pure)/1,
           (extensional)/1,
           pure_pred/1,
           is_pure_pred/1,
           op(800,xfx,with),
           op(1150,xfx,(pred)),
           op(850,xfx,(extensional)),
           op(1150,fx,(pure)),
           op(800,xfy,leftjoin),
           leftjoin/2
          ]).

:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util)).

:- op(1150,fx,pred).
:- op(850,fx,extensional).
:- op(800,xfx,with).
:- op(800,xfy,leftjoin).
:- module_transparent leftjoin/2.
:- module_transparent merge_facts/2.
:- module_transparent remove_duplicates/1.
:- module_transparent remove_duplicates/2.
:- module_transparent lookup_or_insert/3.

%% datapred(?Mod,?Pred) is nondet.
%   this is a dynamic db fact, specifying that Pred is defined in Mod

%% datapred_spec(?Mod,?PredSpec)
%   this has an introspectable predicate definition
:- dynamic
        user:source_localpath/2,
        fact_source/2,
        datapred/2,             % datapred(Mod,Pred)
        datapred_spec/2,
        datamod/1.

:- multifile user:dynamic_db/1.
:- multifile user:dynamic_db_pred/2.
:- multifile user:persistent/3.
:- multifile user:schema_statistic/2.
:- multifile user:suppress_fact/1.
:- multifile pure_pred/1.

:- multifile fact_chain_hook/2.

:- module_transparent materialize_view/2.
:- module_transparent materialize_view/1.
:- module_transparent datapreds/1.
%:- module_transparent (pred)/1.
:- module_transparent (extensional)/1.
%:- module_transparent (pure)/1.


pred(Heads):-
	throw(error(context_error(nodirective, pred(Heads)), _)).
pure(Heads):-
	throw(error(context_error(nodirective, pure(Heads)), _)).

pure_pred(pure_pred/1).

is_pure_pred(X) :- pure_pred(X).
is_pure_pred(X) :- pure_pred(Xs),member_pure_pred(X,Xs).

member_pure_pred(X,(X,_)).
member_pure_pred(X,(_,Xs)) :- member_pure_pred(X,Xs).


extensional Pred:-
        datapreds([Pred]).



datapreds(Preds):-
        context_module(Mod),
        datapreds(Mod,Preds).
% defines predicates - tells parent module that Preds are multifile
datapreds(Mod,Preds1):-
        forall(member(Pred,Preds1),
               assert(datapred_spec(Mod,Pred))),
        normalize_preds(Preds1,Preds),
        assert(datamod(Mod)),
        (   user:dynamic_db(Mod)
        ->  debug(dbmeta,'dynamic_db ~w',[Mod]),
            forall(member(Pred,Preds),
                   dynamic(Mod:Pred))
        ;   true),
        forall(user:dynamic_db_pred(Mod,Pred),
               dynamic(Mod:Pred)),
        forall(user:dynamic_db_pred(Mod,Pred),
               export(Mod:Pred)),
        forall(member(Pred,Preds),
               (   assert(datapred(Mod,Pred)),
                   multifile(Mod:Pred))).

datamodule(Mod):-
        datapred(Mod,_).

normalize_preds(Ps1,Ps2):-
        findall(P2,(member(P,Ps1),normalize_pred(P,P2)),Ps2).

normalize_pred(P/A,P/A):- !.    % pred/arity
normalize_pred(P-_,P2):-        % pred-comment
        !,
        normalize_pred(P,P2).
normalize_pred(P,N/Arity):-     % pred(c1,c2,...,cn)
        P=..[N|Args],
        length(Args,Arity),
        !.


%% pred_query(+Pred,?Var,?Query)
% TEST
%
%  ==
%  % assumes fact: person(ID,FName,SName)
%  pred_query(person(ID,N,_),N,contains(fred))
%  ==
pred_query(Pred,Var,Query):-
        flatten([Var],Vars),    % list-or-unary
        member(AnyVar,Vars),
        pred_query1(Pred,AnyVar,Query).

pred_query1(Pred,Var,contains(SubAtom)):-
        Pred,
        % succeed zero_or_one times
        (sub_atom(Var,_,_,_,SubAtom) -> true ; fail).

pred_query1(Pred,Var,contains(SubAtom,i)):-
        Pred,
        downcase_atom(Var,Var2),
        downcase_atom(SubAtom,SubAtom2),
        % succeed zero_or_one times
        (sub_atom(Var2,_,_,_,SubAtom2) -> true ; fail).

pred_query_bagof(Pred,Var,Query,SelectVar,Results):-
        bagof(SelectVar,Pred^pred_query(Pred,Var,Query),Results).
        
pred_query_setof(Pred,Var,Query,SelectVar,Results):-
        %get_free_vars(Pred,SelectVar,Free),
        setof(SelectVar,Pred^pred_query(Pred,Var,Query),Results).
        
pred_query_findall(Pred,Var,Query,SelectVar,Results):-
        findall(SelectVar,pred_query(Pred,Var,Query),Results).

%get_free_vars(Pred,Var,FreeArgs):-
%        flatten([Var],Vars),    % list-or-unary
%        Pred =.. [_|Args],
%        setof(Arg,(member(Arg,Args),
%                   not((member(Var,Vars),
%                        Arg==Var))),
%              FreeArgs).
        
        
%% materialize_views(+DbModule) is det.
%  generates facts for all extensional predicates in DbModule TEST
%
% db modules consist of existential and intension predicates; ie
%tables and views. Calling this will turn all views into tables. May
%make some applications faster.
%
%Compare to memoization/tabling
:- mode materialize_views(+) is det.
materialize_views(Mod):-
        forall(datapred(Mod,Pred),
               materialize_view(Mod,Pred)).

%% materialize_view(+DbModule,+Predicate)
%   materialize a view (intensional predicate)
%
%% materialize_view(+DbModule:Predicate)
%   materialize a view (intensional predicate)
%
%  The prolog database consists of extensional and intensional
%predicates. In standard relational database terminology, there can be
%thought of as tables and views.
%
%views are defined in terms of other views or tables. this can
%sometimes lead to repeated computations. If we believe that we are
%going to be using the same view predicate multiple times
%
%materialized views are assert/1-ed and hence may in some cases be
%slower than their unmaterialized counterparts
%
%  ==
%  :- use_module(bio(ontol_db)).
%  :- use_module(bio(io)).
%  :- use_module(bio(dbmeta)).
%
%  % demonstrate pre-computing the closure of the subclassT/2 relation.
%  % note: the materializion incurs a startup cost and would only
%  % be performed if repeated use was to be made of subclassT/2, or if
%  % use of subclassT/2 was time-critical (ie if it was queried in
%  % response to user actions)
%  demo:-
%    load_bioresource(go),                % load gene ontology into in-memory db
%    set_prolog_flag(verbose,normal),
%    debug(dbmeta),
%    materialize_view(ontol_db:subclassT/2),  % precompute transitive closure over is_a
%    % results should be no different from without materialization, but may be faster from here
%    class(ID,apoptosis),                 
%    forall(subclassT(ID,PID),
%           (class(PID,PN),format('is_a parent: ~w ~w~n',[PID,PN]))).
%  
%  ==
%
%  
:- mode materialize_view(+,+) is det.
materialize_view(Mod,Pred):-
        pred_to_unground_term(Pred,PredTerm),
        debug(dbmeta,'Preparing to materialize: ~w',[Mod:Pred]),
        statistics(cputime,T1),
        findall(PredTerm,Mod:PredTerm,PredTerms),
        length(PredTerms,NumPredTerms),
        abolish(Mod:Pred),
        debug(dbmeta,'Asserting Materialized View: ~w',[Mod:Pred]),
        forall(member(PredTerm,PredTerms),
               asserta(Mod:PredTerm)),
        statistics(cputime,T2),
        TimeTotal is T2-T1,
        debug(dbmeta,'Materialized: ~w time: ~w facts: ~w',[Mod:Pred,TimeTotal,NumPredTerms]),
        (   dynamic_db(Mod)
        ->  true
        ;   compile_predicates([Mod:Pred])).

:- mode materialize_view(+) is det.
materialize_view(Mod:Pred/Arity):-
        !,
        materialize_view(Mod,Pred/Arity).
materialize_view(ModPred):-
        concat_atom([Mod,Pred],':',ModPred),
        concat_atom([PredName,Arity],'/',Pred),
        atom_number(Arity,ArityAsNum),
        materialize_view(Mod,PredName/ArityAsNum).

remove_duplicates(Mod):-
        forall(datapred(Mod,Pred),
               remove_duplicates(Mod,Pred)).
        
remove_duplicates(Mod,Pred):-
        pred_to_unground_term(Pred,PredTerm),
        (   setof(Mod:PredTerm,PredTerm,AllTerms)
        ->  abolish(Mod:Pred),
            maplist(assert,AllTerms)
        ;   true).

%% fact_clausesource(+Fact,?File)
% true if Fact was defined in File
fact_clausesource(Fact,Source):-
        clause(Fact,_,ClauseID),
        clause_property(ClauseID,file(File)),
        concat_atom(Parts1,':',File),
        reverse(Parts1,[Base1|_]),
        concat_atom(Parts,'/',Base1),
        reverse(Parts,[Base|_]),
        concat_atom([Source|_],'.',Base).



%% merge_facts(+Pred,+EquivGoal)
%  @param Pred
%  fact predicate, in the form Module:PredName
%  @param EquivGoal
%  a predicate of arity 2
%   merges facts in database based on an equivalence predicate which tests if two distinct primary key values should be merged
%  
:- mode merge_facts(+,+) is det.
merge_facts(MP,EquivGoal):-
        (   EquivGoal = _:EquivGoalLocal % may have module prefix
        ->  EquivGoalLocal =.. [_,X,Y]
        ;   EquivGoal =.. [_,X,Y]),
                                % find all quivalent ID-pairs
        solutions(X-Y,(EquivGoal,X \= Y),Pairs), % symmetric, non-reflexive
        solutions(X-[X|L],
                  (   member(X-_,Pairs),
                      solutions(Y,member(X-Y,Pairs),L1),
                      sort(L1,L)),
                  ESets),       % equivalence sets: [ID-EquivIDs,...]
        forall(member(X-[X|L],ESets), % don't include self
               dbmeta:merge_facts_in_set(MP,X,L)).

:- mode merge_facts_in_set(+,+,+) is det.
merge_facts_in_set(MP,ID,StaleIDs):-
        forall(member(StaleID,StaleIDs),
               merge_ids(MP,ID,StaleID)).

:- mode merge_ids(+,+,+) is det.
merge_ids(MP,ID,StaleID):-
        MP=Mod:PredName,
        fact_by_pk(Mod:PredName,StaleID,Fact),
        delete_fact(Mod,Fact),  % assume only one fact with pk
        forall(foreign_key_for(Mod:PredName,Col,FMod:FPredName),
               forall(lookup_fact(FMod:FPredName,Col=StaleID,FFact),
                      (   %delete_fact(FMod,FFact),
                          rewrite_fact(FMod,FFact,Col=ID,FFact2),
                          insert_fact(FMod:FFact2)))).

delete_fact(Mod,Fact):- retractall(Mod:Fact).

delete_all_facts(Mod):-
        db_facts(Mod,Facts),
        maplist(delete_fact(Mod),Facts).

assert_if_unique(Fact):-
        Fact,
        !.
assert_if_unique(Fact):-
        assertz(Fact).

insert_facts(Facts):-
        insert_facts(Facts,[]).

%% insert_facts(+Facts,+Opts)
% adds facts to the prolog database. Note that this doesn't require dynamic/1 as the facts are written to a temp file and then compiled
insert_facts(Facts,Opts):-
        tmp_file(factfile,TmpFactFile),
        open(TmpFactFile,write,IO,[]),
        forall(member(Fact,Facts),
               write_fact(Fact,IO,Opts)),
        close(IO),
        load_files([TmpFactFile],[qcompile(true)]).
% delete file now??
        
write_fact(Fact,IO,_Opts):-
        (   fact_persistence_file(Fact,FactFile)
        ->  insert_fact_to_factfile(Fact,FactFile)
        ;   true),
        (   Fact
        ->  true                % don't write to tempfile if already in db
        ;   format(IO,'~q.~n',Fact)).

insert_fact(Fact):-
        persistent_insert_fact(Fact),
        assert_if_unique(Fact).
insert_fact(Mod,Fact):- assertz(Mod:Fact). % ??


%% insert_fact_with_source(+Fact,+Source)
% as insert_fact/1, in addition records the source (eg filename)
insert_fact_with_source(Fact,Source):-
        insert_fact(Fact),
        assert(fact_source(Fact,Source)).

fact_persistence_file(Mod:Fact,FactFile):-
        functor(Fact,P,A),
        user:persistent(Mod,P/A,FactFile).

persistent_insert_fact(Mod:Fact):-
        persistent_insert_fact(Mod,Fact).

%% persistent_insert_fact(+Mod,+Fact) is det
persistent_insert_fact(Mod,Fact):-
        functor(Fact,P,A),
        user:persistent(Mod,P/A,FactFile),
        !,
        load_files([FactFile]) if exists_file(FactFile), % may have changed
        (   Mod:Fact
        ->  true                % already have it
        ;   open(FactFile,append,WriteIO,[]),
            format(WriteIO,'~w:~q.~n',[Mod,Fact]),
            close(WriteIO)).
persistent_insert_fact(_,_).

% TODO: fix code duplication
append_fact_to_factfile(Mod:Fact,FactFile):-
        load_files([FactFile]) if exists_file(FactFile), % may have changed
        (   Mod:Fact
        ->  true                % already have it
        ;   open(FactFile,append,WriteIO,[]),
            format(WriteIO,'~w:~q.~n',[Mod,Fact]),
            close(WriteIO)).

test_persistent(Mod:Fact):-
        test_persistent(Mod,Fact).
test_persistent(Fact):-
        context_module(Mod),
        test_persistent(Mod,Fact).
test_persistent(Mod,Fact):-
        functor(Fact,P,A),
        user:persistent(Mod,P/A,FactFile),
        !,
        load_files([FactFile]) if exists_file(FactFile).
test_persistent(_,_).

%rewrite_fact(Mod,Fact,Col=Val,NewFact):-
%        delete_fact(Mod,Fact),
%        true.
        %colspec(Mod:Pred,ColSpec),
        %predspec_name_colspecs(PredSpec,PN,ColSpecs).

% eg lookup_or_insert(genotype(G,foo),G)
lookup_or_insert(_,Fact,_):-
        test_persistent(Fact),
        Fact,
        !.
lookup_or_insert(NS,Fact,Key):-
        Fact=Mod:Fact1,
        functor(Fact1,P,A),
        functor(UngroundFact1,P,A),
        arg(1,UngroundFact1,Key),
        repeat,
        gensym(NS,Key),
        (   Mod:UngroundFact1
        ->  fail
        ;   !),
        insert_fact(Fact).

:- mode lookup_fact(+,+,?) is nondet.
lookup_fact(Mod:PredName,Col=Val,Fact):-
        colspec(Mod:PredName,ColSpec,N,Arity),
        ColSpec =.. [_DataType,Col|_],
        functor(Fact,PredName,Arity),
        arg(N,Fact,Val),
        Mod:Fact.

:- mode fact_by_pk(+,+,?) is det.
fact_by_pk(MP,PKVal,Fact):-
        primary_key(MP,PK),
        lookup_fact(MP,PK=PKVal,Fact).

:- mode primary_key(?,?) is nondet.
:- mode primary_key(+,?) is semidet.
primary_key(MP,PK):-
        colspec(MP,ColSpec),
        ColSpec =.. [pk,PK|_].

:- mode foreign_key_for(+,?,?) is nondet.
foreign_key_for(Mod:PredName,FK,FMP):-
        colspec(FMP,ColSpec),
        ColSpec =.. [fk,FK,ToPred],
        (ToPred=Mod:PredName
        ;ToPred=PredName).

% (+,?) nd
colspec(MP,ColSpec):-
        colspec(MP,ColSpec,_,_).
colspec(Mod:PN,ColSpec,N,Arity):-
        datapred_spec(Mod,PredSpec),
        predspec_name_colspecs(PredSpec,PN,ColSpecs),
        length(ColSpecs,Arity),
        nth1(N,ColSpecs,ColSpec).

predspec_name_colspecs(PredSpec-_Comments,PN,ColSpecs):-
        !,
        predspec_name_colspecs(PredSpec,PN,ColSpecs).
predspec_name_colspecs(PredSpec,PN,ColSpecs):-
        PredSpec =.. [PN|ColSpecs].

write_db_facts(F):-
        write_db_facts(pro,F).
write_db_facts(Fmt,F):-
        (var(F)->true; tell(F)),
        (setof(Mod,datamodule(Mod),Mods)
        ->  true
        ;   Mods=[]),
        forall(member(Mod,Mods),
               write_db_facts(Fmt,F,Mod)),
        (var(F)->true; told).
        

write_db_facts(Fmt,F,Mod):-
        debug(dbmeta,'writing pro facts in mod ~w format ~w to file ~w~n',[Mod,Fmt,F]),
        (   var(F)              % unground means stdout
        ->  true                % do nothing - stdout by default
        ;   F=append(F1)        % append(F) means write to end of F
        ->  (   var(F1)         % append(_) means append to stdout
            ->  true            % appending to stdout by default
            ;   append(F1))     % like tell/1 but end of file
        ;   tell(F)),           % F is ground AND not = append(_)
        forall(datapred(Mod,Pred),
               write_db_facts(Fmt,F,Mod,Pred)).
write_db_facts(Fmt,_F,Mod,Pred):-
        debug(dbmeta,'pred: ~w ~w ~w~n',[Fmt,Mod,Pred]),
        pred_to_unground_term(Pred,PredTerm),
        findall(PredTerm,(Mod:PredTerm,
                          write_db_fact(Fmt,Mod:PredTerm)),
                _),
        debug(dbmeta,' DONE pred: ~w ~w ~w~n',[Fmt,Mod,Pred]).

write_db_fact(T):-
        write_db_fact(pro,T).

write_db_fact(pro,T):-
        writeq(T),
        write('.'),
        nl.
write_db_fact(tbl,T):-
        T =.. L,
        writecols(L),
        nl.

%% write_db_fact_chain(+Fmt,+F,+Fact)
% 
% Fact is a term corresponding to a term in a database.
% this is the starting point to write out all dependent facts recursively.
% The database module should defined the fact_chain/2 hook.
write_db_fact_chain(Fmt,F,Fact) :-
        debug(dbmeta,'chaining facts from ~w~n',[Fact]),
        (   var(F)              % unground means stdout
        ->  true                % do nothing - stdout by default
        ;   F=append(F1)        % append(F) means write to end of F
        ->  (   var(F1)         % append(_) means append to stdout
            ->  true            % appending to stdout by default
            ;   append(F1))     % like tell/1 but end of file
        ;   tell(F)),           % F is ground AND not = append(_)
	write_db_fact_chain(Fmt,Fact).

%% write_db_fact_chain(+Fmt,+Fact)
% as write_db_fact_chain/3
write_db_fact_chain(Fmt,Fact) :-
	write_db_fact(Fmt,Fact),
	forall((fact_chain(Fact,ChainedFacts),
		member(ChainedFact,ChainedFacts),
	       ChainedFact),
	       write_db_fact_chain(Fmt,ChainedFact)).

fact_chain(_:F,C) :- fact_chain(F,C).
fact_chain(F,C) :- fact_chain_hook(F,C).


%% db_fact(?Mod,?PredTerm)
%  nd
%  
db_fact(Mod,PredTerm):-
        datapred(Mod,Pred),
        pred_to_unground_term(Pred,PredTerm),
        Mod:PredTerm.

%% db_facts(?Mod,?Ts) is nondet
% db_facts(+Mod,?Ts) is det
db_facts(Mod,Ts):-
        datamod(Mod),
        findall(T,db_fact(Mod,T),Ts).

%% write_dbsubset_facts(+File,+ID)
% as write_dbsubset_facts/3, for all modules
write_dbsubset_facts(F,ID):-
        (var(F)->true; tell(F)),
        (   setof(Mod,datamodule(Mod),Mods)
        ->  true
        ;   Mods=[]),
        forall(member(Mod,Mods),
               write_dbsubset_facts(F,ID,Mod)),
        (var(F)->true; told).
        

%% write_dbsubset_facts(+File,+ID)
% write the subset of prolog facts that reference ID in Mod
% (ID mist be first argument).
% see also write_db_facts_chain.
write_dbsubset_facts(F,ID,Mod):-
        forall(datapred(Mod,Pred),
               write_dbsubset_facts(F,ID,Mod,Pred)).
write_dbsubset_facts(_F,ID,Mod,Pred):-
        pred_to_unground_term(Pred,PredTerm),
        % assumes ID is first argument in datapred
        PredTerm =.. [_,ID|_],
        findall(PredTerm,(Mod:PredTerm,
                          writeq(Mod:PredTerm),
                          write('.'),
                          nl),_).


%% write_db_summary(?File)
%  writes all facts in all db modules in database
%
%  NOTE: this module should not really do any I/O. This should be moved
%to the io module
%  @param File
%  if unground will write to stdout
write_db_summary:-
        write_db_summary(_).
write_db_summary(F):-
        (var(F)->true; tell(F)),
        (setof(Mod,datamodule(Mod),Mods)
        ->  true
        ;   Mods=[]),
        forall(member(Mod,Mods),
               write_db_summary(F,Mod)),
        (var(F)->true; told).
        
%% write_db_summary(?File,?DbModule)
%  @param File
%  if unground will write to stdout
%
%   writes all facts in database from DbModule
%
%  ==
%  :- use_module(bio(io)).
%  :- use_module(bio(dbmeta),[write_db_summary/0]).
%
%  demo:-
%    load_bioresource(go),
%    write_db_summary.
%  ==
%  
%  
write_db_summary(F,Mod):-
        forall(datapred(Mod,Pred),
               write_db_summary(F,Mod,Pred)).
write_db_summary(_F,Mod,Pred):-
        pred_to_unground_term(Pred,PredTerm),
        findall(PredTerm,Mod:PredTerm,PredTerms),
        length(PredTerms,NumPredTerms),
        writeq(count(Pred,NumPredTerms)),
        write('.'),
        nl.

% DB style predicates

%% leftjoin(X,Y) is nondet.
%  relational left join between X and Y
%  i.e. same as X,Y except that if Y cannot be satisfied it still succeeds
%  with all variable arguments unified with null(_)
%
% can be written using infix notation: G1 leftjoin G2
X leftjoin Y:-
        X,
        findall(Y,Y,Ys),
        (   Ys=[]
        ->  Y=..[N|As],
            findall(A,(member(A,As),(var(A)->A=null(_);true)),As2),
            Y=..[N|As2]
        ;   member(Y,Ys)).
% TODO - do as macro


:- multifile
	system:term_expansion/2.
:- dynamic
	system:term_expansion/2.

system:term_expansion((:- pred(Pred)),
                    [(:- discontiguous(pred_metadata/2)),
                     pred_metadata(Pred,[])]).
system:term_expansion((:- pure(Pred)),
               [dbmeta:pure_pred(Pred)]).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(materialize,
            [],
            (   ensure_loaded(bio(ontol_db)),
                load_bioresource(cell),
                materialize_view(ontol_db,subclassT/2),
                class(ID,'B cell'),
                time(findall(PID,subclassT(ID,PID),_))),
            true)).

unittest(load(go)=
      load_bioresource(go)/[]).


unittest(test(class_query,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                setof(ID,
                      N^pred_query(ontol_db:class(ID,N),N,contains(trans)),
                      IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(pred_query_setof,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                pred_query_setof(ontol_db:class(ID,N),
                                 N,
                                 contains(trans),
                                 ID,
                                 IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(pred_query_bagof,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                pred_query_bagof(ontol_db:class(ID,N),
                                 N,
                                 contains(trans),
                                 ID,
                                 IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(pred_query_findall,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                pred_query_findall(ontol_db:class(ID,N),
                                 N,
                                 contains(trans),
                                 ID,
                                 IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(case_insensitive,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                setof(ID,
                      N^pred_query(ontol_db:class(ID,N),N,contains(nadh,i)),
                      IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).


/** <module> metadata on extensional prolog predicates

  ---+ Synopsis

  ==
  :- use_module(bio(taxon_db)).
  :- use_module(bio(dbmeta)).

  demo:-
    format('The taxon module defines the following data predicates~n'),
    forall(datapred(taxon_db,P),
           format(' Data predicate: ~w~n',[P])).
  
  ==

  ---+ Description
  
  bioprolog is in part a database querying system. it contains several
  'subschemas' arranged into modules; they include:

  
  * ontol_db
  * sb_db
  * seqfeature_db
  * taxon_db
  
each subschema module consists of both data predicates (ie prolog
goals with ground arguments in the head no body) and conventional
predicates. instances of the data predicates come from outside the
system as data files (either prolog fact files, or some other format
that can be parsed by the io module).

this module allows other modules to declare what their data predicates are.

TODO - SQL DDL syntax option


*/

%TODO - make compatible with new pldoc. See doc_process
%?- doc_comment(A,B,C,D),A=..L.
%Correct to: pldoc_process:doc_comment(A, B, C, D)? yes
