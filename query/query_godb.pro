:- use_module(bio(ontol_sqlmap_go)).
:- use_module(bio(seqfeature_sqlmap_go)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(rdb_util)).
:- use_module(bio(bioprolog_util)).

/*

  todo - find functions on an individual per-protein basis, then find p-value for each
  
  */

meta(process_function_prob(process,function,genesInBoth,genesWithFunc,genesWithProc,probProcessGivenFunction,probFunctionGivenProcess)).
process_function_prob(P,F,CI,CF,CP,ProbPGivenF,ProbFGivenP):-
        %rdb_connect(Dbh,go_latest_lite),
        rdb_connect(Dbh,go),
        solutions(P-F,process_function(Dbh,P,F),PFs),
        member(P-F,PFs),
        F\=all,
        \+ ((member(P-F2,PFs),
             parentT(F2,F))),
        debug(pf,'Finding probs for: P:~w F:~w',[P,F]),
        rdb_query(Dbh,ProbPGivenF-CI-CF,class_conditional_prob(P,F,CI,CF,ProbPGivenF)),
        rdb_query(Dbh,ProbFGivenP-CP,class_conditional_prob(F,P,_,CP,ProbFGivenP)).

process_function(Dbh,P,F):-
        qprocess(P),
        debug(pf,'trying for ~w',[P]),
        process_function_q(Dbh,P,F).

process_function_q(Dbh,P,F):-
        rdb_query(Dbh,
                  P-F,
                  correlated_process_function(P,F)).

        
correlated_process_function(P,F):-
        curation_statement(_,G,_,P),
        curation_statement(_,G,_,FI),
        belongs(FI,molecular_function),
        parentT(FI,F).

% :- sql_tie(process_function/2,go).

