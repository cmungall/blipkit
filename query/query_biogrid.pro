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

meta(interaction('INTERACTOR_A', 'INTERACTOR_B', 'OFFICIAL_SYMBOL_A', 'OFFICIAL_SYMBOL_B', 'ALIASES_FOR_A', 'ALIASES_FOR_B', 'EXPERIMENTAL_SYSTEM', 'SOURCE', 'PUBMED_ID', 'ORGANISM_A_ID', 'ORGANISM_B_ID')).

meta(process_function_prob(process,function,genesInBoth,genesWithFunc,genesWithProc,probProcessGivenFunction,probFunctionGivenProcess)).

interspecies_interaction(G1,G2,Tax1,Tax2) :-
        interaction(_,_,G1,G2,_,_,_,_,_,Tax1,Tax2),
        Tax1\=Tax2.

interaction_ccspan(G1,G2,Cs1,Cs2,EvL):-
        rdb_connect(Dbh,go_latest),
        %rdb_connect(Dbh,go),
        %Tax=4896,
        %Tax=9606,
        Tax=4932, % yeast
        solutions(G1-G2,interaction(_,_,G1,G2,_,_,_,_,_,Tax,Tax),GPairs),
        debug(biogrid,'found all pairs',[]),
        member(G1-G2,GPairs),
        %count_by(G1-G2,Exp-S-Pub,interaction(_,_,G1,G2,_,_,Exp,S,Pub,Tax,Tax),GPairCounts),
        %member(G1-G2-Num,GPairCounts),
        %Num>1,
        findall(Exp-S-Pub,interaction(_,_,G1,G2,_,_,Exp,S,Pub,Tax,Tax),EvL),
        length(EvL,Num),
        %debug(biogrid,'~w-~w : ~w',[G1,G2,EvL]),
        Num>1,
        rdb_solutions(Dbh,C,gsym2cc(G1,C,Tax),Cs1),
        rdb_solutions(Dbh,C,gsym2cc(G2,C,Tax),Cs2),
        Cs1\=[],
        Cs2\=[],
        \+ ( (member(C1,Cs1),
              member(C2,Cs2),
              proximal(C1,C2))).

% given symbol, find location
gsym2cc(GS,C,Tax):-
        feature_label(G,GS),
        feature_organism(G,Tax),
        curation_statement(_,G,_,C),
        belongs(C,cellular_component).

proximal(C1,C2):-
        parentRT(C1,C2).
proximal(C1,C2):-
        parentRT(C2,C1).


qprocess(P):- entity_label(P,'photosynthesis, light reaction').
qprocess(P):- entity_label(PP,'photosynthetic electron transport chain'),parentRT(P,PP).

process_function(Dbh,P,F):-
        qprocess(P),
        debug(pf,'trying for ~w',[P]),
        process_function_q(Dbh,P,F).
process_function_q(Dbh,P,F):-
        rdb_query(Dbh,
                  P-F,
                  process_function(P,F)).

        
process_function(P,F):-
        curation_statement(_,G,_,P),
        curation_statement(_,G,_,FI),
        belongs(FI,molecular_function),
        parentT(FI,F).

% :- sql_tie(process_function/2,go).

