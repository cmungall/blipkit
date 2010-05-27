/* -*- Mode: Prolog -*- */


:- module(blipkit_phenotype,[]).

:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).
:- use_module(bio(phenotype_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(io)).
:- use_module(bio(tabling),[table_pred/1]).

% TODO: move
:- use_module(library('thea2/owl2_basic_reasoner')).
:- table_pred(owl2_basic_reasoner:entailed/1).

:- blip('phenotype-all-by-all',
        'all-by-all search',
        [atom(to,Fmt,txt),
	 atom(method,Method,postcomposed)],
        _,
        phenotype_all_by_all(Method,Fmt)).

:- blip('phenotype-search',
        'all-by-1 search',
        [	 atom(method,Method,postcomposed)],
        IDs,
        phenotype_search(Method,IDs)).

:- blip('phenotype-compare',
        '1-by-1 ',
        [atom(method,Method,postcomposed)],
        [F1,F2],
        phenotype_comparison_report(Method,txt,F1,F2)).

phenotype_search(Method,IDs):-
        forall(member(F1,IDs),
               forall(feature_with_phenotype(F2),
                      phenotype_comparison_report(Method,txt,F1,F2))).

phenotype_all_by_all(Method,Fmt):-
        forall(feature_with_phenotype(F1),
               forall((feature_with_phenotype(F2),F1\=F2),
                      phenotype_comparison_report(Method,Fmt,F1,F2))).

phenotype_comparison_report(Method,Fmt,F1,F2) :-
        (   calculate_method_feature_pair_phenosim(Method,F1,F2,Results) % TODO
        ->  phenotype_report(Method,Fmt,F1,F2,Results)
        ;   format(user_error,'failed ~w vs ~w~n',[F1,F2])).

phenotype_report(Method,pro,F1,F2,Results) :-
        format('~q.~n',feature_pair_simnr(Method,F1,F2,Results)).

phenotype_report(_,_,F1,F2,Results) :-
	%Results = [P1Xs,P2Xs,MaxIC,ICCS], % TODO
	member(max_ic(MaxIC),Results),
	member(avg_ic_bestmatches(ICCS),Results),
	member(bestmatches(P1Xs,P2Xs),Results),
        format('Query: '),
        show(F1),
        nl,
        format('Subject: '),
        show(F2),
        nl,
        format('  MaxIC: ~w~n',[MaxIC]),
        format('  ICCS: ~w~n',[ICCS]),
        nl,
        append(P1Xs,P2Xs,PXs),
        setof(IC-P,P1^P2^member(P1-P2-P-IC,PXs),ICPsRev),
        reverse(ICPsRev,ICPs),
        forall(member(ICP,ICPs),
               show_subs(ICP,P1Xs,P2Xs)),
        nl,
        writeln('=================').


show_subs(IC-P,P1Xs,P2Xs) :-
        write('  SHARED: '),
        show_p(P),
        nl,
        format('  IC = ~w~n',[IC]),
        solutions(P1,
                  (   member(P1-_-P-_,P1Xs)
                  ;   member(_-P1-P-_,P2Xs)),
                  P1s),
        solutions(P2,
                  (   member(P2-_-P-_,P2Xs)
                  ;   member(_-P2-P-_,P1Xs)),
                  P2s),
        writeln('    * Query feature phenotypes:'),
        forall(member(P1,P1s),
               (   write('      '),show_p(P1),nl)),
        writeln('    * Subject feature phenotypes:'),
        forall(member(P2,P2s),
               (   write('      '),show_p(P2),nl)),
        nl.


show(X) :-
        write(X),
        (   entity_label(X,N)
        ->  format(' "~w"',[N])
        ;   true).

show_p((E,Q,D,W)) :-
        write('(( '),
        show(E),
        write(' '),
        show(Q),
        write(' '),
        show(D),
        write(' '),
        show(W),
        write(')) ').
        
        

