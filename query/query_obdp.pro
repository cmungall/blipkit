
%% blip -u query_obdp -i fm_closure.pro -i fm_allg_direct.pro -u tabling -table_pred simmatrix:attribute_information_content/2 -i uid_label_genes.pro -r obd/obdp -u ontol_sqlmap_obd -u ontol_db -u metadata_db -i omim_gene.pro -i fm_allg.pro -u simmatrix -goal "simmatrix:generate_term_indexes(F,A,fm(F,A))"

% blip -debug q -u query_obdp -goal "load_info,make_ix"


:- use_module(bio(simmatrix)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(tabling)).
:- use_module(bio(io)).

:- table_pred(simmatrix:attribute_information_content/2).

load_info :-
        load_biofile('fm_closure.pro'),
        load_biofile('fm_allg_direct.pro'),
        load_biofile('fm_allg.pro'),
        load_biofile('uid_label_genes.pro'),
        load_biofile('omim_gene.pro').

make_ix :-
        generate_term_indexes(F,A,fm(F,A)).


%query_gene('ZFIN:ZDB-GENE-980526-166'). DONE!!!
%query_gene(X) :- omim_gene(_,X,_).
%query_gene('NCBI_Gene:6663').
query_gene('NCBI_Gene:6663').

runall( OrderBy, Max) :-
        query_gene(X),
        debug(q,'getting matches for ~w',[X]),
        feature_matches(X,L,[metric(OrderBy)]),  % all matches, ordered
        length(L,Len),
        debug(q,'matches ~w num: ~w',[X,Len]),
        sformat(F,'~w-top-~w-hits-by-~w.txt',[X,Max,OrderBy]),
        tell(F),
        runall(X,L,[],Max),
        format(user_error,'Done: ~w',[X]),
        told,
        fail.
runall.

el(X,N):-
        (   gene(X,N)
        ->  true
        ;   N='?').

id_db(X,DB) :-
        concat_atom([DB|_],':',X).


runall(_,[],_,_Max).
runall(X,[_-Y|YL],DoneL,Max) :-
        id_db(Y,DB),
        findall(Done,(member(Done,DoneL),
                      id_db(Done,DB)),
                DoneInDB),
        length(DoneInDB,NumDoneInDB),
        debug(q,'~w vs ~w :: already done ~w in ~w',[X,Y,NumDoneInDB,DB]),
        (   NumDoneInDB < Max      % top00 in each species
        ->  el(X,XN),
            el(Y,YN),
            format('~w\t~w\t~w\t~w',[X,XN,Y,YN]),
            (   feature_pair_all_scores(X,Y,Scores),
                forall(member(Score,Scores),
                       (   Score=..STL,
                           forall(member(ST,STL),
                                  format('\t~w',[ST]))))
            ->  true
            ;   format(user_error,'PROBLEM: ~w ~w ~w ~w~n',[X,XN,Y,YN])),
            nl
        ;   debug(q,'omitting ~w',[Y])
        ),
        !,
        runall(X,YL,[Y|DoneL],Max).

runall(X,[_-Y|YL],DoneL) :-
        !,
        runall(X,YL,[Y|DoneL]).


load_genotype_info :-
        load_biofile('genotype_fm_closure.pro'),
        load_biofile('genotype_fm_direct.pro'),
        load_biofile('genotype_fm.pro'),
        load_biofile('omim_genotype_gene.pro').

runall_genotype :-
        tell('genotype-all-by-all.tab'),
        omim_genotype_gene(A,AN,G,GN),
        debug(q,'A: ~w ~w ~w ~w',[A,AN,G,GN]),
        omim_genotype_gene(A2,AN2,G2,GN2),
        (   feature_pair_all_scores(A,A2,Scores),
            forall(member(Score,[allele(A),
                                 name(AN),
                                 gene(G),
                                 gene_name(GN),
                                 allele2(A2),
                                 name2(AN2),
                                 gene2(G2),
                                 gene_name2(GN2)|Scores]),
                   (   Score=..STL,
                       forall(member(ST,STL),
                              format('\t~w',[ST]))))
        ->  nl
        ;   format(user_error,'PROBLEM: ~w ~w~n',[A,A2])),
        fail.

runall_genotype :- told.


%% blip -u query_obdp -u tabling -table_pred simmatrix:attribute_information_content/2 -u ontol_db -u metadata_db -u simmatrix -goal "simmatrix:generate_term_indexes(F,A,fm(F,A))"

% blip -debug q -u query_obdp -goal "load_genotype_info,make_ix,runall_genotype"
