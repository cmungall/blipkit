/* -*- Mode: Prolog -*- */

:- use_module(genome_sqlmap_enscore).
:- use_module(genome_db).
:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).

:- op(1100,fx,testq).
:- op(1000,xfy,where).

test_query(Proj,Goal) :-
        writeln(Proj where Goal),
        catch(plterm_to_sqlterm(Proj,Goal,SqlTerm),
              E,
              (   writeln(E),
                  fail)),
        writeln(SqlTerm),
        sqlterm2atom(SqlTerm,Sql),
        writeln(Sql),
        format('** SUCCESS: ~w~n',[Proj]),
        nl.

test_query_exec(Rdb,Proj,Goal) :-
        writeln(Proj where Goal),
        forall(rdb_query(Rdb,Proj,Goal),
               writeln(Proj)),
        format('** SUCCESS: ~w~n',[Proj]),
        nl.


%testq brca1_feature(G) where feature(G,'BRCA1',_).
testq brca1_gene(G) where gene_symbol(G,'BRCA1').
testq brca1_g2t(G,T) where gene_symbol(G,'BRCA1'),gene_transcript(G,T).
testq brca1_g2cds(G,CDS) where gene_symbol(G,'BRCA1'),gene_transcript(G,T),transcript_cds(T,CDS).
testq brca1_g2p(G,P) where gene_symbol(G,'BRCA1'),gene_polypeptide(G,P).
testq brca1_gtx(G,T,X,Rank) where gene_symbol(G,'BRCA1'),gene_transcript(G,T),genome_db:exon_transcript_order(X,T,Rank).
%testq brca1_fr(G,T,TN) where feature(G,'BRCA1',_),genome_db:feature_relationship(T,G),genome_db:feature(T,TN,_).
testq brca1_pos(G,Seq,Start,End,Strand) where gene_symbol(G,'BRCA1'),gene_dnaseq_pos(G,Seq,Start,End,Strand).
%testq brca1_rreg(R,RStart,REnd,Strand) where gene_symbol(G,'BRCA1'),gene_dnaseq_pos(G,Seq,Start,_End,Strand),regulatory_region_dnaseq_pos(R,Seq,RStart,REnd,Strand),RStart>Start,Dist is RStart - Start, Dist < 100000.
%testq brca1_trr(G,R) where gene_symbol(G,'BRCA1'),gene_transcript(G,T),regulates_transcript(R,T). % not used?
%testq brca1_rr(G,R) where gene_symbol(G,'BRCA1'),regulates_gene(R,G).
%testq brca1_rr_dist(G,R,Dist) where gene_symbol(G,'BRCA1'),regulates_gene(R,G),gene_dnaseq_pos(G,Seq,Start,_End,Strand),regulatory_region_dnaseq_pos(R,Seq,RStart,_REnd,Strand),Dist is RStart - Start.
testq exonpos(G,SeqN,Start,End,Strand) where gene_symbol(G,'BRCA1'),genome_db:exon_gene(X,G),exon_dnaseq_pos(X,SeqN,Start,End,Strand).
testq apoe_intron(G,T,X1,I,X2) where gene_symbol(G,'APOE'),gene_transcript(G,T),exon_intron_exon_transcript(X1,I,X2,T).

:- begin_tests(enscore, []).

test(nodb):-
        findall(Proj-Where,
                (   (   testq Proj where Where),
                    \+ test_query(Proj,Where)),
                Fails),
        (   Fails=[]
        ->  format('passed~n')
        ;   format('FAILED:~n'),
            maplist(writeln,Fails),
            fail).


:- end_tests(enscore).

:- begin_tests(withdb, []).

% Requires ODBC to be configured
test(withdb) :-
        writeln(connecting),
        rdb_connect(Rdb,enscore(homo_sapiens_core)),
        format('connected: h=~w~n',[Rdb]),
        findall(Proj-Where,
                (   (   testq Proj where Where),
                    \+ test_query_exec(Rdb,Proj,Where)),
                Fails),
        (   Fails=[]
        ->  format('passed~n')
        ;   format('FAILED:~n'),
            maplist(writeln,Fails),
            fail).


:- end_tests(withdb).

:- begin_tests(multidb, [setup(load)]).

load :-
        ensure_loaded(bio(sql_compiler)),
        ensure_loaded(bio(rdb_util)),
        ensure_loaded(bio(genome_db)),
        ensure_loaded(bio(ontol_db)),
        ensure_loaded(bio(curation_db)),
        ensure_loaded(bio(genome_sqlmap_enscore)),
        ensure_loaded(bio(seqfeature_sqlmap_go)),
        ensure_loaded(bio(ontol_sqlmap_go)).
        
test(multidb) :-
        rdb_connect(DbGO,rdb(go)),
        rdb_forall(DbGO,class(C,apoptosis),writeln(C)),
        rdb_forall(DbGO,G,(class(C,apoptosis),curation_statementT(Protein,_,C,_),feature_xref(Protein,'ENSEMBL',G)),
                   writeln(G)),
        rdb_connect(DbEnscoreHs,homo_sapiens_core),
        rdb_forall(DbGO,(class(C,apoptosis),curation_statementT(Protein,_,C,_),feature_xref(Protein,'ENSEMBL',G)),
                   rdb_forall(DbEnscoreHs,(gene_transcript(G,T)),
                              writeln(T))),
                                %DbGO:forall((class(C,apoptosis),curation_statementT(Protein,_,C,_),feature_xref(Protein,'ENSEMBL',G)),
                                %           DbEnscoreHs:forall((gene_transcript(G,T)),
                                %                              writeln(T))),
        writeln(done).

:- end_tests(multidb).






