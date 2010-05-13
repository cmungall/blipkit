:- use_module(gene_db).
:- use_module(bio(curation_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(seqfeature_db)).

%:- index(metadata_db:entity_label(1,1)).

x(G,N,PMID,Rif) :-
        gene_rif(_,G,PMID,_,Rif),entity_label(G,N).


gene_with_rif_and_go(G) :-
        inst(G),
        entity_label(G,N),
        entity_label(G2,N), % link by label - go annotations to uniprot
        G2\=G,
        seqfeature_db:feature(G2),
        debug(foo,'checking ~w ~w',[G,G2]),
        \+ \+ ((curation_statement(A,G2,_,_),
                curation_source(A,PMID),
                gene_rif(_,G,_,PMID,_)
               )).

rif_report(G) :-
        entity_label(G,N),
        format('Gene: ~w ~w~n',[G,N]),
        forall((entity_label(G2,N),
                curation_statement(_,G2,_,X),
                entity_label(X,XN)),
               format('  GO: ~w "~w"~n',[X,XN])),
        forall(gene_rif(_,G,_,_,Rif),
               format('  RIF: "~w"~n',[Rif])).

% blip-findall -r go -r go_assoc_local/goa_human -r generif -r gene/9606 -u adhoc_gene "gene_pmid_go_rif(G,PMID,X,Rif)" -label
gene_pmid_go_rif(G,PMID,X,Rif) :-
        call_unique(curation_statement(A,G2,_,X)),
        curation_source(A,PMID),
        gene_rif(_,G,PMID,_,Rif),
        entity_label(G,N),
        entity_label(G2,N). % use symbol to match between ncbi and uniprot

gene_pmid_go_rif_rpt(G) :-
        gene_with_rif_and_go(G),
        entity_label(G,N),
        format('Gene: ~w ~w~n',[G,N]),
        solutions(PMID,gene_rif(_,G,PMID,_,_),PMIDs),
        member(PMID,PMIDs),
        format('  PMID: ~w~n',[PMID]),
        forall((entity_label(G2,N),
                curation_statement(A,G2,_,X),
                curation_source(A,PMID),
                entity_label(X,XN)),
               format('    GO: ~w "~w"~n',[X,XN])),
        forall(gene_rif(_,G,_,_,Rif),
               format('    RIF: "~w"~n',[Rif])).
        

