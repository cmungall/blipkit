/* -*- Mode: Prolog -*- */

:- module(curation_exhibit_bridge,
          [
           allgenes_json/1
           ]).

:- use_module(bio(mode)).
:- use_module(bio(curation_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(library(http/json)).

io:redirect_stdout(gene_exhibit).
io:redirect_stdout(annotation_exhibit).
        
io:write_all(gene_exhibit,_,_):-
        allgenes_json(J),
        atom_json_term(A,J,[as(atom)]),
        writeln(A).

io:write_all(annotation_exhibit,_,_):-
        allanns_json(J),
        atom_json_term(A,J,[as(atom)]),
        writeln(A).

% GENES

gene_to_class(G,C) :- curation_statement(_,G,_,C).
gene_to_class(G,C) :-
        curation_statement(Ann,G,_,_),
        curation_subject_property_value(Ann,_,_,C).

gene_to_ont_class_label(G,Ont,CN) :-
        gene_to_class(G,C),entity_label(C,CN),belongs(C,Ont).
gene_to_ont_class_label_tr(G,Ont,CN) :-
        gene_to_class(G,C),parentRT(C,C2),
        entity_label(C2,CN),belongs(C2,Ont).


allgenes_json(JSON) :-
        setof(G,feature(G),Genes),
        geneset_json(Genes,JSON).

geneset_json(Genes,json([items=Items])) :-
        findall(Item,(member(Gene,Genes),gene_json(Gene,Item)),Items).

gene_json(G,json(TVs)) :-
        findall(T=V,gene_tv(G,T,V),TVs).

gene_tv(G,id,G).
gene_tv(G,label,N) :- entity_label(G,N).
gene_tv(G,type,N) :- feature_type(G,N).
gene_tv(G,taxon,N) :- feature_organism(G,N).
gene_tv(G,Ont,Vals) :- setof(CN,gene_to_ont_class_label(G,Ont,CN),Vals).
gene_tv(G,OntTr,Vals) :-
        setof(CN,gene_to_ont_class_label_tr(G,Ont,CN),Vals),
        atom_concat('transitive_',Ont,OntTr).


% ANNOTATIONS

ann_to_class(A,C) :- curation_statement(A,_,_,C).
ann_to_class(A,C) :- curation_subject_property_value(A,_,_,C).

ann_to_ont_class_label(A,Ont,CN) :-
        ann_to_class(A,C),entity_label(C,CN),belongs(C,Ont).
ann_to_ont_class_label_tr(A,Ont,CN) :-
        ann_to_class(A,C),parentRT(C,C2),
        \+ entity_partition(C2,upper_level),
        entity_label(C2,CN),belongs(C2,Ont).

allanns_json(JSON) :-
        setof(G,curation(G),Anns),
        annset_json(Anns,JSON).

annset_json(Anns,json([items=Items])) :-
        findall(Item,(member(Ann,Anns),ann_json(Ann,Item)),Items).

ann_json(A,json(TVs)) :-
        findall(T=V,ann_tv(A,T,V),TVs).

ann_tv(A,id,A).
ann_tv(A,label,A). % TODO
ann_tv(A,type,N) :- curation_publisher(A,N).
ann_tv(A,taxon,N) :- curation_statement(A,G,_,_),
        call_unique(feature_organism(G,T)),
        (   entity_label(T,N)
        ->  true
        ;   N=T).
ann_tv(A,gene_id,G) :- curation_statement(A,G,_,_).
ann_tv(A,gene,N) :- curation_statement(A,G,_,_),call_unique(entity_label(G,N)).
ann_tv(A,evidence_type,N) :- curation_evidence_code(A,N).
ann_tv(A,evidence_with,N) :- curation_evidence_with(A,N).
ann_tv(A,reference,N) :- curation_source(A,N).
ann_tv(A,Ont,Vals) :- setof(CN,ann_to_ont_class_label(A,Ont,CN),Vals).
ann_tv(A,OntTr,Vals) :-
        setof(CN,ann_to_ont_class_label_tr(A,Ont,CN),Vals),
        atom_concat('transitive_',Ont,OntTr).



             



