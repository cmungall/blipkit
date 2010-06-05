/* -*- Mode: Prolog -*- */


:- module(seqfeature_sqlmap_go,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(seqfeature_db),[]).

:- load_schema_defs(bio('sql_schema/schema_go')).

% TODO: ensure compatibility
:- [bio(ontol_sqlmap_go_util)].

:- multifile
	system:term_expansion/2.

dbxref_iid_id(DBXrefID,X) <-
  dbxrefd(DBXrefID,_,_,_,_,X). 

feature0(InternalID,ID,Name) <-
  product0(InternalID,ID,Name). % ensure compat with product0, which uses dbxref0
  %feature0(InternalID,ID,Name,_TypeID,_SpeciesID). % ensure compat with product0, which uses dbxref0

feature0(InternalID,ID,Name,TypeID,SpeciesID) <-
  gene_product(InternalID,Name,DBXrefID,SpeciesID,_,TypeID,_Full),dbxref0(DBXrefID,ID).
 %dbxref_iid_id(DBXrefID,ID).

% PROBLEM! TODO! feature/N has meaning in both sql schema and prolog schema. former takes priority??
%:- abolish(seqfeature_db:feature/3).
%=seqfeature_db:feature(F,N,T) <- entity_label(F,N),feature_type(F,T).  % TODO leftjoin??

seqfeature_db:feature(ID) <- feature0(_InternalID,ID,_N).
seqfeature_db:feature_label(ID,N) <- feature0(_InternalID,ID,N).
%seqfeature_db:feature_dbxref(ID,X) <- feature0(InternalID,ID,_N),gene_product_seq(InternalID,SeqInternalID,_),seq_dbxref(SeqInternalID,XIID),dbxref0(XIID,X).
seqfeature_db:feature_dbxref(ID,DB,Acc) <- feature0(InternalID,ID,_N),gene_product_seq(InternalID,SeqInternalID,_),seq_dbxref(SeqInternalID,XIID),dbxref(XIID,Acc,_,DB,_).
seqfeature_db:feature_dbxref(ID,X) <- feature0(InternalID,ID,_N),gene_product_seq(InternalID,SeqInternalID,_),seq(SeqInternalID,X).
seqfeature_db:feature_db(ID,DB) <- feature0(InternalID,ID,_N),gene_product_seq(InternalID,SeqInternalID,_),seq_dbxref(SeqInternalID,XIID),dbxref(XIID,_,_,DB,_).
seqfeature_db:feature_organism(ID,TaxNo) <- feature0(_InternalID,ID,_Name,_TypeID,SpeciesID),species(SpeciesID,TaxNo,_,_,_,_).

%metadata_db:entity_label(ID,N) <- feature0(_InternalID,ID,N).


seqfeature_db:feature_type(ID,T) <-
  feature0(_InternalID,ID,_,TypeIID,_),
  term0(TypeIID,T,_,_).


:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

%unittest:testq(basic,foo,feature('FB:FBgn0000490')).
%unittest:testq(basic,dpp(G,T),(seqfeature_db:feature(G,dpp,T))).
unittest:testq(basic,dpp(G),(seqfeature_db:feature_label(G,dpp))).
unittest:testq(basic,dpp_type(G,T),(seqfeature_db:feature_label(G,dpp),feature_type(G,T))).
unittest:testq(basic,num_f(C),(C is count(X,feature(X)))).
unittest:testq(basic,num_pr(C),(C is count(X,feature_type(X,protein)))).
%unittest:testq(ontol,dpp_assoc(C),(curation_statement(_,'FB:FBgn0000490',_,C))). %%%%% '
%unittest:testq(ontol,dpp_assoc(C),(seqfeature_db:feature(G,dpp,_),curation_statement(_,G,_,C))).
unittest:testq(ontol,dpp_assoc(C,N),(seqfeature_db:feature_label(G,dpp),curation_statement(_,G,_,C),ontol_db:class(C,N))).
unittest:testq(bridge,dpp_class(C),(ontol_db:class(C,dpp))).
unittest:testq(homol,ghomol(G,H),(feature_label(G,'TAZ'),homologset_member(H,G))).
unittest:testq(homol,homolsib(H,G,GN),(feature_label(G1,'TAZ'),homologset_member(H,G1),homologset_member(H,G),feature_label(G,GN))).
unittest:testq(homol,homolsib2(G,GN),(feature_label(G1,'TAZ'),reflexive_homologous_to(G1,G),feature_label(G,GN))).
unittest:testq(homol,homolsib3(G,GN),(feature_label(G1,'TAZ'),interspecies_reflexive_homologous_to(G1,G),feature_label(G,GN))).
unittest:testq(homol,homolsib4(G,S,GN),(feature_label(G1,'TAZ'),reflexive_homologous_to(G1,G),feature_organism(G,S),feature_label(G,GN))).
unittest:testq(homol,homol_annot(G,H,C),(feature_label(G,'TAZ'),homologset_member(H,G),curation_statement(_,G,_,C))).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                ensure_loaded(bio(ontol_db)),
                rdb_connect(Rdb,go),
                forall(testq(basic,Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           nl)),
                writeln(done)),
            true)).

unittest(test(ontol,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_sqlmap_go)),
                rdb_connect(Rdb,go),
                forall(testq(ontol,Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           nl)),
                writeln(done)),
            true)).

unittest(test(compare_homolset,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_compare)),
                ensure_loaded(bio(ontol_writer)),
                ensure_loaded(bio(ontol_writer_text)),
                ensure_loaded(bio(ontol_sqlmap_go)),
                ensure_loaded(bio(homol_sqlmap_go)),
                ensure_loaded(bio(homol_db)),
                ensure_loaded(bio(seqfeature_sqlmap_go)),
                ensure_loaded(bio(seqfeature_bridge_to_class)),
                rdb_connect(Rdb,go),
                load_bioresource(go),
                solutions(G-GN-C,
                          rdb_query(Rdb,G-GN-C,(feature_label(G1,'TAZ'),interspecies_reflexive_homologous_to(G1,G),feature_label(G,GN),curation_statement(_,G,_,C))),
                          GeneClassPairs),
                solutions(G-N,member(G-N-_,GeneClassPairs),Genes),
                forall((member(G1-G1N,Genes),member(G2-G2N,Genes),G1\=G2),
                       (   
                           format('~w [~w] vs ~w [~w]~n',[G1N,G1,G2N,G2]),
                           solutions(C,member(G1-_-C,GeneClassPairs),Classes1),
                           solutions(C,member(G2-_-C,GeneClassPairs),Classes2),
                           class_set_difference(Classes1,Classes2,Uniq1,Uniq2),
			   writeln('** Uniq1'),
			   maplist(write_class(textnl),Uniq1),
			   writeln('** Uniq2'),
			   maplist(write_class(textnl),Uniq2),
			   writeln('** Intersection'),
                           class_set_intersection(Classes1,Classes2,ClassesBoth),
			   maplist(write_class(textnl),ClassesBoth),
			   nl)),
		writeln(done)),
            true)).

unittest(test(bridge,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_sqlmap_go)),
                rdb_connect(Rdb,go),
                
                forall(testq(basic,Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           nl)),
                writeln(done)),
            true)).

unittest(test(P,
             [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_sqlmap_go)),
                ensure_loaded(bio(homol_sqlmap_go)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(homol_db)),
                test_rdb_connect(Rdb,go),
                writeln(trying(Proj)),
                forall(catch(rdb_query(Rdb,Proj,Goal),E,writeln(E)),
                       writeln(Proj)),
                nl,
                writeln(done)),
            true)):-
        unittest:testq(_,Proj,Goal),
        functor(Proj,P,_).

/** <module> Maps seqfeature_db predicates to GO database tables

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_from_gosql)).
  :- seqfeature_bridge_from_gosql:set_bridge_resource(go_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Go Schema to masquerade as seqfeature
  predicates

  See <http://www.gmod.org/schema> Go

  Command line:
  
  ==
  blip-sql prolog-to-sql -u seqfeature_sqlmap_go "feature(X)" -proj "x(X)"
  ==

  @see go_database_tutorial.txt
  
  */
