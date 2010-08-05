:- module(ontol_sqlmap_go,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
%:- use_module(bio(curation_db),[]). % separate module?
:- use_module(bio(curation_db)). % separate module?

:- load_schema_defs(bio('sql_schema/schema_go')).

% views
%  term0
%  product0
%  etc
:- [ontol_sqlmap_go_util].


% METADATA
metadata_db:entity_label(ID,N) <- term0(_,ID,N,_).
metadata_db:entity_resource(ID,NS) <- term0(_,ID,_,NS).
metadata_db:entity_synonym(ID,N) <- term0(IID,ID,_,_),term_synonym(IID,N,_,_).

is_class_or_property_or_inst(ID) <- term0(_,ID,_,_).

metadata_db:entity_source(ID,DB) <- dbxrefd(_IID,_Acc,_,DB,_,ID).


% ONTOL
ontol_db:class(ID) <- term0(_,ID,_,_).

%:- abolish(ontol_db:parent/2).
ontol_db:parent(X,Y) <- term0(XI,X,_,_),link_n(XI,_,YI),term0(YI,Y,_,_).

/*
%:- abolish(ontol_db:parent/3). % map to internal
ontol_db:parent(X,subclass,Y):- ontol_db:parent0(X,is_a,Y).
ontol_db:parent(X,R,Y):- ontol_db:parent0(X,R,Y),R\=is_a.
*/

%:- abolish(ontol_db:parentT/3). % TODO is_a/subclass
ontol_db:parentT(X,R,Y) <- term0(XI,X,_,_),link_implied(XI,RI,YI),term0(YI,Y,_,_),term0(RI,R,_,_).
%:- abolish(ontol_db:parentT/2). % TODO is_a/subclass
ontol_db:parentT(X,Y) <- term0(XI,X,_,_),link_implied(XI,_,YI),term0(YI,Y,_,_). % todo: enforce proper / non-reflexive?
ontol_db:parentRT(X,Y) <- term0(XI,X,_,_),link_implied(XI,_,YI),term0(YI,Y,_,_).
ontol_db:bf_parentRT(X,Y) <- term0(XI,X,_,_),link_implied(XI,_,YI),term0(YI,Y,_,_).

%:- abolish(ontol_db:subclass/2).
ontol_db:subclass(X,Y) <- parent0(X,is_a,Y).
ontol_db:restriction(X,R,Y) <- parent(X,R,Y),not(R=is_a).
%ontol_db:inst_of(X,Y) <- parent(X,'OBO_REL:instance_of',Y).
%:- abolish(ontol_db:subclassT/2).
ontol_db:subclassT(X,Y) <- parentT(X,is_a,Y).

%:- abolish(ontol_db:noparent/1).
ontol_db:noparent(X) <- class(X),not(parent(X,_)).

%:- abolish(ontol_db:genus/2).
ontol_db:genus(X,Y) <- intersection0(X,is_a,Y).

%:- abolish(ontol_db:differentium/3).
ontol_db:differentium(X,R,Y) <- intersection0(X,R,Y),not(R=is_a).

ontol_db:def(X,Label) <- term0(XI,X,_,_),term_definition(XI,Label,_,_,_).

%lookup_class(search(S,_),ID):-
%        foo.

% TODO: split this into curation package?
curation_db:curation(X) <- association(X,_,_,_,_,_,_).
curation_db:curation_statement(X,S,has_role,O) <- association(X,OI,SI,0,_,_,_),term0(OI,O,_,_),product0(SI,S,_).
%curation_db:curation_statement(X,S,_,O) <- association(X,OI,SI,0,_,_,_),term0(OI,O,_,_),product0(SI,S,_).
curation_db:negative_curation_statement(X,S,has_role,O) <- association(X,OI,SI,1,_,_,_),term0(OI,O,_,_),product0(SI,S,_).
curation_db:curation_evidence(C,E) <- evidence(E,_,C,_,_).
curation_db:evidence_type(E,T) <- evidence(E,T,_,_,_).
curation_db:evidence_with(E,X) <- evidence_dbxref(E,XI),dbxref0(XI,X).
curation_db:evidence_source(E,S) <- evidence(E,_,_,SI,_),dbxref0(SI,S).

% TODO: special optimization module?
curation_db:class_annotated_entity_count(C,AEC) <-
  AEC is sum(SC,(term0(CI,C,_,_),gene_product_count(CI,'!IEA',_,SI,SC),null(SI))). % other constraints must go inside expr

curation_db:class_infocontent(C,InfoContent) <-
  term0(CI,C,_,_),
  class_node_entropy(CI,InfoContent).


:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

% ----------------------------------------
% TESTS
% ----------------------------------------
unittest:testq(id(ID),(entity_label(ID,molecular_function))).
unittest:testq(bool1(ID),(entity_label(ID,apoptosis),not(belongs(ID,molecular_function)))).
unittest:testq(child(Y,XN),(entity_label(Y,apoptosis),parent(X,Y),entity_label(X,XN))).
unittest:testq(children_of_apoptosis(C),(C is count(X,(ontol_db:class(A,apoptosis),parentT(X,A))))).
unittest:testq(num_apoptosis_genes(C),(C is count(G,(class(A,apoptosis),curation_statementT(_,G,_,A))))).
unittest:testq(num_apoptosis_genes2(C),(C is count(G,(ontol_db:class(A,apoptosis),parentT(X,A),curation_statement(_,G,_,X))))).
unittest:testq(annot_intersection_direct(G),
             (   entity_label(T1,'olfactory receptor activity'),
                 entity_label(T2,'sensory perception of smell'),
                 curation_statement(_,G,_,T1),
                 curation_statement(_,G,_,T2))).
unittest:testq(annot_intersection(G,N),
             (   entity_label(T1,'olfactory receptor activity'),
                 entity_label(T2,'sensory perception of smell'),
                 %feature(G,N,_), %fails
                 %entity_label(G,_), %fails - causes gp dbxref to be rewritten as acc 
                 feature_label(G,N),
                 curation_statementT(_,G,_,T1,_),
                 curation_statementT(_,G,_,T2,_))).
unittest:testq(annot_intersection_count(T1,T2,C),
             (   entity_label(T1,'olfactory receptor activity'),
                 entity_label(T2,'sensory perception of smell'),
                 C is count_distinct(G,(
                                        curation_statementT(_,G,_,T1,_),
                                        curation_statementT(_,G,_,T2,_))))).
unittest:testq(annot_intersection2(G),
             (   entity_label(T1,'olfactory receptor activity'),
                 entity_label(T2,'sensory perception of smell'),
                 parentRT(T1A,_,T1),
                 parentRT(T2A,_,T2),
                 curation_db:curation_statement(_,G,_,T1A),
                 curation_db:curation_statement(_,G,_,T2A))).
unittest:testq(annot_intersection_negp(G,N),
             (   entity_label(T1,'olfactory receptor activity'),
                 entity_label(T2,'sensory perception of smell'),
                 %feature(G,N,_), %fails
                 %entity_label(G,_), %fails - causes gp dbxref to be rewritten as acc 
                 feature_label(G,N),
                 curation_statementT(_,G,_,T1,_),
                 not(curation_statementT(_,G,_,T2,_)))).
unittest:testq(annot_intersection_negf(G,N),
             (   entity_label(T1,'olfactory receptor activity'),
                 entity_label(T2,'sensory perception of smell'),
                 %feature(G,N,_), %fails
                 %entity_label(G,_), %fails - causes gp dbxref to be rewritten as acc 
                 feature_label(G,N),
                 %not((curation_statementT(_,G2,_,T1,_),G2=G)), % TODO: we shouldnt have to do this
                 not(curation_statementT(_,G,_,T1,_)),
                 curation_statementT(_,G,_,T2,_))).
unittest:testq(annot_prob(TF,TP,CI,CP,Prob),
             (   entity_label(TF,'olfactory receptor activity'),
                 entity_label(TP,'sensory perception of smell'),
                 CI is count_distinct(G,(
                                         curation_statementT(_,G,_,TP,_),
                                         curation_statementT(_,G,_,TF,_))),
                 CP is count_distinct(G,(
                                         curation_statementT(_,G,_,TP,_))),
                 Prob is CI/CP)).
                 

unittest:test_rdb_connect(H,DB):-
        catch(rdb_connect(H,DB),
              Err,
              (   format(' Cannot connect: ~w~nCarrying on for debugging purposes...~n~n',[Err]),
                  H=null(_))).


unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                unittest:ensure_loaded(bio(ontol_db)),
                unittest:ensure_loaded(bio(curation_db)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_sqlmap_go)),
                test_rdb_connect(Rdb,go),
                forall(testq(Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(catch(rdb_query(Rdb,Proj,Goal),E,writeln(E)),
                                  writeln(Proj)),
                           nl)),
                writeln(done)),
            true)).

unittest(test(P,
             [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                unittest:ensure_loaded(bio(ontol_db)),
                unittest:ensure_loaded(bio(curation_db)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_sqlmap_go)),
                test_rdb_connect(Rdb,go),
                writeln(trying(Proj)),
                %forall(catch(rdb_query(Rdb,Proj,Goal),E,writeln(E)),
                %       writeln(Proj)),
                rdb_forall(Rdb,Goal,
			   writeln(Proj)),
                nl,
                writeln(done)),
            true)):-
        unittest:testq(Proj,Goal),
        functor(Proj,P,_).



unittest(test(inline,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ontol_sqlmap_go:test_inline),
            true)).

unittest(test(rewrite,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                %ensure_loaded(bio(rdb_util)),
                %unittest:ensure_loaded(bio(ontol_db)),
                %unittest:ensure_loaded(bio(curation_db)),
                load_files(bio(ontol_db)),
                writeln(trying),
                clause(parentRT(_,_),Body),
                writeln(b=Body),
                rewrite_query(parentRT(_,_),Q),
                writeln(q=Q),
                Q \= parentRT(_,_)),
            true)).

test_inline:-
        ensure_loaded(bio(ontol_db)),
        unittest:test_rdb_connect(Rdb,go),
        setrdb(Rdb),
        forall(class(X,molecular_function),
               writeln(id=X)).

/** <module> mappings between ontol_db and GO DB schema

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_sqlmap_go)).
  
  % access biopax OWL model using ontol_db predicates
  demo:-
    load_bioresource(rdb(go)),
    class(ID,rna),
    format('In biopax, rna is a subclass of the following:~n',[]),
    forall(subclass(ID,PID),
           showclass(PID)).

  showclass(ID):-
    class(ID,N),
    format('Class ID:~w~nClass name:~w~n',[ID,N]).
  ==

  ---+ Prerequisites


  Add the following or similar to your odbc.ini:

  ==
[go]
Driver       = /usr/local/lib/libmyodbc5.so
Description  = go_latest
Database     = go_latest
Server       = mysql.ebi.ac.uk
User         = go_select
Password     = amigo
Type         = MySQL
IncludeViews = Yes
  ==

See odbc_setup.txt

  ---+ Description


  ---+ Examples
  
  command line usage


  prolog query mapped to SQL:
  
  =|blip prolog-to-sql -u ontol_sqlmap_go -u blipkit_sql  -proj 'X' 'entity_label(X,apoptosis)'|=

  more complex query. By default parentT/2 is mapped to graph_path/5 in the database (subclassT/2 not mapped, will unfold iteratively)
  
  =|blip-godb -debug sql -u ontol_db -r rdb/go sql-map -proj Y,YN  '(class(X,apoptosis),parentT(Y,X),class(Y,YN))'|=

  combined query: get ontology structure from prolog database, and annotation information from SQL database:
  
  =|blip -r go  -n 'contractile vacuolar membrane'  -u ontol_sqlmap_go -u curation_db -sqlbind curation_db:class_annotated_entity_count/2-go -showannots|=

  show ontology denormalized tree focused around "apoptosis". TODO (need to bind predicates to SQL equivalents, as this is no longer done by default)
  
  =|blip -u ontol_sqlmap_go -r rdb/go2 ontol-subset -n apoptosis|=

  remember, no infix rules on command line:

  ==
  blip-godb -u seqfeature_sqlmap_go -debug sql -u ontol_db prolog-to-sql -proj C  'is(C,count(X,(class(A,apoptosis),parentT(X,A))))'
  ==

  ---+ Example queries

  
  =|class(X,'histone deacetylase activity'),curation_statement(A,S,Role,X),curation_evidence(A,E),evidence_with(E,With)|=

  apoptosis and caspase activity:
  
  =|(class(X1,apoptosis),parentT(Y1,X1),parentT(Y2,'GO:0030693'),curation_statement(A,G,_,Y1),curation_statement(_,G,_,Y2))|=
  
  apoptosis and NO caspase activity:
  
  =|class(X1,apoptosis),parentT(Y1,X1),curation_statement(A,G,_,Y1),not((parentT(Y2,'GO:0030693'),curation_statement(_,G,_,Y2)))|=
  
  @author Chris Mungall
  @version  $Revision: 1.10 $
  @license LGPL
  @see README, go_database_tutorial.txt, ontol_db.pro 
  
  */
