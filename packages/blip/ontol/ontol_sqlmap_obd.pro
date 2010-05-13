:- module(ontol_sqlmap_obd,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(curation_db),[]). % move?

:- load_schema_defs(bio('sql_schema/schema_obd')).

ontol_db:parent0(X,R,Y) <- node(XI,X,_,_),link_asserted(XI,RI,YI),node(RI,R,_,_),node(YI,Y,_,_).
ontol_db:parent0(X,Y) <- node(XI,X,_,_),link_asserted(XI,_,YI),node(YI,Y,_,_).

:- [ontol_sqlmap_obd_core].





% ----------------------------------------
% TESTS
% ----------------------------------------
unittest:testq(id(ID),(entity_label(ID,molecular_function))).
unittest:testq(child(Y,XN),(entity_label(Y,apoptosis),parent(X,Y),entity_label(X,XN))).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                rdb_connect(Rdb,obd2),
                forall(testq(Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           nl))),
            true)).


unittest(test(inline,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ontol_sqlmap_obd:test_inline),
            true)).

test_inline:-
        ensure_loaded(bio(ontol_db)),
        rdb_connect(Rdb,obd2),
        setrdb(Rdb),
        forall(class(X,molecular_function),
               writeln(id=X)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.10 $
  @date  $Date: 2005/09/30 16:23:45 $
  @license LGPL

  ---+ Name
%  ontol_sqlmap_obd

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_sqlmap_obd)).
  
  % access biopax OWL model using ontol_db predicates
  demo:-
    load_bioresource(rdb(obd)),
    class(ID,rna),
    format('In biopax, rna is a subclass of the following:~n',[]),
    forall(subclass(ID,PID),
           showclass(PID)).

  showclass(ID):-
    class(ID,N),
    format('Class ID:~w~nClass name:~w~n',[ID,N]).
  ==

  ---+ Description

  command line usage:
  ==
  blip -u ontol_sqlmap_obd -r rdb/obd2 ontol-subset -n apoptosis
  ==

  ==
  blip-sql -debug sql -r obd/obdp -u ontol_sqlmap_obd sql-map "curation_statement(A,'OMIM:601653.0001',R,P)"
  ==

  blip-sql -u ontol_db -u ontol_sqlmap_obd -sqlbind ontol_db:inst_rel_type/3-pkb -r obd/pkb findall "inst_rel_type(X,Y,Z)"

  (now have to bind to the full term e.g obd(pkb) or just _)

  ==
  blip -r obd/obdbirn -debug sql -u curation_db -u ontol_db -u blipkit_sql -u ontol_sqlmap_obd sql-map "curation_statementT(_,'PKB:Human_with_Parkinsons_Disease',_,X,Y)"

  blip -r obd/obdbirn -debug sql -u curation_db -u ontol_db -u blipkit_sql -u ontol_sqlmap_obd ontol-fmatch PKB:Human_with_Parkinsons_Disease PKB:nlx_organ_20090204_214 -sqlbind "curation_db:curation_statementT/5-_"
  ==

  ---++ Finding the LCA given two classes

  In ontol_db.pro, the predicate subclass/2 is used to relate a class
  to its superclass. E.g. =|subclass(human,homo)|=. subclassT/2 is the
  transitive version of this predicate, and subclassRT/2 is the
  reflexive transitive version (i.e. =|subclassRT(human,human)|=)

  The class_pair_subclass_lca/3 definition looks like this:
  
  ==
  class_pair_subclass_lca(X,Y,LCA):-
        subclassRT(X,LCA),
        subclassRT(Y,LCA),
        \+ (( subclassRT(X,CA),
              subclassRT(Y,CA),
              subclassT(CA,LCA))).
  ==

  i.e. LCA is the lowest common ancestor of X and Y by subclass if:
  
  * subclassRT/2 holds between X and LCA (i.e. LCA is an ancestor of or identical to X)
  * subclassRT/2 holds between Y and LCA (i.e. LCA is an ancestor of or identical to Y)
  * there is no common ancestor of X and Y that is more 'recent' than LCA

  We could avoid some repetition by defining CA first, and then LCA from there. Anyway.

  The above predicate will work given a prolog database of subclass/2 facts. E.g.

  ==
  blip -i http://purl.org/obo/obo-all/ncbi_taxonomy/ncbi_taxonomy.pro -f ontol_db:pro -u ontol_db findall "(class(X,'Mus musculus'),class(Y,'Homo sapiens'),class_pair_subclass_lca(X,Y,A))" -select A -label
  ==

  (This may take a while for the initial download, but the ontology is
  cached for future references)

  After a short wait this will yield A=NCBITaxon:314146 (which has the
  scientific name 'Euarchontoglires'. The -label option will use
  entity_label/2 to find the labels for class IDs)

  This clause will also work for relational databases too.
  
  The first way to do this is to bind all predicates in the model to the database:
  
  ==
  blip-obd -debug sql -r obd/pkb2 -sqlbind ontol_db:all -sqlbind metadata_db:all findall "(class(X,'Mouse'),class(Y,'Human'),class_pair_subclass_lca(X,Y,A))" -select A -label
  ==

  blip-obd is an alias for =|blip -u ontol_db -u blipkit_sql -u ontol_sqlmap_obd|=

  ontol_sqlmap_obd.pro contains mappings between ontol_db model facts and the obd schema.

  For example:
  ==
  ontol_db:subclassT(X,Y) <- node(XI,X,_,_),link(XI,RI,YI,_),node(YI,Y,_,_),node(RI,'OBO_REL:is_a',_,_),\+ (XI=YI).
  ==

  The tables 'node' and 'link' are used for classes and transitive
  reflexive relationships in OBD. We have to specify extra joins here
  as like many relational databases this uses internal integer
  surrogate keys. We don't want to expose these at the ontol_db model
  level, so they are hidden in the mapping.
  
  We use sqlbind/2 to use all mappings in the sqlmap module.

  Note that this results in class_pair_subclass_lca/3 being rewritten
  to a goal that executes a SQL query. This is done entirely
  automatically by examining the prolog rule - there is no
  human-specified mapping for this rule.  The resulting SQL query is
  quite large and uses a SELECT .. WHERE NOT EXISTS ... pattern with
  lots of joins. In theory the RDBMS should be able to optimize this,
  but if not we have the option of more fine grained control over what
  gets rewritten to SQL.

  (we also map entity_label/2 to the database, to fetch class labels
  from there too. The label fetching is done iteratively here but it
  could easily be folded into the main query)

  The following only rewrites the transitive subclass predicates. class_pair_subclass_lca/3 is executed by the prolog engine:
  
  ==
  blip-obd -debug sql -r obd/pkb2 -sqlbind ontol_db:class/1 -sqlbind ontol_db:subclassT/2 -sqlbind ontol_db:subclassRT/2 -sqlbind metadata_db:all findall "(class(X,'Mouse'),class(Y,'Human'),class_pair_subclass_lca(X,Y,A))" -select A -label
  ==

  This results in multiple individual subclassT/2 and subclassRT/
  queries being executed. All CAs are checked until one that is the
  LCA is found. This more procedural approach may be less efficient,
  but it is less dependent on database join optimization.
  
  ---+ See Also


  
  */

