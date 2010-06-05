/* -*- Mode: Prolog -*- */


:- module(goa_bridge_from_gosql,
          [
          ]).

:- multifile goa_db:goal_expansion/2.

:- use_module(bio(goa_db),[]).
:- use_module(bio(seqfeature_db),[]).
:- use_module(bio(rdb_util)).


getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

set_bridge_resource(Resource):-
        rdb_connect(Rdb,Resource),
        setrdb(Rdb).

rdb_query(Sqlt):-
        getrdb(Rdb),
        sqltmpl_sqlterm(Sqlt,SqlTerm),
        rdb_query(Rdb,SqlTerm).

goa_db:association(ID,ClassID,FeatureID,IsNot):-
        rdb_query(sql(cols=['a.id'=ID,
                            't.acc'=ClassID,
                            'a.gene_product_id'=FeatureID,
                            'a.is_not'=IsNot],
                      from=['association AS a INNER JOIN term AS t ON (a.term_id=t.id)'])).
goa_db:evidence(AID,EClassID):-
        rdb_query(sql(cols=['association_id'=AID,
                            'code'=EClassID],
                      from=['evidence'])).

goa_db:goal_expansion(goa_distinct_feature_count_by_term(ID,Db,Code,Num),
                      goa_bridge_from_gosql:goa_distinct_feature_count_by_term(ID,Db,Code,Num)).
goa_db:goa_distinct_feature_count_by_term(ID,Db,Code,Num):-
        rdb_query(sql(cols=['gpc.product_count'=Num,
                            'speciesdbname'=Db,
                            'code'=Ev,
                            't.acc'=ID],
                      from=['gene_product_count AS gpc INNER JOIN term AS t ON (t.id=gpc.term_id)'])).

goa_db:goal_expansion(goa_distinct_feature_count(Db,Num),
                      goa_bridge_from_gosql:goa_distinct_feature_count(Db,Num)).
goa_db:goa_distinct_feature_count(Db,Num):-
        rdb_query(sql(cols=['count (gp.id)'=Num,
                            'x.xref_dbname'=Db]
                      from=['gene_product AS gp INNER JOIN dbxref AS x ON (x.id=gp.dbxref_id)'])).
        

seqfeature_db:feature(ID,N,Type):-
        rdb_query(sql(cols=['p.id'=ID,
                            'p.symbol'=N,
                            't.name'=Type],
                      from=['gene_product AS p INNER JOIN term AS t ON (type_id=t.id)'])).



% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(go)=
      (   goa_bridge_from_gosql:set_bridge_resource(go),
          load_bioresource(go))/[]).


unittest(test(basic,
            [_=load(go)],
            (   ensure_loaded(bio(goa_db)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(goa_bridge_from_gosql)),
                class(ClassID,'ribosome'),
                forall((   association(AID,ClassID,FeatureID,_),
                           feature(FeatureID,Name,Type),
                           evidence(AID,Ev)),
                       (   format(' Assoc to [~w] ~w ~w [~w]~n',[Type,FeatureID,Name,Ev])))),
            
            true)).

unittest(test(basic2,
            [_=load(go)],
            (   ensure_loaded(bio(goa_db)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(goa_bridge_from_gosql)),
                class(ClassID,'ribosome'),
                forall(association(_,ClassID,FeatureID,_,Name,Type,Ev),
                       (   format(' Assoc to [~w] ~w ~w [~w]~n',[Type,FeatureID,Name,Ev])))),
            
            true)).

unittest(test(recursive,
            [_=load(go)],
            (   ensure_loaded(bio(goa_db)),
                ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(goa_bridge_from_gosql)),
                class(QClassID,'cysteine metabolism'),
                forall((   associationRT(QClassID,_,ClassID,FeatureID,_N),
                           feature(FeatureID,Name,Type)),
                       (   class(ClassID,ClassName),
                           format(' Assoc to [~w] ~w ~w [~w ~w]~n',[Type,FeatureID,Name,ClassID,ClassName])))),
            
            true)).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.3 $
  @date  $Date: 2005/10/11 03:23:16 $
  @license LGPL

  ---+ Name
%  goa_bridge_from_gosql

  ---+ Synopsis

  ==
  :- use_module(bio(goa_db)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(goa_bridge_from_gosql)).
  :- goa_bridge_from_gosql:set_bridge_resource(go).
  
  % fetch classes from bioresource (obo file), fetch associations
  % from database
  demo:-
    class(ID,'L-cysteine catabolism to taurine'),
    association...
    format('[~w] ~w~n',[PID,N]),
    fail.
  ==

  ---+ Description

  This allows an SQL Database using the GO Schema to masquerade as
facts from the goa_db datalog module

  This also projects views onto seqfeature_db

  See <http://www.godatabase.org/dev> GODatabase

  */