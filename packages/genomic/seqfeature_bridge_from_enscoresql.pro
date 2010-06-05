/* -*- Mode: Prolog -*- */


:- module(seqfeature_bridge_from_enscoresql,
          [
          ]).

:- multifile seqfeature_db:goal_expansion/2.

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

seqfeature_db:goal_expansion(gene(ID,N),seqfeature_bridge_from_enscoresql:gene(ID,N)).
seqfeature_db:gene(ID,ID):-
        rdb_query(sql(cols=['gene_id'=ID],
                      from=['gene'])).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(hs)=
       (seqfeature_bridge_from_enscoresql:set_bridge_resource(hs))/[]).


unittest(test(basic,
            [_=load(hs)],
            (   %trace,
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_bridge_from_enscoresql)),
                gene(ID,N),
                writeln(gene(ID,N))),
            true)).


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/06/24 13:53:08 $
  @license LGPL

  ---+ Name
%  seqfeature_bridge_from_enscoresql

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_from_enscoresql)).
  :- seqfeature_bridge_from_enscoresql:set_bridge_resource(enscore_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Ensembl-core schema to
masquerade as seqfeature predicates

  See <http://www.ensembl.org> Enscore

  */