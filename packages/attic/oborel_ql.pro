/* -*- Mode: Prolog -*- */



:- module(oborel_ql,
          [is_a/2,
           part_of/2,
           is_aN/2,
           part_ofN/2,
           op(300,xfy,is_a),
           op(300,xfy,part_of),
           op(300,xfy,is_aN),
           op(300,xfy,part_ofN)
          ]).

:- op(300,xfy,'is_a*').
:- op(300,xfy,is_a).
:- op(300,xfy,part_of).
:- op(300,xfy,is_aN).
:- op(300,xfy,part_ofN).

:- use_module(bio(ontol_db)).

X is_a Y:- subclass(X,Y).
X part_of Y:- restriction(X,part_of,Y).

X is_aN Y:- subclassN(X,Y).
X part_ofN Y:- restrictionN(X,part_of,Y).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/01/25 02:17:22 $
  @license LGPL

  ---+ Name
  ---++ oborel_ql
- query lang for OBO relations

  ---+ Synopsis

  ==
  :- use_module(bio(oborel_ql)).

  ==

  ---+ Description

**/