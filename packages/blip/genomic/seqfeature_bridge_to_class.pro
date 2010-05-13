/* -*- Mode: Prolog -*- */


:- module(seqfeature_bridge_to_class,[
                                    ]).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_db)).

:- discontiguous
        ontol_db:class/2,
        ontol_db:restriction/3.

% convert a name to an ID
n2cid(N,ID):-
        (class(ID,N)
        ->  true
        ;   (property(ID,N)
            ->  true
            ;   ID=N)).

% construct a unique ID from a prolog term
makeid(Term,ID):-
        Term =.. TL,
        concat_atom([orphan|TL],'::',ID).

% (?,?) nd
featureloc_id(Loc,LocID):-
        Loc=featureloc(_,_,_,_,_),
        Loc,
        makeid(Loc,LocID).

ontol_db:class(ID):-
        feature(ID).
metadata_db:entity_resource(ID,NS):-
        feature(ID),
        (   concat_atom([NS,_],':',ID)
        ->  true
        ;   NS=feature).
%ontol_db:class_comment(ID,Cmt):-
%        featureprop(ID,description,Cmt).

% TODO: use IDs not names in seqfeature_db model
ontol_db:subclass(ID,CID):-
        feature(ID,_,CN),
        (class(CID,CN)
        ->  true
        ;   %format(user_error,'no_such_class ~w~n',[CN]),
            CID=CN).
ontol_db:restriction(ID,'OBD:in_organism',Org):-
        feature_organism(ID,Org).
ontol_db:restriction(ID,R,ToClass):-
        feature_cvterm(ID,R,ToClass).
ontol_db:restriction(ID,T,PID):-
        feature_relationship(ID,PID,T1),
        n2cid(T1,T).

% TODO  - uncomment when obo handles metadata
%ontol_db:inst_sv(ID,P,V,'xsd:string'):-
%        featureprop(ID,P,V).

% now done automatically from metadata_db
%ontol_db:synonym(ID,exact,Syn):-
%        featureprop(ID,synonym,Syn).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/06/22 02:10:33 $
  @license LGPL

  ---+ Name
  ---++ seqfeature_bridge_to_class
- view go assocs as generic classs

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_to_class)).

  ==

  ---+ Description

  bridging layer from extensional data predicates in seqfeature module.

  treats a feature as a class (set)


 ---++ Command line usage

 exports a gene association file to OWL. Note that 'obo' can also be
used as an option
 
 ==
  blip -f chaos -i Rab1.chaos-xml -u seqfeature_bridge_to_class io-convert -to owl -o Rab1-feature-as-class.owl
 ==

 ==
  blip -r so  -include "feature ontology" -f chaos -i Rab1.chaos-xml io-convert -u seqfeature_bridge_to_class -u ontol_db -to obo -o Rab.obo
 ==
  

  ---++ TODO

  this is unfinished - only completed to extent needed for go annotations right now
  
  correct treatment of features. diff between 1d (SO) and 3d structures?
  
  */
