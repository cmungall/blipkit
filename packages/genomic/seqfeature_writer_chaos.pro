/* -*- Mode: Prolog -*- */



:- module(seqfeature_writer_chaos, []).

:- use_module(bio(seqfeature_db)).
:- use_module(bio(xml_writer)).
:- use_module(bio(bioprolog_util)).

exclude_type(start_codon).
exclude_feature(F):-
        feature(F,_,T),
        exclude_type(T).

io:write_all(chaos,_,_):-
        doc_start(X),
        xmlnode_start(X,X2,chaos),
        
        forall( (feature(F),\+ exclude_feature(F)),
               write_pred(X2,feature(F))),
        forall( (   feature_relationship(F,ObjF,Type),
                    \+ exclude_feature(F),
                    \+ exclude_feature(ObjF)),
                write_pred(X2,feature_relationship(F,ObjF,Type))),
        
        xmlnode_end(X2,X).

write_pred(X,feature(F)):-
        feature(F,N,Type),
        xmlnode_start(X,X2,feature),
        xmlnode(X2,feature_id,F),
        (   N=''
        ->  true
        ;   xmlnode(X2,name,N)),
        xmlnode(X2,type,Type),
        write_preds(X2,featureloc(F,_,_,_,_,_,_)),
        write_preds_once(X2,feature_residues(F,_)),
        xmlnode_end(X2,X).

write_pred(X,feature_residues(_,Seq)):-
        xmlnode(X,residues,Seq).
        
write_pred(X,featureloc(_,S,Beg,End,Dir,R,G)):-
        xmlnode_start(X,X2,featureloc),
        xmlnode(X2,srcfeature_id,S),
        xmlnode(X2,nbeg,Beg),
        xmlnode(X2,nend,End),
        xmlnode(X2,strand,Dir),
        xmlnode(X2,rank,R),
        xmlnode(X2,locgroup,G),
        xmlnode_end(X2,X).

write_pred(X,feature_relationship(F,PF,Type)):-
        xmlnode_start(X,X2,feature_relationship),
        xmlnode(X2,subject_id,F),
        xmlnode(X2,object_id,PF),
        xmlnode(X2,type,Type),
        (   feature_relationship_order(F,PF,Type,Rank)
        ->  xmlnode(X2,rank,Rank)
        ;   true),
        xmlnode_end(X2,X).

write_preds(X,Pred):-
        forall(Pred,write_pred(X,Pred)).
% 0 or 1 times
write_preds_once(X,Pred):-
        (   findall(Pred,Pred,[Pred1|Preds])
        ->  write_pred(X,Pred1),
            (   Preds=[]
            ->  true
            ;   throw(error(expected_one_fact(Pred,Pred1,Preds))))
        ;   true).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.3 $
  @date  $Date: 2005/12/08 02:05:41 $
  @license LGPL

  ---+ Name
  ---++ seqfeature_writer_chaos
- generates chaos format from seqfeature_db

  ---+ Synopsis
  
  ==
  

  ==

  ---+ Description

**/
