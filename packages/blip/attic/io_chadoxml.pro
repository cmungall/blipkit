/* -*- Mode: Prolog -*- */


:- module(io_chadoxml,[
                       ]).

:- use_module(bio(io)).
:- use_module(bio(xml_writer)).
:- use_module(library(sgml)).

io:write_data(chadoxml):-
        xmldoc_start(X),
        xmlnode_start(X,X2,chado),
        
        % module: sb
        xmlnode_comment(X2,'chado cvterm macros for events and participants'),
        forall(sb_db:annotation(_,URI,_,_),
               write_pred(X2,ontol_db:class(URI))),

        xmlnode_comment(X2,'compartments'),
        write_preds(X2,sb_db:compartment(_,_)),

        xmlnode_comment(X2,'species'),
        write_preds(X2,sb_db:species(_,_,_)),

        xmlnode_comment(X2,'reactions'),
        write_preds(X2,sb_db:reaction(_,_)),

        xmlnode_end(X2,X).

% module: identifier (no such module as yet, but we still want to export ids)
write_pred(X,identifier:id(ID)):-
        xmlnode_start(X,X2,dbxref,[id=ID]),
        split_id(ID,DB,Acc),
        xmlnode_start(X2,X3,db,[id=DB]),
        xmlnode(X3,name,DB),
        xmlnode_end(X3,X2),
        xmlnode(X2,accession,Acc),
        xmlnode_end(X2,X).

% module: ontol
write_pred(X,ontol_db:class(ID)):-
        xmlnode_start(X,X2,cvterm,[id=ID]),
        write_pred(X2,identifier:id(ID)),
        xmlnode_end(X2,X).

% module: sb
write_pred(X,sb_db:compartment(ID,N)):-
        xmlnode_start(X,X2,participant,[id=ID]),
        xmlnode(X2,name,N),
        xmlnode(X2,uniquename,ID),
        write_preds(X2,sb_db:annotation(ID,_,_,_)),
        xmlnode_end(X2,X).

write_pred(X,sb_db:species(ID,N,CompartmentID)):-
        xmlnode_start(X,X2,participant,[id=ID]),
        xmlnode(X2,name,N),
        xmlnode(X2,uniquename,ID),
        xmlnode_start(X2,X3,participant_relationship),
        xmlnode(X3,type_id,'cvterm__OBO_REL:contained_in'),
        xmlnode(X3,object_id,CompartmentID),
        xmlnode_end(X3,X2),
        write_preds(X2,sb_db:annotation(ID,_,_,_)),
        xmlnode_end(X2,X).

write_pred(X,sb_db:reaction(ID,N)):-
        xmlnode_start(X,X2,event,[id=ID]),
        xmlnode(X2,name,N),
        xmlnode(X2,uniquename,ID),
        write_preds(X2,sb_db:annotation(ID,_,_,_)),
        write_preds(X2,sb_db:reaction_reactant(ID,_)),
        write_preds(X2,sb_db:reaction_product(ID,_)),
        write_preds(X2,sb_db:reaction_modifier(ID,_)),
        xmlnode_end(X2,X).

write_pred(X,sb_db:annotation(_,_,_,ClassID)):-
        xmlnode(X,type_id,ClassID).

reactionpred_type(sb_db:reaction_reactant,has_input).
reactionpred_type(sb_db:reaction_product,has_output).
reactionpred_type(sb_db:reaction_modifier,has_modifier).
write_pred(X,sb_db:Pred):-
        Pred =.. [Func,_RID,SID],
        reactionpred_type(sb_db:Func,Type),
        xmlnode_start(X,X2,event_participant,[]),
        xmlnode(X2,type_id,Type),
        xmlnode(X2,participant_id,SID),
        xmlnode_end(X2,X).

write_preds(X,Pred):-
        forall(Pred,write_pred(X,Pred)).
        

%%

split_id(ID,NS,LocalID):-
        sub_atom(ID,0,_,_,'http://'),
        !,
        sub_atom(ID,From,_,Dist,'#'),
        sub_atom(ID,0,From,_,NS),
        sub_atom(ID,_,Dist,0,LocalID).
split_id(ID,NS,LocalID):-
        sub_atom(ID,From,_,Dist,':'),
        !,                      % if multi :s, use first
        sub_atom(ID,0,From,_,NS),
        sub_atom(ID,_,Dist,0,LocalID).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2006/01/04 11:49:05 $
  @license LGPL

  DEPRECATED
  
  */