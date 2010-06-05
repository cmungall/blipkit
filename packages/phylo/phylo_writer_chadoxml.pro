/* -*- Mode: Prolog -*- */


:- module(phylo_writer_chadoxml,[
                                 ]).
:- use_module(bio(io)).
:- use_module(bio(xml_writer)).
:- use_module(bio(mode)).
:- use_module(bio(phylo_db)).
:- use_module(bio(taxon_db)).
:- use_module(bio(bioprolog_util)).

:- discontiguous write_pred/2.

io:redirect_stdout(phylo_chadoxml).
io:write_all(phylo_chadoxml):-
        xmldoc_start(X),
        xmlnode_start(X,X2,chado),
        
        xmlnode_comment(X2,'chado macros'),
        dsetof(Sp,ID^phylonodeprop(ID,'S',Sp),Sps),
        forall(member(Sp,Sps),
               write_pred(X2,phylo_db:organism(Sp))),
        dsetof(Prop,ID^V^phylonodeprop(ID,Prop,V),Props),

        write_pred(X2,phylo_db:cv(phylogenetic_property)),
        forall(member(Prop,Props),
               write_pred(X2,phylo_db:property(Prop))),
        
        xmlnode_comment(X2,'main data'),
        forall(phylo_db:phylotree(Root),
               write_pred(X2,phylo_db:phylotree(Root))),

        xmlnode_end(X2,X).

write_pred(X,phylo_db:phylotree(ID)):-
        once(phylotree_index(ID,LRs)),
        concat_atom([ID,tree],'-',TreeID),
        xmlnode_start(X,X2,phylotree,[id=TreeID]),
        write_pred_in(X2,dbxref_id,phylo_db:dbxref(test,1)),
        write_preds(X2,phylo_db:phylonode(ID,_,_,_),LRs),
        xmlnode_end(X2,X).

write_pred(X,phylo_db:phylonode(ID,Name,PID,BranchLen),LRs):-
        xmlnode_start(X,X2,phylonode,[id=ID]),
        xmlnode(X2,label,Name),
        xmlnode(X2,distance,BranchLen),
        (PID=root
        ->  true
        ;   xmlnode(X2,parent_phylonode_id,PID)),
        member(idx(Left,Right,ID),LRs),
        xmlnode(X2,left_idx,Left),
        xmlnode(X2,right_idx,Right),
        write_preds(X2,phylo_db:phylonodeprop(ID,_,_)),
        forall(phylonodeprop(ID,'S',Sp),
               write_pred(X2,phylo_db:phylonode_species(ID,Sp))),
        xmlnode_end(X2,X),
        write_preds(X,phylo_db:phylonode(_,_,ID,_),LRs).

write_pred(X,phylo_db:phylonodeprop(_ID,P,V)):-
        xmlnode_start(X,X2,phylonodeprop),
        xmlnode(X2,type_id,P),
        xmlnode(X2,value,V),
        xmlnode_end(X2,X).

write_pred(X,phylo_db:phylonode_species(_,Sp)):-
        (   taxname(TaxID,Sp,_,_,_)
        ->  xmlnode_start(X,X2,phylonode_organism),
            xmlnode(X2,organism_id,TaxID),
            xmlnode_end(X2,X)
        ;   true).

write_pred(X,phylo_db:organism(Sp)):-
        (   taxname(ID,Sp,_,_,_),
            taxbinomial(ID,N1,N2)
        ->  xmlnode_start(X,X2,organism,[id=ID,op=lookup]),
            xmlnode(X2,common_name,Sp),
            xmlnode(X2,genus,N1),
            xmlnode(X2,species,N2),
            xmlnode_end(X2,X)
        ;   true).

write_pred(X,phylo_db:property(P)):-
        xmlnode_start(X,X2,cvterm,[id=P,op=lookup]),
        xmlnode(X2,cv_id,phylogenetic_property),
        xmlnode(X2,name,P),
        xmlnode_end(X2,X).

write_pred(X,phylo_db:cv(N)):-
        xmlnode_start(X,X2,cv,[id=N,op=lookup]),
        xmlnode(X2,name,N),
        xmlnode_end(X2,X).

write_pred(X,phylo_db:dbxref(DB,ID)):-
        xmlnode_start(X,X2,dbxref),
        xmlnode(X2,accession,ID),
        write_pred_in(X2,db_id,phylo_db:db(DB)),
        xmlnode_end(X2,X).

write_pred(X,phylo_db:db(N)):-
        xmlnode_start(X,X2,db),
        xmlnode(X2,name,N),
        xmlnode_end(X2,X).

write_pred_in(X,E,Pred):-
        xmlnode_start(X,X2,E),
        write_pred(X2,Pred),
        xmlnode_end(X2,X).

write_preds(X,Pred):-
        forall(Pred,write_pred(X,Pred)).
write_preds(X,Pred,Args):-
        forall(Pred,write_pred(X,Pred,Args)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL


  
  */