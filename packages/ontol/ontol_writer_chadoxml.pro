/* -*- Mode: Prolog -*- */



:- module(ontol_writer_chadoxml, [
                                  ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

io:write_all(chadoxml,_,_):-
        doc_start(X),
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
        dsetof(O,ID^N^(belongs(ID,O),class(ID,N)),Os),
        forall(member(O,Os),
               write_ontology(chadoxml,O)),
        forall(inst_of(ID,_),
               write_inst(chadoxml,ID)).

write_ontology(chadoxml,O):-
        write_header,
        forall_strict(belongs(ID,O),
                      write_entity(chadoxml,ID)).
        
write_header:-
        format('format-version: 1.2~n'),
        nl.

write_class(chadoxml,ID):-
        class(ID,N).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 03:35:06 $
  @license LGPL

  ---+ Name
  ---++ ontol_writer_chadoxml
- generates chadoxml format from ontol_db

  ---+ Synopsis

  ==
  

  ==

  ---+ Description

  TODO
  
**/
