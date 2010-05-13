/* -*- Mode: Prolog -*- */



:- module(phylo_xmlmap_phyloxml,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(phyloxml,XML,PL):-
        apply_xmlpred(phylo_xmlmap_phyloxml,XML,PL).

% http://www.phyloxml.org:
xmlpred(phyloxml,_,[],
        [
         translate(phylogeny,top)
        ]).
xmlpred(phylogeny,_,phylogeny(ID,Rooted),
        [
         prolog(gensym(phylo,ID)),
         let(Rooted=att(rooted)),
         translate(name,ID),    % TODO - treat phylogeny differently from node name
         translate(clade,ID)]).
xmlpred(clade,PID,[phylonode(ID),phylonode_parent(ID,PID,Len)],
        [
         prolog(gensym(phylo,ID)),
         translate(name,ID),
         translate(events,ID),
         translate(taxonomy,ID),
         translate(branch_length,Len), % todo - also as attribute
         translate(confidence,ID),
         translate(clade,ID)
         ]).
xmlpred(name,ID,[metadata_db:entity_label(ID,Name)],
        let(Name='.')).
xmlpred(taxonomy,ID,phylonode_taxon(ID,Tax),
        let(Tax=code)). % todo
xmlpred(events,ID,[],
        [translate(speciations,ID),
         translate(duplications,ID)]).
xmlpred(speciations,ID,[phylonode_speciations(ID,Num)],
        let(Num=number('.'))).
xmlpred(duplications,ID,[phylonode_duplications(ID,Num)],
        let(Num=number('.'))).
xmlpred(branch_length,Len,[],
        let(Len=number('.'))).
xmlpred(confidence,ID,[phylonode_confidence(ID,Type,Num)],
        [let(Type=att(type)),
         let(Num=number('.'))]).


        

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest([
        load(bcl2)=
       load_biofile(phyloxml,'bcl_2.phylo-xml')/[],
        
        test(load_file,
             [_=load(bcl2)],
             (   ensure_loaded(bio(phylo_db)),
                 setof(N,ID^phylonode_taxon(ID,N),Ns),
                 writeln(Ns)),
             (true))
       ]).

        
/** <module> maps phylo_db to PhyloXML

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(phylo_db)).
  :- use_module(bio(phylo_xmlmap_phyloxml)).

  demo:-
    load_biofile(phyloxml,'bcl_2.phylo-xml'),
    forall(phylotree(Node),
           (   phylonode_height(Node,Height),
               format('tree ~w has height ~w~n',[Node,Height]))).
  ==

  bridging layer from phylo-xml to native phylo model (phylo_db).

  ---+ Mapping
  
  See the source for details of the mapping

  * <phylogeny> --> phylogeny/2
  * <clade><branch_length> --> phylonode/1, phylonode_parent/3
  * <taxonomy> --> phylonode_taxon/2
  * <name> --> entity_label/2 (in metadata_db.pro)
  * <speciations> --> phylonode_speciations/2
  * <duplications> --> phylonode_duplications/2
  * <confidence> --> phylonode_confidence/3

    ---+ Use

  you should not need to use this module directly - use load_biofile/2 with =|phyloxml|= as first argument

@author  Chris Mungall
@version $Revision$
@see     README
@license License


  */
