:- module(phylo_bridge_to_ontol,[]).

:- use_module(phylo_db).
:- use_module(bio(ontol_db),[]).

ontol_db:inst(X):- phylonode(X).
ontol_db:inst(X):- phylogeny(X,_).
%ontol_db:inst(X-Y):- phylonode_parent(X,Y).
ontol_db:inst_of(X,'phylo:Phylogeny'):- phylogeny(X,_).
ontol_db:inst_of(X,'phylo:Clade'):- phylonode(X).
%ontol_db:inst_of(X-Y,'phylo:Branch'):- phylonode_parent(X,Y).
ontol_db:inst_rel(X,'phylo:parent',Y):- phylonode_parent(X,Y).
%ontol_db:inst_sv(X-Y,'phylo:length',Len):- phylonode_parent(X,Y).
ontol_db:inst_sv(X,'phylo:branch_length',Len,'xsd:float'):- phylonode_branchlen(X,Len).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest([
        load(bcl2)=
       load_biofile(phyloxml,'bcl_2.phylo-xml')/[],
        
        test(load_file,
             [_=load(bcl2)],
             (   ensure_loaded(bio(phylo_db)),
                 ensure_loaded(bio(phylo_bridge_to_ontol)),
                 inst_sv('145_XENLA','phylo:branch_length',Len),
                 writeln(Len)),
             (true))
       ]).


/** <module> manifests an ontology view of phylogenetic data

  ---+ Synopsis

==
  :- use_module(bio(io)).
  :- use_module(bio(phylo_db)).
  :- use_module(bio(phylo_xmlmap_phyloxml)).
  :- use_module(bio(phylo_bridge_to_ontol)).

  demo:-
    load_biofile(phyloxml,'bcl_2.phylo-xml'),
    write_biofile(owl,'bcl_2.owl').
==

---+ Mapping

Although ontol_db.pro and OBO allow n-ary relations, we avoid them here to retain compatibility with OWL.

We model branchlengths as a property of the clade/node. This works for
trees and is compact, but is slightly unsatisfactory - ideally it is the property of the relation between clades.


A temporary ontology is used for now. Map to CDAO?

  * phylogeny/2 --> instances of Phylogeny
  * phylonode/2 --> instances of Clade
  * phylonode_branchlen/2 --> data properties of xsd:float branch_length to clade
  * phylonode_parent/2 --> instance level relations between 
  
  
@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
