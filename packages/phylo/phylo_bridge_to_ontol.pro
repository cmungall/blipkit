:- module(phylo_bridge_to_ontol,[]).

:- use_module(phylo_db).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(ontol_db),[]).

metadata_db:entity_synonym(X,Y) :- phylonodeprop(X,'ID',Y).
metadata_db:entity_synonym_scope(X,Y,'RELATED') :- phylonodeprop(X,'ID',Y).
metadata_db:entity_xref(X,Y) :- phylonodeprop(X,'S',Y).

ontol_db:class(X) :- phylonode(X).
ontol_db:class(X) :- phylogeny(X,_).
ontol_db:restriction(X,'phylo:parent',Y) :- phylonode_parent(X,Y).
ontol_db:inst_sv(X,'phylo:branch_length',Len,'xsd:float') :- phylonode_branchlen(X,Len).
