/* -*- Mode: Prolog -*- */


:- module(taxon_db,
          [
           taxname/2,
           taxname/4,
           taxname/5,
           taxnode/13,
           taxparent/2,

           entity_taxon/2,
           
           taxbinomial/3,
           taxroot/1,
           taxleaf/1,
           taxparentT/2,
           nearest_common_ancestor/3
          ]).

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).

% link between some entity and a taxon
:- extensional(entity_taxon/2).

:- datapreds(taxon_db,[
                       taxnode(pk('Taxon'),
                               fk('Parent',taxnode),
                               atom('Rank'),
                               atom('EmblCode'),
                               atom('DivID'),
                               atom('InhDiv'),
                               atom('GCID'),
                               atom('InhGC'),
                               atom('MitoGCID'),
                               atom('GBHide'),
                               atom('STHide'),
                               atom('Unknown'),
                               atom('Comments')),
                       taxname(fk('Taxon',taxnode),
                               atom('Name'),
                               atom('Name2'),
                               atom('NameClass'),
                               atom('Extra'))
                      ]).

%% taxname(?ID,?ScientificName) is nd
%  
%  scientific name
taxname(ID,N):-
        taxname(ID,N,_,'scientific name',_).
taxname(ID,N,X,Type):-
        taxname(ID,N,X,Type,_).

%% taxbinomial(?ID,?Genus,?Species)
%  
taxbinomial(ID,N1,N2):-
        taxname(ID,N),
        (   concat_atom([N1,N2|_],' ',N)
        ->  true
        ;   N1=N,N2='').

%% taxparent(?ID,?ParentID) is  nd
%% taxparent(+ID,?ParentID) is  d
%
%  true if ParentID is parent of ID
taxparent(ID,PID):-
        taxnode(ID,PID,_Rank,_EmblCode,_DivID,_InhDiv,_GCID,_InhGC,_MitoGCID,_GBHide,_STHide,_Foo,_Comments),
        ID \= PID.                % why do ncbi create a cyclic link 1->1???

%% taxroot(?ID) is  d
%% taxroot(+ID) is  sd
%
%  unifies with the root of the taxonomy tree
%  
taxroot(ID):-
        taxname(ID,_),
        not(taxparent(ID,_)).
%% taxleaf(?ID) is  nd
%
%  unifies with all tips of the taxonomy tree
%  
taxleaf(ID):-
        not(taxparent(_,ID)).

%% taxparentT(?ID,?PID) is : nd
%
%  unifies with all ancestors of node ID (ie transitive parent)
%  
taxparentT(ID,PID):- taxparent(ID,PID).
taxparentT(ID,PID):- taxparent(ID,XID),taxparentT(XID,PID).

%% nearest_common_ancestor(+ID1,+ID2,?PID) is : d
%% nearest_common_ancestor(?ID1,?ID2,?PID) is : nd
%
%  finds least common supernode in taxonomy tree
%  
nearest_common_ancestor(ID1,ID2,PID):-
        setof(ID,common_taxparentT(ID1,ID2,ID),IDs),
        member(PID,IDs),
        not((   member(ID,IDs),
                taxparentT(ID,PID)
            )).

common_taxparentT(ID1,ID2,PID):-
        taxparentT(ID1,PID),
        taxparentT(ID2,PID).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/12/03 00:06:24 $
  @license LGPL

  ---+ Name
  ---++ taxon
- organismal taxonomy tree data

  ---+ Synopsis

    DEPRECATED?? this is more of a mapping than a model

  

  ==
  :- use_module(bio(taxon_db)).
  :- use_module(bio(io)).

  demo:-
    load_bioresource(taxnames),
    load_bioresource(taxnodes),
    taxname(ID1,'Drosophila melanogaster'),
    taxname(ID2,'Homo sapiens'),
    nearest_common_ancestor(ID1,ID2,PID),
    taxname(PID,N),
    format('least common ancestor node of fly and human is ~w~n',[N]).
  ==

  ---+ Description

  intensional and extensional data predicates for taxonomy
data. Schema is identical to NCBI taxonomy model

  ---++ Importing and exporting data

  This is a data module. Facts can be imported and exported from both
prolog fact databases and other formats
  
  ---+++ Import

  The following file formats can be read in using load_biofile/2
and load_bioresource/1

  
  * ncbitaxnames - maps to taxname/5
  * ncbitaxnodes - maps to taxnode/13
  

  ---+++ Export

  No direct export formats are supported. However, using
taxon_bridge_to_ontol it is possible to export to any ontology
format. See ontol_db for details.

  ---++ Bridges

  
  * taxon_bridge_to_ontol - maps to class/2 and subclass/2
  * pheno_bridge_from_taxon - maps to phylonode/4
  
  
  */
