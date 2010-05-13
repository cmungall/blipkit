/* -*- Mode: Prolog -*- */


:- module(taxon_bridge_to_ontol,[]).
:- use_module(bio(taxon_db)).
:- use_module(bio(ontol_db)).

ontol_db:belongs(ID,taxonomy):-
        taxname(ID,_).
ontol_db:class(ID,N):-
        taxname(ID,N).
ontol_db:subclass(ID,PID):-
        taxparent(ID,PID).
ontol_db:synonym(ID,exact,Syn):-
        taxname(ID,Syn,_,Categ),
        Categ \= 'scientific name'.

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/12/03 00:06:24 $
  @license LGPL

  ---+ Name
%  taxon_bridge_to_ontol

  ---+ Synopsis
% 

  ==
  :- use_module(bio(io)).
  :- use_module(bio(taxon_bridge_to_ontol)).
  :- use_module(bio(taxon_db)).
  :- use_module(bio(ontol_db)).

  show_class(ID):-
    class(ID,Name),
    format(' [~w] ~w~n',[ID,Name]).
  
  demo:-
    load_bioresource(taxnames),
    load_bioresource(taxnodes),
    class(ID,'Drosophila melanogaster'),
    format('Lineage of fruitfly is:~n'),
    forall(subclassT(ID,ParentID),
           show_class(ParentID)).
  ==

  ---+ Description
  
  mapping from the taxon module to the ontol module. ontol predicates
can be used to view taxon data

  ---++ Mapping

  
  * taxname/2 to class/2
  * taxparent/2 to subclass/2
  * taxname/4 to synonym/4
  
  
  */
