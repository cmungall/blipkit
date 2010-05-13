/* -*- Mode: Prolog -*- */



:- module(ontol_writer_jsontree,
          [
	   edges_to_jsontree_atom/3
           ]).

:- multifile ontol_writer:write_class/2.

:- use_module(bio(mode)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).
:- use_module(library('http/json')).

ontol_writer:write_class(jsontreenl,ID,Opts):-
        ontol_writer:write_class(jsontree,ID,Opts),
        nl.


node_json(ID,json([id=ID,name=Name,children=Children])):-
	entity_label(ID,Name),
	findall(json(['_reference'=C]),subclass(C,ID),Children).

edges_to_jsontree_atom(Edges,A,Opts):-
	edges_to_jsontree(Edges,JSON,Opts),
	atom_json_term(A,JSON,[as(atom)]).
edges_to_jsontree(Edges,JSON,_Opts):-
        solutions(ID,(   member(edge(_,ID,_),Edges)
                     ;   member(edge(ID,_,_),Edges)),IDs),
	findall(json([id=ID,name=Name,children=Children|Attrs]),
		(   member(ID,IDs),
		    entity_label(ID,Name),
		    findall(Attr,node_attr(ID,Edges,Attr),Attrs),
		    %(	member(edge(ID,_,_),Edges)
		    %->	IsRoot=false
		    %;	IsRoot=true),
		    findall(json(['_reference'=C]),
			    member(edge(C,ID,_),Edges),
			    Children)),
		JsonNodes),
	JSON=json([label=name,
		   identifier=id,
		   items=JsonNodes]).

node_attr(ID,Edges,isRoot=true):-
	\+ member(edge(ID,_,_),Edges).

node_attr(ID,_,definition=Def):-	def(ID,Def).
node_attr(ID,_Edges,link=json([relation=R,target=T])):-	restriction(ID,R,T).


/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ ontol_writer_jsontree
- 

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_writer_jsontree)).

  ==

  ---+ Description

  writes ontology classes as formatted jsontree

  Designed to be used in conjunction with the Dojo tree widget
  
**/
