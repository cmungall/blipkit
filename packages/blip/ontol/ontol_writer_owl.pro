/* -*- Mode: Prolog -*- */


:- module(ontol_writer_owl,[write_owl/1]).
:- use_module(library('semweb/rdf_db')).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_bridge_to_owl)).

io:write_all(owl,F,_):-
        ontol_writer_owl:write_owl(F).

write_owl(File):-
        forall(idspace(NS,URI),
               rdf_register_ns(NS,URI)),
        owl_assertall,
        force_ns(xsd),          % ??? required for older versions of lib
        rdf_save(File).


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
