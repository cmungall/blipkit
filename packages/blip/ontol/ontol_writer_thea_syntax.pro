/* -*- Mode: Prolog -*- */


:- module(ontol_writer_thea_syntax,[write_thea_syntax/1]).

:- use_module(bio('thea/owl_parser')).
:- use_module(bio('thea/owl_as2rdf')).
:- use_module(bio('thea/owl_reasoner')).

io:redirect_stdout(thea).
io:write_all(thea,F,_):-
        ontol_writer_thea_syntax:write_thea_syntax(F).

io:write_all(thea_dlp,_F,_):-
        debug(ontol,'Writing thea predicates',[]),
        dynpreds(Preds),
        forall((member(P/A,Preds),functor(Term,P,A),Term),
               owl_as2prolog(Term,[no_base(_)])).



dynpreds(X):-
        X=[
           class/5,             % Export all the dynamic ones also
           owl/4,
           subclassOf/2,
           property/7,
           annotationProperty/1,
           differentIndividuals/1,
           sameIndividuals/1,
           ontology/2,
           individual/4,
           disjointSet/1,
           equivalentSet/1
          ].


write_thea_syntax(_File):-
        debug(ontol,'Writing thea predicates',[]),
        %owl_parse(no),
        %owl_pack_ontology,
        %owl_parser:ontology(O,OA,CL,SL,PL,APL,DIL,SIL,IL,DSL,ESL,BNL),
        dynpreds(Preds),
        maplist(write_pred,Preds),
        nl.
        %writeln(ontology(O,OA,CL,SL,PL,APL,DIL,SIL,IL,DSL,ESL,BNL)).
        %portray_clause(ontology(O,OA,CL,SL,PL,APL,DIL,SIL,IL,DSL,ESL,BNL)).

write_pred(P/A):-
        functor(Term,P,A),
        debug(ontol,'  Thea term: ~w',[Term]),
        forall(Term,
               (   writeq(Term),
                   format('.~n'))).


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
