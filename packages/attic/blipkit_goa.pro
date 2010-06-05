/* -*- Mode: Prolog -*- */


:- module(blipkit_goa,[]).

:- use_module(bio(goa_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(biotable)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(graph)).
:- use_module(bio(blipkit)).

%% GO %%
blipkit:opt_description(assocfile,'Gene association file in standard go_assoc format').

blipkit:main('go-show-minset',
     'Summarise gene products in go_assoc file by minimal cover set',
     [atom(id,ID),
                                %atom([n,name],N),
      atoms([assocfile,'A'],InFiles)],
     InFiles2,
     [bio(ontol_db),bio(biotable)],
     (   % accept assocs as both arg list and as -A option
         (var(InFiles)-> InFiles=InFiles2 ; true),
         debug(load,'Assocfiles: ~w',[InFiles]),
         forall(member(File,InFiles),
                forall(blipkit_goa:show_assoc_minset(ID,File),
                       true)))).

show_assoc_minset(ProdID,F):-
        debug(load,'Connecting to ~w',[F]),
        (   user:bioresource(F,_,_)
        ->  connect_biotable(F,Conn)
        ;   connect_biotable(F,go_assoc,Conn)),
        in_table(Conn,[prodacc(ProdID),
                       prodsymbol(Sym)],_),
        format('Product: ~w ~w~n',[ProdID,Sym]),
        forall(get_assoc_minset(Conn,ProdID,Ont,SpanIDs),
               (   format('   Ont: ~w~n',[Ont]),
                   forall(member(ID,SpanIDs),
                          (   (   class(ID,N)
                              ->  true
                              ;   N='unk'),
                              format('      Class: ~w ~w~n',[ID,N]))))).
        
get_assoc_minset(Conn,ProdID,Ont,SpanIDs):-
        setof(ProdID,
              Pos^in_table(Conn,[prodacc(ProdID)],Pos),
              ProdIDs),
        member(ProdID,ProdIDs),
        writeln(prod(ProdID)),
        setof(ClassID,
               Pos^in_table(Conn,[prodacc(ProdID),
                                  termacc(ClassID)],Pos),
               ClassIDs),
        setof(Ont,ClassID^(member(ClassID,ClassIDs),
                            ontol_db:belongs(ClassID,Ont)),
               Onts),
        member(Ont,Onts),
        writeln(ont(Ont)),
        span_ids_by_ont(Ont,ClassIDs,SpanIDs),
        writeln(Ont-ClassIDs-SpanIDs).

span_ids_by_ont(Ont,AllIDs,SpanIDs):-
        setof(ID,(member(ID,AllIDs),belongs(ID,Ont)),IDs),
        setof(SpanID,
              minimal_spanning_node(closure(ontol_db:parent),IDs,SpanID),
              SpanIDs).
                  
        
blipkit:main('go-list-assocs',
     'Show gene associations by a GO class or similar',
     [atom(id,ID),
      atom([n,name],N),
      atoms([assocfile,'A'],InFiles)],
     InFiles2,
     [bio(ontol_db),bio(biotable)],
     (  (class(ID,N) -> true ; die(no_such_class(ID,N))),
         format('Class [~w] ~w~n',[ID,N]),
         % accept assocs as both arg list and as -A option
         (var(InFiles)-> InFiles=InFiles2 ; true),
         forall(member(File,InFiles),
                blipkit_goa:show_all_assocs_in(ID,File)))).

show_all_assocs_in(ID,F):-
        (user:bioresource(F,_,_)
        ->  connect_biotable(F,Conn)
        ;   connect_biotable(F,go_assoc,Conn)),
        findall(P,(lookup_assoc_by_id(Conn,ID,IDc,[prodacc(P),
                                                   prodsymbol(Sym)]),
                   class(IDc,Nc),
                   format('Product: ~w ~w Annotation: ~w ~w~n',
                          [P,Sym,IDc,Nc])),
                _),
        nl.

lookup_assoc_by_id(Conn,ID,IDc,Spec):-
        setof(IDc,parentRT(IDc,ID),IDcL),
        in_table(Conn,[termacc(IDc)|Spec],_),
        member(IDc,IDcL).


:- blip('go-ic',
        'information content of classes by gp associations',
        [],
        _,
        (   ensure_loaded(bio(goa_db)),
            ensure_loaded(bio(tabling)),
            ensure_loaded(bio(ontol_db)),
            table_pred(ontol_db:parentRT/2),
            table_pred(goa_db:associationRT/5),
            goa_distinct_feature_count(Num),
            forall(belongs(ID,_),
                   show_ic(ID,Num)))).

show_ic(ID,Total):-
        goa_distinct_feature_count_by_term(ID,Num),
        P is Num/Total,
        (   Num=0
        ->  I=inf
        ;   I is -log(P)),
        format('~w ~w~n',[ID,I]).
                   
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2005/10/12 23:57:14 $
  @license LGPL

  ---+ Name
  ---++ blipkit
- simple interface to blip module functionality

  ---+ Description

  this is a submodule of blipkit for handling go_db*/