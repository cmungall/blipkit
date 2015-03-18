/* -*- Mode: Prolog -*- */


:- module(blipkit_sb,[]).

:- use_module(bio(blipkit)).
:- use_module(bio(sb_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(lego_db)).
:- use_module(bio(ontol_db)).

opt_description(distance,'Maximum distance to search').

:- blip('sb-simulate',
        'simulate',
        [],
        _,
     (   
         ensure_loaded(bio(sb_simulator)),
         simulate)).


blipkit:main('sb-query',
     'Basic queries on sb entities: by species, component, reaction ID',
     [atom([id],ID),
      atom([name,n],N),
      number([distance,dist],Dist,1)],
     FileL,
     [bio(sb_db)],
     (   load_factfiles(FileL),
         findall(ID,
                 (   sb_entity(ID,N),
                     blipkit_sb:sb_query(ID,0,Dist)),
                 IDs),
         (IDs=[]->write(user_error,no_data(ID,N));true))).

sb_query(_ID,Indent,MaxDist):-
        Indent >= MaxDist,
        !.
sb_query(ID,Indent,MaxDist):-
        sb_entity(ID,_,_),
        writetab(Indent),
        Indent2 is Indent+1,
        sb_query1(ID,Indent2,MaxDist).

sb_query1(ID,Indent,MaxDist):-
        reaction(ID,N),
        format('reaction: ~w ~w~n',[ID,N]),
        forall(reaction_rel_continuant(ID,T,CID),
               (   writetab(Indent),
                   format('[~w has ~w ~w]~n',[ID,T,CID]),
                   sb_query(CID,Indent,MaxDist))),
        nl.

sb_query1(ID,Indent,MaxDist):-
        species(ID,N,CompID),
        format('species: ~w ~w in: ~w~n',[ID,N,CompID]),
        forall(reaction_rel_continuant(RID,T,ID),
               (   writetab(Indent),
                   format(' [~w plays role ~w in reaction ~w]~n',[ID,T,RID]),
                   sb_query(RID,Indent,MaxDist))),
        nl.

sb_query1(ID,Indent,MaxDist):-
        compartment(ID,N),
        format('compartment: ~w ~w~n',[ID,N]),
        forall(species(SID,_,ID),
               (   writetab(Indent),
                   format('[~w contains ~w]~n',[ID,SID]),
                   sb_query(SID,Indent,MaxDist))),
        nl.

blipkit:main('sb-query-by-annot',
     'As sb-query; id or name is for a class in some ontology used to annotate s/c/r',
     [atom([id],ID),
      atom([name,n],N),
      number([distance,dist],Dist,1)],
     FileL,
     [bio(sb_db),bio(ontol_db)],
     (   load_factfiles(FileL),
         (class(ID,N) -> true ; die(no_such_class(ID,N))),
         format('Class [~w] ~w~n',[ID,N]),
         findall(EntityID,
                 (   entity_class(EntityID,AnnotClassID),
                     parentRT(AnnotClassID,ID),
                     blipkit_sb:sb_query(EntityID,0,Dist)),
                 EntityIDs),
         (EntityIDs=[]->write(user_error,no_data(ID,N));true))).


blipkit:main('sb-summarize',
     'Summarise a sb_db file or dataset (try running on a SBML file)',
     [],
     FileL,
     [bio(ontol_db),bio(sb_db)],
     (   nb_getval(format,Format),
         findall(File,(member(File,FileL),
                       load_biofile(Format,File)),_),
         print_message(informational,m(summarizing_models)),
         blipkit_sb:sb_summarize_pred(ID,model(ID,_)),
         blipkit_sb:sb_summarize_pred(ID,compartment(ID,_)),
         blipkit_sb:sb_summarize_pred(ID,species(ID,_,_)),
         blipkit_sb:sb_summarize_pred(ID,reaction(ID,_)))).

sb_summarize_pred(ID,Goal):-
        findall(ID,Goal,IDs),
        setof(ClassID,ID^(member(ID,IDs),entity_class(ID,ClassID)),ClassIDs),
        !,
        Goal =.. [Pred|_],
        forall(member(ClassID,ClassIDs),
               (   format(' Entity: [~w] ClassID: [~w]',[Pred,ClassID]),
                   ((class(ClassID,N),belongs(ClassID,Ont))
                   ->  format(' Name: [~w] in ~w~n',[N,Ont])
                   ;   nl))).
sb_summarize_pred(_,Goal):-
        Goal =.. [Pred|_],
        format('NO CLASSES FOR ~w~n',Pred).
