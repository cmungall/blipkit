:- module(blipkit_pathway,[]).

:- use_module(bio(mode)).
:- use_module(bio(blipkit)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).
:- use_module(bio(dotwriter)).
:- use_module(bio(pathway_db)).
:- use_module(bio(pathway_lookup)).
:- use_module(bio(pathway_writer_dot)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(graph)).
:- use_module(bio(io)).

blipkit:opt_description(show,'show additional info. E.g. -show xrefs. Valid: xrefs, participants, subevents').

blipkit:example('blip -r go -r reactome/Homo_sapiens pathway-viz -n "Mitotic Prometaphase"  -to display',
                'visualize pathway').
blipkit:example('blip -r go -r reactome/Homo_sapiens pathway-viz -id GO:0050796 -show participants -show xrefs -show subevents',
                'textual info on pathway').
blipkit:example('blip -r reactome/Homo_sapiens pathway-viz -n "TCR signaling" -diagram event -to display',
                'vizualize (event-centric)').
blipkit:example('blip -r reactome/Homo_sapiens pathway-viz -n "TCR signaling" -diagram event([role(catalyst),nestp])-to display',
                'vizualize (event-centric)').

blipkit:trusted_command('pathway-viz').
user:opt_insecure(query).
:- blip('pathway-viz',
        'vizualizes a pathway',
        [atoms([id],QueryIDsIn),
         atom([ids,idlist],QueryIDsAtom),
         atom([idfile],QueryIDsFile),
         atoms([name,n],QueryNames),
         atom([to,t],OutFmt,text),
         atoms(constraint,Constraints),
         term([diagram,dt],Diagram,full),
         atoms([show,s],ShowList),
         atom(query,OntolQueryAtom)
        ],
        FileL,
        (   load_factfiles(FileL),
            findall(show(S),member(S,ShowList),Opts),
            (   Constraints=[]
            ->  true
            ;   ensure_loaded(bio(ontol_vizlayout)),
                forall(member(ConstraintF,Constraints),
                       load_viz_constraints(bio(ConstraintF)))),
            query_pathways([ids(QueryIDsIn),
			    idsatom(QueryIDsAtom),
			    names(QueryNames),
			    idfile(QueryIDsFile),
			    query(OntolQueryAtom)],
			   PIDs,
			   NoIDs),
            forall(member(NoID,NoIDs),
                   format(user_error,'No match: ~w~n',[NoID])),
            show_pathways(PIDs,OutFmt,[diagram(Diagram)|Opts]))).

        
% DISPLAY

show_pathways(PIDs,Fmt,Opts) :-
        forall(member(PID,PIDs),
               show_pathway(PID,Fmt,Opts)).

show_pathway(PID,text,Opts) :-
        !,
        format(user_error,':: Showing ~w~n',[PID]),
        path_toks(PID,Opts,Toks,[]),
        concat_atom(Toks,A),
        writeln(A).

show_pathway(P,dot,Opts) :-
        !,
        member(diagram(Diagram),Opts),
        pathway_to_dotgraph(P,Diagram,GX,Opts),
        graph_to_dot_atom(GX,A),
        writeln(A).

show_pathway(P,display,Opts) :-
        !,
        member(diagram(Diagram),Opts),
        pathway_to_dotgraph(P,Diagram,GX,Opts),
        graph_display(GX,open).
        
path_toks(PID,Opts) -->
        [PID],
        name_toks(PID),
        xrefs_toks(PID,Opts),
        participants_toks(PID,Opts),
        subevents_toks(PID,Opts).

name_toks(PID) --> {entity_label(PID,N)},[' ! '],[N],!.
name_toks(_) --> [].

participants_toks(PID,Opts) -->
        {member(show(participants),Opts)},
        !,
        {solutions(R-C,event_participating_continuant_roleT(PID,C,R),RCs)},
        prlist_toks(RCs,Opts).
participants_toks(_,_) --> [].

subevents_toks(PID,Opts) -->
        {member(show(subevents),Opts)},
        !,
        subevents_recursive_toks(PID,Opts,'  ').
subevents_toks(_,_) --> [].

subevents_recursive_toks([],_,_) --> !,[].
subevents_recursive_toks([PID|PIDs],Opts,Tab) -->
        !,
        subevents_recursive_toks(PID,Opts,Tab),
        subevents_recursive_toks(PIDs,Opts,Tab).
subevents_recursive_toks(PID,Opts,Tab) -->
        !,
        [Tab],
        [PID],
        name_toks(PID),
        xrefs_toks(PID,Opts),
        participants_toks(PID,Opts),
        nl,
        {atom_concat('  ',Tab,Tab2)},
        {(   setof(P2,subpathway_of(P2,PID),Ps)
         ->  true
         ;   Ps=[])},
        subevents_recursive_toks(Ps,Opts,Tab2).

prlist_toks([],_) --> [].
prlist_toks([R-C|Cs],Opts) --> role(R),[':'],e_toks(C,Opts),!,prlist_toks(Cs,Opts).
e_toks(C,Opts) --> [C],name_toks(C),xrefs_toks(C,Opts),nl.
role(complex(R)) --> !,['complex-'],role(R).
role(R) --> [R].


xrefs_toks(E,Opts) -->
        {member(show(xrefs),Opts),
         solutions(X,entity_xref(E,X),Xs),
         solutions(X,(event_catalyst(E,_,C),
		      entity_xref(C,X))
		  ,Xs2)},
        !,
        flist_toks(Xs),
        flist_toks(Xs2).
xrefs_toks(_,_) --> [].


flist_toks([]) --> [].
flist_toks([C|Cs]) --> f_toks(C),!,flist_toks(Cs).
f_toks(C) --> [' <',C],name_toks(C),['> '].


nl --> {sformat(S,'\n')},[S].


        
