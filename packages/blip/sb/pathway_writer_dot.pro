/* -*- Mode: Prolog -*- */

:- module(pathway_writer_dot,[
                              pathway_to_dotgraph/4
                           ]).
:- use_module(bio(metadata_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(interaction_db)).
:- use_module(bio(dotwriter)).
:- use_module(bio(bioprolog_util)).

:- multifile system:image_display_exec/1.
:- dynamic system:image_display_exec/1.

:- multifile system:graphviz_pathway_param/2.
:- multifile system:graphviz_pathway_param/3.
% todo - move to conf file

node_shape(ID,diamond):- event(ID),!.
node_shape(ID,box):- is_complex(ID),!.
node_shape(ID,hexagon):- is_small_molecule(ID),!.
node_shape(_,oval).

% Opts arg is optional
system:graphviz_pathway_param(Template,Param,_):- 
        system:graphviz_pathway_param(Template,Param).

system:graphviz_pathway_param(node(ID),label=NF,Opts):-
	member(xreflabel(IDSpace),Opts),
	event_xref(ID,X),
	id_idspace(X,IDSpace),
	!,
	system:graphviz_pathway_param(node(ID),label=N1),
	system:graphviz_pathway_param(node(X),label=N2),
	concat_atom([N1,'\\n[',X,' ',N2,']'],N),
	format_label(N,NF).

system:graphviz_pathway_param(node(ID),label=N2):- entity_label(ID,N), !, format_label(N,N2).
system:graphviz_pathway_param(node(ID),label=ID):- !.
system:graphviz_pathway_param(edge(_,_,R),label=R).
        
% see http://www.graphviz.org/doc/info/attrs.html
%system:graphviz_pathway_param(graph,prop(style=filled)).
%system:graphviz_pathway_param(node(_),fillcolor=red).
system:graphviz_pathway_param(node(ID),shape=Shape):- node_shape(ID,Shape).
system:graphviz_pathway_param(node(_),fontname=helvetica).
%system:graphviz_pathway_param(node(_),fontname='/System/Library/Fonts/Courier.dfont'). % OS X only!!
system:graphviz_pathway_param(node(_),fontsize=14).
system:graphviz_pathway_param(edge(_,_,input),arrowhead=none).
system:graphviz_pathway_param(edge(_,_,input),arrowtail=normal).
system:graphviz_pathway_param(edge(_,_,input),color=green).
system:graphviz_pathway_param(edge(_,_,output),arrowhead=normal).
system:graphviz_pathway_param(edge(_,_,output),arrowtail=none).
system:graphviz_pathway_param(edge(_,_,output),color=blue).
system:graphviz_pathway_param(edge(_,_,catalyst),arrowhead=none).
system:graphviz_pathway_param(edge(_,_,catalyst),arrowtail=box).
system:graphviz_pathway_param(edge(_,_,catalyst),color=red).

system:graphviz_pathway_param(edge(_,_,subpathway_of),color=grey).
system:graphviz_pathway_param(edge(_,_,followed_by),color=blue).
system:graphviz_pathway_param(edge(_,_,followed_by),arrowtail=crow).

%% pathway_to_dotgraph(+P,+Diagram,?Graph,+Opts)
% Graph can be used in dotwriter
%
% Diagram=full
%  * shows everything
%  * nested complexes
%
% Diagram=event
% Diagram=event(Opts)
%  * event-centric
%  * optionally shows participants
%  * Opt=nestp - nests subevent_of/2
%  * Opt=role(Role) - shows participants that play role. E.g. Role=catalyst

pathway_to_dotgraph(P,full,GX,Opts):-
        setof(E,has_subeventRT(P,E),Es),
        debug(dot,'events= ~w',[Es]),
        setof(M,R^event_participant_roleT(P,M,R),Ms),
        debug(dot,'snaps= ~w',[Ms]),
        setof(Loc,M^(member(M,Ms),located_in(M,Loc)),Locs),
        debug(dot,'locs= ~w',[Locs]),
        %setof(C,M^(member(M,Ms),snapshot_continuant(M,C)),Cs),
        findall(Part-Whole,
                (   member(Part,Ms),
                    has_part(Whole,Part)),
                NestPairs1),
        findall(Part-Whole,
                (   member(Part,Ms),
                    \+ has_part(_,Part), % roots only
                    located_in(Part,Whole)),
                NestPairs2),
        append(NestPairs1,NestPairs2,NestPairs),
        %debug(dot,'nps= ~w',[NestPairs]),
        findall(Term,
                (   member(ID,Es),
                    findall(Param,system:graphviz_pathway_param(node(ID),Param,Opts),NodeParams),
                    Term=node(ID,NodeParams)),
                EventNodeTerms),
        findall(Term,
                (   member(ID,Ms),
                    findall(Param,system:graphviz_pathway_param(node(ID),Param,Opts),NodeParams),
                    Term=node(ID,NodeParams)),
                SnapNodeTerms),
        findall(Term,
                (   member(ID,Locs),
                    findall(Param,system:graphviz_pathway_param(node(ID),Param,Opts),NodeParams),
                    Term=node(ID,NodeParams)),
                LocNodeTerms),
        findall(edge(M1,M2,EdgeParams),
                (   member(M1,Ms),
                    snapshot_continuant(M1,C1),
                    interacts_with(C1,C2),
                    snapshot_continuant(M2,C2),
                    member(M2,Ms),
                    findall(Param,system:graphviz_pathway_param(edge(M1,M2,interactions),Param,Opts),EdgeParams)),
                InteractionEdgeTerms),
        findall(edge(E,M,EdgeParams),
                (   member(E,Es),
                    event_participant_role(E,M,Role),
                    member(M,Ms),
                    findall(Param,system:graphviz_pathway_param(edge(E,M,Role),Param,Opts),EdgeParams)),
                EMEdgeTerms),
        findall(edge(E,E2,EdgeParams),
                (   member(E,Es),
                    subpathway_of(E,E2),
                    findall(Param,system:graphviz_pathway_param(edge(E,M,subpathway_of),Param,Opts),EdgeParams)),
                SubEventEdgeTerms),
        flatten([EventNodeTerms,SnapNodeTerms,LocNodeTerms,InteractionEdgeTerms,EMEdgeTerms,SubEventEdgeTerms],AllTerms),
        G=graph(g,[],AllTerms),
        graph_nest_pairs(G,GX,NestPairs),
        !.

% events only
pathway_to_dotgraph(P,event,GX,Opts) :-
        pathway_to_dotgraph(P,event([]),GX,Opts).
pathway_to_dotgraph(P,event(Opts),GX,Opts) :-
        \+ is_list(Opts),   % we allow e.g. event(c)
        !,
        rterm_to_list(Opts,OptsL),
        pathway_to_dotgraph(P,event(OptsL),GX,Opts).
pathway_to_dotgraph(P,event(Opts),GX,_Opts) :-
	member(nestp,Opts),
	!,
	% NEST SUB-EVENTS
        setof(E,has_subeventRT(P,E),Es),
        debug(dot,'events= ~w',[Es]),
	findall(E-E2,
                (   member(E,Es),
                    subpathway_of(E,E2),
                    member(E2,Es)),
                NestPairs),
	% OPTS
        collect_terms(continuant,Es,CTerms,Opts),
	% MAIN NODES ARE EVENTS
        findall(Term,
                (   member(ID,Es),
                    findall(Param,system:graphviz_pathway_param(node(ID),Param,Opts),NodeParams),
                    Term=node(ID,NodeParams)),
                EventNodeTerms),
	% TEMPORAL
        findall(edge(E2,E,EdgeParams),
                (   member(E,Es),
                    preceded_by(E,E2),
                    member(E2,Es),
                    findall(Param,system:graphviz_pathway_param(edge(E,E2,followed_by),Param,Opts),EdgeParams)),
                TemporalEdgeTerms),
        flatten([CTerms,EventNodeTerms,TemporalEdgeTerms],AllTerms),
        G=graph(g,[],AllTerms),
        graph_nest_pairs(G,GX,NestPairs).
pathway_to_dotgraph(P,event(Opts),GX,_Opts) :-
	% do not nest
        setof(E,has_subeventRT(P,E),Es),
        debug(dot,'events= ~w',[Es]),
        NestPairs=[],
        collect_terms(continuant,Es,CTerms,Opts),
        findall(Term,
                (   member(ID,Es),
                    findall(Param,system:graphviz_pathway_param(node(ID),Param,Opts),NodeParams),
                    Term=node(ID,NodeParams)),
                EventNodeTerms),
        findall(edge(E,E2,EdgeParams),
                (   member(E,Es),
                    subpathway_of(E,E2),
                    member(E2,Es),
                    findall(Param,system:graphviz_pathway_param(edge(E,E2,subpathway_of),Param,Opts),EdgeParams)),
                SubEventEdgeTerms),
        findall(edge(E2,E,EdgeParams),
                (   member(E,Es),
                    preceded_by(E,E2),
                    member(E2,Es),
                    findall(Param,system:graphviz_pathway_param(edge(E,E2,followed_by),Param,Opts),EdgeParams)),
                TemporalEdgeTerms),
        flatten([CTerms,EventNodeTerms,SubEventEdgeTerms,TemporalEdgeTerms],AllTerms),
        G=graph(g,[],AllTerms),
        graph_nest_pairs(G,GX,NestPairs),
        !.


% e.g. 163924 - complexes. use their parts?
snapshot_continuant_guess(M,C) :- snapshot_continuant(M,C).
snapshot_continuant_guess(M,M) :-
        \+ snapshot_continuant(M,_),
        is_complex(M).


collect_terms(continuant,Es,CTerms,Opts) :-
        member(role(R),Opts),
        setof(C,E^M^R^(member(E,Es),event_participant_role(E,M,R),snapshot_continuant_guess(M,C)),Cs),        
        !,
        findall(Term,
                (   member(ID,Cs),
                    findall(Param,system:graphviz_pathway_param(node(ID),Param,Opts),NodeParams),
                    Term=node(ID,NodeParams)),
                CNodeTerms),
        solutions(edge(E,C,EdgeParams),
                (   member(E,Es),
                    event_participant_role(E,M,Role),
                    snapshot_continuant_guess(M,C),
                    member(C,Cs),
                    findall(Param,system:graphviz_pathway_param(edge(E,C,Role),Param,Opts),EdgeParams)),
                CEdgeTerms),
        append(CNodeTerms,CEdgeTerms,CTerms).
        
collect_terms(continuant,_,[],_).

rterm_to_list(A+B,[B|L]) :-
        !,
        rterm_to_list(A,L).
rterm_to_list(A,[A]).
        
%format_label(N,N) :- !.
format_label(N,N2) :-
        concat_atom(Toks,' ',N),
        concat_atom(Toks,'\\n',N2).


/** <module> generate DOT/graphviz from pathways

---+ Synopsis
  
==
blip -r reactome/Homo_sapiens pathway-viz -n 'TCR signaling' -to display -diagram event
==

---+ Details




*/
