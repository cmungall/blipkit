:- module(pathway_go_util,
	  [
	   event_goxref/2
	   ]).

:- use_module(bio(metadata_db)).
:- use_module(pathway_db).

%% event_goxref(?P,?GOClass)
%
event_goxref(P,GOID) :-
        entity_xref(P,GOID),
        id_idspace(GOID,'GO').
event_goxref(P,GOID) :-
        event_catalyst(P,_,C),
        entity_xref(C,GOID),
        id_idspace(GOID,'GO').
% humancyc biopax export does not have go xrefs - use the xrefs in GO itself
event_goxref(P,GOID) :-
	entity_xref(P,PX),
	id_idspace(PX,'HUMANCYC'),
	id_localid(PX,CYC_ID),
	% switch humancyc->metacyc
	concat_atom(['MetaCyc',CYC_ID],':',XrefInGO),
	entity_xref(GOID,XrefInGO).
event_goxref(Interaction,GOID) :-
	event_catalyst(P,_,Interaction,catalyst),
	entity_xref(P,PX),
	id_idspace(PX,'HUMANCYC'),
	id_localid(PX,CYC_ID),
	% switch humancyc->metacyc
	concat_atom(['MetaCyc',CYC_ID],':',XrefInGO),
	entity_xref(GOID,XrefInGO).
