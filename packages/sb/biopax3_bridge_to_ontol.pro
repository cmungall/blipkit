:- module(biopax3_bridge_to_ontol,[]).

%% @deprecated - instead biopax -> pathway -> ontol

:- use_module(bio(biopax3_db)).
%:- use_module(bio(biopax3_rules)).
:- use_module(bio(ontol_db)).

% THIS IS IDENTICAL TO biopax2_bridge_to_ontol. DRY??

ontol_db:class(X):- pathway(X).  % process
ontol_db:class(X):- biochemicalReaction(X). % process or MF?
ontol_db:class(X):- catalysis(X). % function
metadata_db:entity_label(E,L):- E name L.
metadata_db:entity_xref(E,X):- oboxref(E,X).
metadata_db:entity_comment(E,X):- biopax3_db:comment(E, X).
metadata_db:entity_resource(E,reactome):- pathway(E).



%ontol_db:restriction(X,part_of,Y):- pathway(X), Y has_subpathway X. % P to P
ontol_db:subclass(X,Y):- pathway(X), Y has_subpathway X. % P to P

%ontol_db:restriction(Path,executes,Func):- Path pathway_components Step, next_stepRT(Step,Step2), step_interactions(Step2,Func).
ontol_db:restriction(Path,executes,Func):- Path pathway_components Step, step_interactions(Step,Func).

ontol_db:restriction(P2,preceded_by,P1):- PS1 stepProcess P1, PS1 nextStep PS2, PS2 stepProcess P2.

ontol_db:class(X):- pathwayStep(X).
