:- module(biopax2_bridge_to_ontol,[]).

%% @deprecated - instead biopax -> pathway -> ontol

:- use_module(bio(biopax2_db)).
:- use_module(bio(biopax2_rules)).
:- use_module(bio(ontol_db)).

ontol_db:class(X):- pathway(X).  % process
ontol_db:class(X):- catalysis(X). % function
metadata_db:entity_label(E,L):- E biopax_name L.
metadata_db:entity_synonym(E,L):- E short_name L.
metadata_db:entity_xref(E,X):- oboxref(E,X).
metadata_db:entity_comment(E,X):- E biopax_comment X.
metadata_db:entity_resource(E,reactome):- pathway(E).
%ontol_db:restriction(X,part_of,Y):- pathway(X), Y has_subpathway X. % P to P
ontol_db:subclass(X,Y):- pathway(X), Y has_subpathway X. % P to P

%ontol_db:restriction(Path,executes,Func):- Path pathway_components Step, next_stepRT(Step,Step2), step_interactions(Step2,Func).
ontol_db:restriction(Path,executes,Func):- Path pathway_components Step, step_interactions(Step,Func).

% contextual..
ontol_db:restriction(F2,preceded_by,F1):- F1 next_step F2.

ontol_db:class(X):- pathwayStep(X).
