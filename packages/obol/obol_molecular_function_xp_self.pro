:- [bio(obol_obo_xp)]. 

:- multifile molecular_function/3,molecular_function5/3.
:- multifile term_label/3.

term_label(P) --> molecular_function(P). % TODO - DRY

molecular_function(P that regulates(RF)) --> molecular_function5(RF),regulator_activity(P).
molecular_function(P that positively_regulates(RF)) --> molecular_function5(RF),activator_activity(P).
molecular_function(P that negatively_regulates(RF)) --> molecular_function5(RF),inhibitor_activity(P).

% assume activity suffixes have been pre-stripped
regulator_activity(F) --> [regulator],{class_label_exact(F,'molecular_function')}.
activator_activity(F) --> [activator],{class_label_exact(F,'molecular_function')}.
inhibitor_activity(F) --> [inhibitor],{class_label_exact(F,'molecular_function')}.

%regulator_activity(F) --> [regulator],[activity],{class_label_exact(F,'molecular_function')}.
%activator_activity(F) --> [activator],[activity],{class_label_exact(F,'molecular_function')}.
%inhibitor_activity(F) --> [inhibitor],[activity],{class_label_exact(F,'molecular_function')}.

% TODO: co-activator