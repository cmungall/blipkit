:- [bio(obol_cellular_component_xp)].
%:- [bio(obol_molecular_function_xp)]. 

:- multifile cellular_component/3.
:- multifile cellular_component5/3.

% X complex
%cellular_component(CC that has_function(F),In,Rest):-
def(cellular_component(CC that has_funtion(F))) --> ['A'],terminal(CC),['that possesses'],terminal(F).

%cellular_component(CC that has_function(Function)) --> molecular_function(Function),rcomplex(CC).

% voltage-gated sodium channel complex = macromolecular complex that has_function voltage-gated sodium channel activity
% relies on abbreviated form being present...
cellular_component(CC that has_function(Function)) --> molecular_function(Function),rcomplex(CC).
rcomplex(CC) --> [complex],{class_label_exact(CC,'macromolecular complex')}.

% duplicate...?
% LOOP cellular_component(CC that has_function(F that binds_to(C))) --> continuant(C),receptor_complex(CC),{class_label_exact(F,'binding activity')}.
%receptor_complex(CC) --> any_kind_of(CC,'receptor complex').


