:- [bio(obol_cellular_component_xp)].

:- multifile cellular_component/3.
:- multifile cellular_component5/3.

% WATCH OUT FOR CIRCULARITIES

% virion transport vesicle
% TODO: weight this so it has priority over false+ transport_vesicle that part_of virion
cellular_component(CC that executes(P)) --> process10(P),cellular_component(CC).

% endocytic vesicle
cellular_component(CC that derives_from(PM)) --> [endocytic],cellular_component(CC),{class_label_exact(PM,'plasma membrane')}.

% mRNA editing complex
cellular_component(CC that executes(P)) --> process10(P),[complex],{class_label_exact(CC,'macromolecular complex')}.




