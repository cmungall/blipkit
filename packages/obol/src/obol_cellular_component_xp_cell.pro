:- [bio(obol_cellular_component_xp)].

:- multifile term_label/3,cellular_component/3,receptor_complex/3.

%% ?need to be more specific, or this has false -ves, eg: B cell receptor complex
%% cellular_component(CC that part_of(Cell)) --> cell(Cell),cellular_component(CC).

% B cell receptor complex
cellular_component(CC that part_of(Cell)) --> cell(Cell),receptor_complex(CC). % 
receptor_complex(CC) --> any_kind_of(CC,'receptor complex').

% female germ cell nucleus
% egg plasma membrane
cellular_component(CC that part_of(Cell)) --> cell(Cell),cellular_component(CC). % 


