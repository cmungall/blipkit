:- [bio(obol_cellular_component_xp)].

:- multifile term_label/3,cellular_component/3.


% B cell receptor complex
cellular_component(CC that has_part(Pro)) --> protein(Pro),protein_complex(CC). % 
protein_complex(CC) --> [CC],{class_label_exact(CC,'protein complex')}.

cellular_component(CC that has_part(Pro)) --> protein(Pro),cellular_component(CC).
