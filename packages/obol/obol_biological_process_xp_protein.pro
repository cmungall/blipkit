:- [bio(obol_biological_process_xp_anatomy)].
:- [bio(obol_biological_process_xp_cellular_component)].

:- multifile process/3,process5/3,anatomical_continuant/3,cell/3,protein/3.

% +++++ Level5 +++++

% cullin deneddylation [DEF: "The modification of cullins by removal of ubiquitin-like protein NEDD8 (RUB1)."]
process5(P that results_in_removal_of(NEDD) and results_in_removal_from(Prot)) --> protein(Prot),[deneddylation],{class_label_exact(P,'protein modification process'),class_label_exact(NEDD,'NEDD8')}.

protein(Prot) --> terminal(Prot).

