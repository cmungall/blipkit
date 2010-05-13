:- [bio(obol_obo_xp)].
:- [bio(obol_anatomy_xp)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_quality_xp)].
% TODO: REINTRODUCE THESE
%:- [bio(obol_biological_process_xp_anatomy)]. % required for class expressions for processes; eg abnormal osteoclast formation
%:- [bio(obol_biological_process_xp_cell)]. % required for class expressions for processes; eg abnormal osteoclast formation

:- multifile term_label/3.
:- multifile bearer/3.
:- multifile gross_anatomical/3.
:- multifile phenotype/3.
term_label(P) --> phenotype(P).

% long mandible
phenotype(Q that inheres_in(B)) --> quality(Q),bearer(B).

bearer(B) --> continuant(B).
bearer(B) --> process(B).

