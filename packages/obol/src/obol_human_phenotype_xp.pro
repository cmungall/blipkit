:- [bio(obol_phenotype_xp)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_anatomy_xp)].
:- multifile phenotype/3.
:- multifile quality/3.
:- multifile bearer/3.

morphological(M) --> [morphological],{class_label_exact(M,morphology)}.

%phenotype(P) --> [complete],phenotype(P). % todo - qualifier? THIS LEADS TO CYCLES IN GENERATION

phenotype(Q that inheres_in(B)) --> quality(Q),[of],[the],bearer(B).
phenotype(Q that inheres_in_part_of(B)) --> quality(Q),[involving],[the],bearer(B).
phenotype(Q that inheres_in_part_of(B that part_of(W))) --> quality(Q),bearer(B),[involving],[the],bearer(W).
phenotype(Q that inheres_in_part_of(B that part_of(W))) --> quality(Q),[involving],[the],bearer(B),[of],[the],bearer(W).

% abnormalities of ...
phenotype(P and qualifier(A)) --> abnormal(A),[of],phenotype(P).
% abnormality of the ..
phenotype(Q that inheres_in_part_of(B) and qualifier(A)) --> abnormal(A),[of,the],bearer(B),{class_label_exact(Q,quality)}.
phenotype(Q that inheres_in_part_of(B) and qualifier(A)) --> abnormal(A),[of],bearer(B),{class_label_exact(Q,quality)}.

% morphological abnormalities of the
phenotype(Q that inheres_in_part_of(B) and qualifier(A)) --> morphological(Q),abnormal(A),[of,the],bearer(B).

% HP:0010647 ! Abnormality of skin texture
phenotype(Q that inheres_in(B) and qualifier(A)) --> abnormal(A),[of],bearer(B),quality(Q).


phenotype(P that inheres_in(B)) --> quality(P),body_qualifier(B).

% abnormal number of teeth
% phenotype(P and qualifier(A)) --> abnormal(A),[number],[of],bearer(B).

% aplasia/hypoplasia of the fibula
sq(Q) --> [aplasia],['/'],[hypoplasia],{class_label_exact(Q,'decreased size')}.
sq(Q) --> [atrophy],['/'],[degeneration],{class_label_exact(Q,atrophied)}.

%  Exostoses (humeral)
phenotype(Q that inheres_in(E)) --> quality(Q),['('],[A],[')'],{relational_adj_ra(A,E,anatomy)}.

phenotype(Q that inheres_in(B)) --> sq(Q),[of],[the],bearer(B).
phenotype(Q that inheres_in_part_of(B)) --> sq(Q),[involving],[the],bearer(B).
phenotype(Q that inheres_in_part_of(B)) --> sq(Q),[affecting],[the],bearer(B).
phenotype(Q that inheres_in_part_of(B that part_of(W))) --> sq(Q),[involving],[the],bearer(B),[of],[the],bearer(W).

% HP:0001221 ! Short, thick distal phalanges
%phenotype(Q that has_part(Q1 that inheres_in(E)) and has_part(Q2 that inheres_in(E))) -->
%        quality(Q1),[','],quality(Q2),bearer(E),{class_label_exact(Q,quality)}.
%phenotype(Q that has_part(Q1 that inheres_in(E)) and has_part(Q2 that inheres_in(E))) -->
%        quality(Q1),quality(Q2),bearer(E),{class_label_exact(Q,quality)}.

% cerebral hemorrhage

body_qualifier(B) --> [','],[male],{class_label_exact(B,'male body')}.
body_qualifier(B) --> [','],[female],{class_label_exact(B,'female body')}.

%quality(Q) --> [W],{entity_synonym(Q,W),belongs(Q,quality)}.
quality(Q) --> terminal(Q),{belongs(Q,medical_genetics)}.

bearer(B) --> [plasma],{class_label_exact(B,'blood')}.
