:- [bio(obol_phenotype_xp)].
:- [bio(obol_phenotype_attribute_value)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_anatomy_xp)].

:- multifile phenotype/3.
:- multifile quality/3.

% tau protein deposits
phenotype(Q that inheres_in(C)) --> continuant(C),{class_label_exact(Q,'aggregated')}.

% corneal deposits
phenotype(Q that inheres_in_part_of(C)) --> anatomy_relational_adj(C),{class_label_exact(Q,'aggregated')}.

% patterns specific to MP grammar:
% (we separate many of these because they slow down processing)

% absent eyelids
% phenotype(Q that towards(P)) --> [absent],[Plural],{nonvar(Plural),atom_concat(Singular,s,Plural),class_label_exact(P,Singular),class_label_exact(Q,'lacks all physical parts of type')}.
% now in xp_plurals

phenotype(Q that towards(C)) --> [absent],continuant(C),{class_label_exact(Q,'lacks all physical parts of type')}.


% increased susceptibility to X
phenotype(Q that towards(C)) --> [Magn],quality(BaseQ),[to],continuant(C),{magnitude_attribute_quality(Magn,BaseQ,Q)}.

% hippocampus pyramidal cell layer (see MP:0008284)
% TODO: check - this one is a but dubious...
% better do do this using spatial?
%gross_anatomical(P that part_of(W)) --> gross_anatomical5(W),gross_anatomical(P),[layer].
% retinal ganglion layer
%gross_anatomical(P that part_of(W)) --> anatomy_relational_adj(W),gross_anatomical(P),[layer].

% abnormal hippocampus CA4 region morphology
%gross_anatomical(C) --> gross_anatomical5(C),[region].

quality(Q) --> [morphology], [or], [physiology],{class_label_exact(Q,quality)}.

%% ----------------------------------------
%% relational qualities: relative number (concentration)
%% ----------------------------------------

% TODO:
% fusion of arytenoid and cricoid cartilages

% high X levels
phenotype(Q that towards(C)) --> concentration(Q),bearer(C),levels.

% increased circulating hormone level
phenotype(Q that towards(C) and inheres_in(B)) --> concentration(Q),in_blood(B),bearer(C),levels.
phenotype(Q that towards(C) and inheres_in(B)) --> concentration(Q),bearer(B),bearer(C),levels.
% abnormal X levels
phenotype(Q that towards(C) and qualifier(A)) --> abnormal(A),bearer(C),levels,{class_label_exact(Q,'concentration')}.
phenotype(Q that towards(C) and inheres_in(B) and qualifier(A)) --> abnormal(A),in_blood(B),bearer(C),levels,{class_label_exact(Q,'concentration')}.
phenotype(Q that towards(C) and inheres_in(B) and qualifier(A)) --> abnormal(A),bearer(B),bearer(C),levels,{class_label_exact(Q,'concentration')}.

concentration(Q) --> any_kind_of(Q,'increased concentration').
concentration(Q) --> [increased],{class_label_exact(Q,'increased concentration')}.
concentration(Q) --> [increased],[percent],{class_label_exact(Q,'increased concentration')}.
concentration(Q) --> any_kind_of(Q,'decreased concentration').
concentration(Q) --> [decreased],[percent],{class_label_exact(Q,'decreased concentration')}.
concentration(Q) --> [decreased],{class_label_exact(Q,'decreased concentration')}.


in_blood(B) --> [circulating],{class_label_exact(B,blood)}.

levels --> [level].
levels --> [levels]. 

% skin inflammation
phenotype(Q that inheres_in(B) and towards(P)) --> bearer(B),inflammation(P),{class_label_exact(Q,'increased rate')}.
inflammation(P) --> [inflammation],{class_label_exact(P,'inflammatory response')}.

