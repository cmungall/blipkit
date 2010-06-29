:- [bio(obol_obo_xp)]. 

:- multifile
        cellular_component5/3,
        anatomical_continuant/3,
        term_label/3,
        term_textdef/3.

term_label(CC) --> cellular_component(CC).
term_textdef(CC) --> def(cellular_component(CC)).


% condensed chromosome kinetochore
cellular_component(Part that part_of(Whole)) --> cellular_component5(Whole),cellular_component(Part).

% kinetochore of condensed chromosome
cellular_component(Part that part_of(Whole)) --> cellular_component5(Part),[of],cellular_component(Whole).

% lytic vacuole within protein storage vacuole
cellular_component(Part that part_of(Whole)) --> cellular_component5(Part),[within],cellular_component(Whole).

% neurofilament cytoskeleton (note reversal: composed of)
cellular_component(Whole that has_part(Part)) --> cellular_component5(Part),cellular_component(Whole).


% virion part
cellular_component(CC that part_of(Whole)) --> cellular_component5(Whole),[part],{class_label_exact(CC,'cellular_component')}.

% nuclear part
cellular_component(CC that part_of(Whole)) --> cc_relational_adj(Whole),[part],{class_label_exact(CC,'cellular_component')}.

def(cellular_component(Whole) that part_of(Part)) --> ['A'],def(Whole),['that is part of a'],def(Part).

% coats?

% intracellular membrane-bound organelle
cellular_component5(Whole that contained_by(Cell)) --> [intracellular],cellular_component(Whole),{class_label_exact(Cell,'cell')}.

% inner kinetochore of condensed chromosome : ???
cellular_component5(Part that inner_part_of(Whole)) --> [inner],cellular_component(Whole),{class_label_exact(Part,'cell part')}.

% intrinsic to peroxisomal membrane
% note: this is more of a location than a cellular component
cellular_component5(CC that located_in(Membrane)) --> [intrinsic],[to],cellular_component(Membrane),{class_label_exact(CC,'intrinsic to membrane')}.
def(cellular_component(CC that located_in(Part))) --> {class(CC,'intrinsic to membrane')},['Located in the'],def(Part),['such that some covalently attached portion of the gene product spans or is embedded in one or both leaflets of the membrane'].

% integral to peroxisomal membrane
% note: this is more of a location than a cellular component
cellular_component5(CC that located_in(Membrane)) --> [integral],[to],cellular_component(Membrane),{class_label_exact(CC,'integral to membrane')}.
def(cellular_component(CC that located_in(Part))) --> {class(CC,'integral to membrane')},['Penetrating at least one phospholipid bilayer of the'],def(Part),['May also refer to the state of being buried in the bilayer with no exposure outside the bilayer'].


% nuclear envelope - TODO - special case?
%cellular_component5(Part that part_of(Whole)) --> cc_relational_adj(Whole),cellular_component(Part).
% cytoplasmic ubiquitin ligase complex
cellular_component5(Part that surrounded_by(Whole)) --> cc_relational_adj(Whole),cellular_component(Part).

%% SURROUNDS

% vacuolar lumen
cellular_component(Part that surrounded_by(Whole)) --> cc_relational_adj(Whole),lumen(Part).

% endosome lumen
cellular_component(Part that surrounded_by(Whole)) --> cellular_component5(Whole),lumen(Part).

% vacuolar lumen
cellular_component(Part that surrounded_by(Whole)) --> cc_relational_adj(Whole),lumen(Part).

lumen(C) --> [lumen],{class_label_exact(C,'cell part')}.
space(C) --> [space],{class_label_exact(C,'cellular_component')}.

% mitochondrial intermembrane space
cellular_component5(Space that surrounds(Part)) --> cc_relational_adj(Part),intermembrane_space(Space).

% plastid intermembrane space
cellular_component(Space that surrounds(Part)) --> cellular_component5(Part),intermembrane_space(Space).

intermembrane_space(C) --> [intermembrane],[space],{class_label_exact(C,'cellular_component')}.

% l5

cc_relational_adj(P) --> [Adj],{relational_adj_ra(Adj,Noun,ObolAv),cc_av(ObolAv),class_label_exact(P,Noun)}.
cc_av(cellular_component).
cc_av(anatomy).
