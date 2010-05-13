:- [bio(obol_obo_xp)]. 

% refs:
% MA naming rules are very consistent; see:
%  http://genomebiology.com/2005/6/3/R29

% TODO:
%  this is a mixture of general anatomical and gross anatomical
%  separation should instead be along the lines of which As we want to decompose eg in P statements,
%  and which we decompose in AOs

:- multifile anatomical_continuant/3,gross_anatomical/3,term_label/3,spatial/3.


term_label(X) --> anatomical_continuant(X).
term_textdef(C) --> def(gross_anatomical(C)).

anatomical_continuant(C) --> anatomical_continuant5(C).

% eg nucleus envelope. 
gross_anatomical(Part that part_of(Whole)) --> gross_anatomical5(Whole),gross_anatomical(Part),{\+class(Part,primordium)}.
gross_anatomical(Part that part_of(Whole)) --> gross_anatomical5(Part),[of],gross_anatomical(Whole).

% WBbt:0005787 neuron of the preproductive system
gross_anatomical(Part that part_of(Whole)) --> gross_anatomical5(Part),[of],[the],gross_anatomical(Whole).

def(gross_anatomical(Part that part_of(Whole))) --> ['A'],terminal(Part),['that is part of a'],terminal(Whole).

% left X: TODO: laterality?
gross_anatomical(Part that has_quality(Spatial)) --> spatial(Spatial),gross_anatomical(Part).
% worm sometimes reverses the order, and says "X left" - eg IL sensillum right
gross_anatomical(Part that has_quality(Spatial)) --> gross_anatomical5(Part),spatial(Spatial).

% virion part
gross_anatomical(CC that part_of(Whole)) --> gross_anatomical5(Whole),[part],{class_label_exact(CC,'gross_anatomical')}.

% intracellular membrane-bound organelle
gross_anatomical(Whole that contained_by(Cell)) --> [intracellular],gross_anatomical(Whole),{class_label_exact(Cell,'cell')}.

% presumptive brain
% use 'portion of tissue' as genus for now, but change to 'developing structure' later
gross_anatomical(C that develops_into(MC)) --> [presumptive],gross_anatomical(MC),{class_label_exact(C,'portion of tissue')}.



% X lumen
% testis lumen
% TODO: enclosed?
gross_anatomical(Space that surrounded_by(Whole)) --> gross_anatomical5(Whole),[lumen],{class_label_exact(Space,'anatomical space')}.
% pharyngeal lumen ; humeral epiphysis
gross_anatomical(Space that surrounded_by(Whole)) --> anatomy_relational_adj(Whole),[lumen],{class_label_exact(Space,'anatomical space')}.

% adult midgut precursor
gross_anatomical(Precursor that develops_into(MatureEntity)) --> gross_anatomical5(MatureEntity),precursor(Precursor).

precursor(C) --> any_kind_of(C,primordium).
precursor(C) --> [precursor],{class_label_exact(C,'portion of tissue')}.

% wing pouch
gross_anatomical(Precursor that develops_into(MatureEntity)) --> gross_anatomical5(MatureEntity),epithelial_precursor(Precursor).
gross_anatomical(Precursor that develops_into(MatureEntity)) --> anatomy_relational_adj(MatureEntity),epithelial_precursor(Precursor).


% NOTE: these may well be incorrect!!
%epithelial_precursor(C) --> [pouch],{class_label_exact(C,'epithelium')}.
epithelial_precursor(C) --> [bud],{class_label_exact(C,'embryonic structure')}.
epithelial_precursor(C) --> [placode],{class_label_exact(C,'embryonic structure')}.
epithelial_precursor(C) --> [layer],{class_label_exact(C,'portion of tissue')}. 

% l5

% midbrain-hindbrain boundary
anatomical_continuant(B that connects_to(P1) and connects_to(P2)) --> anatomical_continuant5(P1),anatomical_continuant(P2),boundary(B).
anatomical_continuant(B that connects_to(P1) and connects_to(P2)) --> anatomical_continuant5(P1),['-'],anatomical_continuant(P2),boundary(B).

% non-standard construct (temp: for xenopus)
anatomical_continuant(B that connects_to(P1) and connects_to(P2)) --> anatomical_continuant5(P1),['-'],anatomical_continuant(P2),['-'],boundary(B).
boundary(B) --> [boundary],{class_label_exact(B,'anatomical line')}. % correct class?
boundary(B) --> any_kind_of(B,'anatomical line').
boundary(B) --> any_kind_of(B,'anatomical surface').

% nuclear envelope
anatomical_continuant5(Part that part_of(Whole)) --> anatomy_relational_adj(Whole),anatomical_continuant(Part).


spatial(S) --> any_kind_of(S,'FMA:66929'). %Structural relationship value





