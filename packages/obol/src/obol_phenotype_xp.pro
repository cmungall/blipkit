:- [bio(obol_obo_xp)].
:- [bio(obol_quality_xp)].
% TODO: REINTRODUCE THESE
%:- [bio(obol_biological_process_xp_anatomy)]. % required for class expressions for processes; eg abnormal osteoclast formation
%:- [bio(obol_biological_process_xp_cell)]. % required for class expressions for processes; eg abnormal osteoclast formation

:- multifile term_label/3.
:- multifile bearer/3.
:- multifile phenotype/3.

term_label(P) --> phenotype(P).
% long mandible
phenotype(Q that inheres_in(B)) --> quality(Q),bearer(B).
% nasal septum morphology
phenotype(Q that inheres_in(B)) --> bearer(B),quality(Q).


% X phenotype
phenotype(Q that inheres_in_part_of(B)) --> bearer(B),[phenotype],{class_label_exact(Q,quality)}.
phenotype(Q that inheres_in(B) and qualifier(A)) --> abnormal(A),quality(Q),bearer(B).
% abnormal retinal ganglion cell morphology
% abnormal nocireceptor morphology
phenotype(Q that inheres_in(B) and qualifier(A)) --> abnormal(A),bearer(B),quality(Q).
phenotype(Q that qualifier_part_of(Ab)) --> abnormal(Ab),quality(Q).

% abnormal brain interneuron morphology
%  here we use inheres_in_part_of - though we could also use a class expression (P that part_of W). However, we prefer named classes
phenotype(Q that inheres_in_part_of(W) and inheres_in(P) and qualifier(A)) --> abnormal(A),bearer(W),bearer(P),quality(Q).

% male_nervous_system_morphology_variant
phenotype(Q that inheres_in_part_of(W) and inheres_in(P) and qualifier(A)) --> bearer(W),bearer(P),quality(Q),abnormal(A).

% hippocampal neuron degeneration
phenotype(Q that inheres_in_part_of(W) and inheres_in(P)) --> quality(Q),anatomy_relational_adj(W),bearer(P).
% hippocampus neuron degeneration
phenotype(Q that inheres_in_part_of(W) and inheres_in(P)) --> quality(Q),bearer(W),bearer(P).

% vaginal atresia
% gingival hyperplasia
phenotype(Q that inheres_in(B)) --> anatomy_relational_adj(B),quality(Q).

% omim-style
phenotype(Q that inheres_in_part_of(P) and qualifier(Ab)) --> bearer(P),abnormal(Ab),{class_label_exact(Q,quality)}.
phenotype(Q that inheres_in_part_of(P) and qualifier(Ab)) --> abnormal(Ab),bearer(P),{class_label_exact(Q,quality)}.

phenotype(Q that inheres_in(B)) --> quality(Q),[','],bearer(B).

phenotype(Q that inheres_in(B)) --> quality(Q),[of],bearer(B).

% optic atrophy from cranial nerve compression
phenotype(Q that inheres_in(B)) --> quality(Q),[from],bearer(B).
phenotype(Q that inheres_in(B)) --> quality(Q),[due],[to],bearer(B).

% abnormal neural plate morphology (requires EMAP?)
%%%%% phenotype(Q that qualifier(Ab)) --> abnormal(Ab),phenotype(Q). this one leads to unwanted post-compositions
phenotype(Q that qualifier(Ab)) --> quality(Q),abnormal(Ab).

% motor_neuron_morphology_abnormal
phenotype(Q that inheres_in( B) and qualifier(Ab)) --> bearer(B),quality(Q),abnormal(Ab).

% organ abnormality
phenotype(Q that inheres_in(B) and qualifier(Ab)) --> bearer(B),abnormal(Ab),{class_label_exact(Q,quality)}.

abnormal(Ab) --> [abnormal],{class_label_exact(Ab,abnormal)}.
abnormal(Ab) --> [abnormalities],{class_label_exact(Ab,abnormal)}.
abnormal(Ab) --> [abnormality],{class_label_exact(Ab,abnormal)}.

% abnormal tongue spamous epithelium
phenotype(Q that inheres_in(B2 that part_of(B1)) and qualifier(A)) --> abnormal(A),continuant(B1),continuant(B2),{class_label_exact(Q,quality)}.

% ameloblast degeneration
phenotype(Q that inheres_in(P)) --> bearer(P),[degeneration],{class_label_exact(Q,'degenerate')}.

% absent X
phenotype(Q that towards(P)) --> [absent],terminal(P),{class_label_exact(Q,'lacks all physical parts of type')}.

% absent tongue spamous epithelium
phenotype(Q that towards(P) and  inheres_in(W)) --> [absent],continuant(W),continuant(P),{class_label_exact(Q,'lacks all physical parts of type')}.

% absent eye pigmentation
phenotype(Q that towards(P) and inheres_in(W)) --> [absent],continuant(W),process(P),{class_label_exact(Q,'lacking processual parts')}.

% no cuticle (WBPh)
phenotype(Q that towards(P)) --> [no],terminal(P),{class_label_exact(Q,'lacks all physical parts of type')}.

% rays_missing (WBPh)
phenotype(Q that towards(P)) --> terminal(P),[missing],{class_label_exact(Q,'lacks all physical parts of type')}.

% OMIM (todo: move?)
% philtrum thick and wide
phenotype(P that has_quality(Q1) and has_quality(Q2)) --> bearer(P),quality(Q1),[and],quality(Q2).
% eyelid and corneal neuroma
% todo: same quality cannot inhere in >1 bearer..
phenotype(Q that inheres_in(P1) and inheres_in(P2)) --> bearer(P1),[and],bearer(P2),quality(Q).
phenotype(Q that inheres_in(P1) and inheres_in(P2) and inheres_in(P3)) --> bearer(P1),[','],bearer(P2),[and],bearer(P3),quality(Q).

% failure of intramembranous bone ossification
phenotype(Q that inheres_in(B)) --> [failure],[of],process(B),{class_label_exact(Q,interrupted)}.

% arrest of spermiogenesis
phenotype(Q that inheres_in(B)) --> [arrest],[of],process(B),{class_label_exact(Q,arrested)}.

% TODO: generalize stuff below..

%% ----------------------------------------
%% relational qualities: absolute number
%% ----------------------------------------

% increased granulocyte number
phenotype(Q that towards(C)) --> quantity(Q),bearer(C),qnumber.

quantity(Q) --> any_kind_of(Q,'increased number').
quantity(Q) --> [increased],{class_label_exact(Q,'increased number')}.
quantity(Q) --> any_kind_of(Q,'decreased number').
quantity(Q) --> [decreased],{class_label_exact(Q,'decreased number')}.

phenotype(Q that towards(C)) --> [supernumerary],bearer(C),{class_label_exact(Q,'has extra parts of type')}.


% increased Il1b secretion
phenotype(Q that towards(P)) --> [increased],process(P),{class_label_exact(Q,'increased rate')}.
phenotype(Q that towards(P)) --> [decreased],process(P),{class_label_exact(Q,'decreased rate')}.
phenotype(Q that towards(P)) --> [reduced],process(P),{class_label_exact(Q,'decreased rate')}.


qnumber --> [numbers].
qnumber --> [number].
qnumber --> [amount].


% TODO: explicit ontologies; allow compositon. just hack for now..
%bearer(B) --> terminal(B),{\+belongs(B,quality)}.
bearer(B) --> continuant(B).
bearer(B) --> process(B).


% evelated artery pressure
phenotype(Q that inheres_in(B)) --> [Magn],bearer(B),[Att],{magn_att_quality(Magn,Att,QN),class_label_exact(Q,QN)}.

% decreased number of
%phenotype(Q that towards(E)) --> [Magn],[Att],[of],bearer(E),{rmagn_att_quality(Magn,Att,QN),class_label_exact(Q,QN)}.

% increased plasma aldosterone
phenotype(Q that inheres_in(B) and towards(C)) --> [Magn],bearer(B),continuant(C),{magn_att_quality(Magn,levels,QN),class_label_exact(Q,QN)}.
% plasma cortisol low
phenotype(Q that inheres_in(B) and towards(C)) --> bearer(B),continuant(C),[Magn],{magn_att_quality(Magn,levels,QN),class_label_exact(Q,QN)}.

magn_att_quality(elevated,pressure,'increased pressure').
magn_att_quality(reduced,pressure,'decreased pressure').
magn_att_quality(increased,levels,'increased concentration').
magn_att_quality(elevated,levels,'increased concentration').
magn_att_quality(high,levels,'increased concentration').
magn_att_quality(decreased,levels,'decreased concentration').
magn_att_quality(low,levels,'decreased concentration').


%rmagn_att_quality(increased,number,'increased amount').
%rmagn_att_quality(decreased,number,'decreased amount').


% increased diameter of long bones
phenotype(Q that inheres_in(B)) --> [Magn],quality(BaseQ),[of],bearer(B),{magnitude_attribute_quality(Magn,BaseQ,Q)}.
% increased X size
phenotype(Q that inheres_in(B)) --> [Magn],bearer(B),quality(BaseQ),{magnitude_attribute_quality(Magn,BaseQ,Q)}.

% TODO: do this generically or with specific usages (see above)
magnitude_attribute_quality(Magn,BaseQ,Q):-
        nonvar(Magn),
        nonvar(BaseQ),
        class_label_exact(BaseQ,BaseQN),
        concat_atom([Magn,BaseQN],' ',QN),
        class_label_exact(Q,QN).
magnitude_attribute_quality(Magn,BaseQ,Q):-
        var(Magn),
        var(BaseQ),
        class_label_exact(Q,QN),
        concat_atom([Magn,BaseQN],' ',QN),
        class_label_exact(BaseQ,BaseQN).

