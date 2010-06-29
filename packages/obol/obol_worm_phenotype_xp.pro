:- [bio(obol_phenotype_xp)].

:- multifile term_label/3.
:- multifile abnormal/3.
:- multifile phenotype/3.
:- multifile quality/3.

term_label(P) --> temporal_phenotype(P).

temporal_phenotype(P and during(S)) --> phenotype(P),wormstage(S).


% place any worm-specific patterns beneath here:
% sperm_release_defective : requires parsing of [sperm release]

abnormal(Ab) --> [variant],{class_label_exact(Ab,abnormal)}.

% lipid_depleted
phenotype(Q that towards(C)) --> bearer(C),depleted(Q).
depleted(Q) --> [depleted],{class_label_exact(Q,'decreased concentration')}.

% male_tail_curling_serotonin_variant
phenotype(Q that inheres_in_part_of(W) and inheres_in(P) and in_response_to(C) and qualifier(A)) --> bearer(W),bearer(P),quality(Q),chemical(C),abnormal(A).

% locomotion_rate_serotonin_variant
phenotype(Q that inheres_in(P) and in_response_to(C) and qualifier(A)) --> process(P),quality(Q),chemical(C),abnormal(A).


% zinc_toxicity_hypersensitive
% zinc_toxicity_resistant
% chloroquinone_hypersensitive

phenotype(Q that towards(C)) --> bearer(C),sensitive(Q).
phenotype(Q that towards(C)) --> bearer(C),resistant(Q).

phenotype(Q that towards(C) and qualifier(A)) --> bearer(C),sensitive(Q),abnormal(A).
phenotype(Q that towards(C) and qualifier(A)) --> bearer(C),resistant(Q),abnormal(A).

sensitive(Q) --> [hypersensitive],{class_label_exact(Q,'sensitive toward')}.
sensitive(Q) --> [toxicity],[hypersensitive],{class_label_exact(Q,'sensitive toward')}.
sensitive(Q) --> [response],{class_label_exact(Q,'response to')}.

resistant(Q) --> [resistant],{class_label_exact(Q,'resistant to')}.
resistant(Q) --> [toxicity],[resistant],{class_label_exact(Q,'resistant to')}.

%phenotype(Q that towards(P)) --> [latency,variant],process(P),{class_label_exact(Q,'irregular duration')}.
%phenotype(Q that towards(P)) --> [latency,increased],process(P),{class_label_exact(Q,'irregular duration')}.

% embryonic_polarity_variant
phenotype(Q that inheres_in(B) and qualifier(Ab)) --> anatomy_relational_adj(B),quality(Q),abnormal(Ab).

% pale embryo
phenotype(Q that inheres_in(O) and during(S)) --> quality(Q),wormstage(S),{class_label_exact(O,organism)}.


wormstage(S) --> terminal(S),{belongs(S,worm_development)}.
wormstage(S) --> [early,emb],{class_label_exact(S,'proliferating embryo')}. % TODO!! CHECK!!

quality(Q) --> [protrusions],{class_label_exact(Q,protruding)}.
quality(Q) --> [curling],{class_label_exact(Q,curled)}.




