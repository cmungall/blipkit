:- [bio(obol_obo_xp)].
:- [bio(obol_anatomy_xp)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.

term_label(P) --> bnproperty(P).

fluff --> [the].
fluff --> [value].
fluff --> [absolute].

uquality(Q) --> quality(Q).
uquality(Q) --> unit(Q).
uquality(Q) --> [abundance],{class_label_exact(Q,amount)}.
uquality(Q) --> [number],{class_label_exact(Q,amount)}.

bnproperty(P) --> bnproperty5(P).
% rate of depolymerization (average)
bnproperty(P that has_qualifier(Mod)) --> bnproperty5(P),['('],qualifier(Mod),[')'].
% Average number of glial cells in brain per neuron
bnproperty(Q that has_qualifier(Mod)) --> qualifier(Mod),bnproperty(Q).
bnproperty(P) --> [fluff],bnproperty(P).

inprep --> [of].
inprep --> [in].
inprep --> [stored],[in]. % Total carbon stored in the oceans
to --> [to].
to --> [':'].


qualifier(anon(relative,qualifier)) --> [relative].
qualifier(Mod) --> any_kind_of(Mod,abnormal).
qualifier(Mod) --> any_kind_of(Mod,normal).
qualifier(max) --> [maximal].
qualifier(max) --> [max].
qualifier(max) --> [maximum].
qualifier(min) --> [minimal].
qualifier(min) --> [minimum].
qualifier(typical) --> [typical].
qualifier(avg) --> [average].
qualifier(avg) --> [mean].
qualifier(med) --> [median].
qualifier(sum) --> [total].

/*
  BASIC
  */
% gene length
bnproperty5(Q that inheres_in(B)) --> bearer(B),uquality(Q).

% size of mRNA
bnproperty5(Q that inheres_in(B)) --> uquality(Q),[of],bearer(B).

% Total CO2 in biosphere
bnproperty5(Q that inheres_in(B) and towards(C)) --> terminal(C),[in],bearer(B),{class_label_exact(Q,quantity)}.

% concentration of MEK in oocytes
bnproperty5(Q that inheres_in(B) and towards(C)) --> relational_quality(Q),[of],terminal(C),inprep,bearer(B).

%  Volume of Nuclei in kidney tissue
% single nucleus or all nuclei?
bnproperty5(Q that inheres_in(B that part_of(W))) --> relational_quality(Q),[of],terminal(B),inprep,bearer(W).

% Volume occupied by rRNA
bnproperty5(Q that inheres_in(B)) --> uquality(Q),[occupied],[by],bearer(B).

% time between blinks TODO
bnproperty5(Q that inheres_in(B)) --> uquality(Q),[between],bearer(B).

% total (dissolved carbon dioxide concentration in ocean water)
bnproperty5(Q that towards(Q2 that inheres_in(C)) and inheres_in(B)) --> uquality(Q2),terminal(C),uquality(Q),inprep,bearer(B).

% number of proteins per transcript
bnproperty5(Q that inheres_in(B) and towards(C)) --> terminal(C),[of],bearer(C),[per],bearer(B).


/*
  RATIOS
  */

% Ratio of silicon to phosphorus in phytoplankton
bnproperty5(Q that inheres_in(B) and towards(X) and relative_to(Y)) --> ratio(Q),[of],terminal(X),to,terminal(Y),inprep,bearer(B).

% Nuclear:cell volume ratio
bnproperty5(RQ that towards(Q that inheres_in(X)) and relative_to(Q that inheres_in(Y))) --> terminal(X),to,terminal(Y),uquality(Q),ratio(RQ).

%bnproperty5(Q that inheres_in(B) and towards(X) and relative_to(Y)) --> terminal(B),uquality(X),[to],uquality(Y),ratio(Q).
%bnproperty5(Q that towards(X) and relative_to(Y)) --> uquality(X),[to],uquality(Y),ratio(Q).
ratio(Q) --> [ratio],{class_label_exact(Q,'proportionality to')}.


bearer('bfo:ObjectAggregate' that has_component(B)) --> [all],bearer(B).

% Xenopus laevis eggs
% rat liver
bearer(B that part_of(W)) --> bearer5(W),bearer(B).

% surface area - COVERED
% bearer(S that part_of(B))) --> bearer(B),spatial(S)



% molecules, proteins, organelles
bearer(union_of(H,T)) --> bearer5(H),[',',],bearer(T).
bearer(union_of(H,T)) --> bearer5(H),[and],bearer(T).
bearer(union_of(H,T)) --> bearer5(H),[or],bearer(T).
bearer(B) --> bearer5(B).

bearer5(B) --> terminal(B).

