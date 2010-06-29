:- [bio(obol_obo_xp)].
:- [bio(obol_anatomy_xp)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.

term_label(P) --> trait(P).

related_trait --> [related],[trait].
related_trait --> [trait].

trait(P) --> trait5(P),related_trait.
trait(P) --> trait5(P).
trait(Q that has_qualifier(Mod)) --> qualifier(Mod),trait(Q).
%trait(P) --> measurement(P).

% bran percentage; requires OBI?
% TODO measurement(M that has_unit(U) and measurement_of(B)) --> terminal(B),unit(U),{class_label_exact(M,measurement)}.

qualifier(anon(relative,qualifier)) --> [relative].
qualifier(Mod) --> any_kind_of(Mod,abnormal).
qualifier(Mod) --> any_kind_of(Mod,normal).

trait5(Q that inheres_in(B) and towards(X) and relative_to(Y)) --> terminal(B),quality(X),[to],quality(Y),ratio(Q).
trait5(Q that towards(X) and relative_to(Y)) --> quality(X),[to],quality(Y),ratio(Q).
ratio(Q) --> [ratio],{class_label_exact(Q,'proportionality to')}.

% B E2 content; eg
trait5(Q that inheres_in(B) and towards(Towards)) --> terminal(B),quality(Towards),content(Q).
content(Q) --> [content],{class_label_exact(Q,'content')}.

% generic relational Q
trait5(Q that towards(Towards)) --> force(terminal(Towards),environment),relational_quality(Q).

% sugar content
trait5(Q that towards(C)) --> continuant(C),[content],{class_label_exact(Q,amount)}.

% Q related trait
trait5(Q that inheres_in(Plant)) --> quality(Q),related_trait,{class_label_exact(Plant,'plant structure')}.

% homeotic development trait
trait5(Q that inheres_in(B)) --> bearer(B),related_trait,{class_label_exact(Q,quality)}.

% X resistance & resistance_to?
trait5(Q that towards(Disease)) --> force(terminal(Disease),disease),resistance(Q).
trait5(Q that towards(Disease)) --> resistance(Q),force(terminal(Disease),disease). % todo: normally has prep included; eg res to
resistance(Q) --> any_kind_of(Q,'resistance to').
resistance(Q) --> [resistance],any_kind_of(Q,'resistance to').
% todo - add non-to syns back to pato

% generic BQ; eg
trait5(Q that inheres_in(B)) --> terminal(B),relational_quality(Q).

% glutinous endosperm
% TODO: value slim only?
trait5(Q that inheres_in(B)) --> quality(Q),bearer(B).

% carpel shape. TODO: attribute slim only?
trait5(Q that inheres_in(B)) --> bearer(B),quality(Q).

% rhizome internode number
trait5(Q that inheres_in(B) and towards(E2)) --> bearer(B),bearer(E2),number(Q).

% TO:0000320 ! rubisco to chlorophyll ratio
trait5(Q that has_rato_quality(RQ) and has_dividend_quality(E1) and has_divisor_quality(E2)) --> bearer(E1),[to],bearer(E2),ratio(Q,RQ).

ratio(Q,RQ) --> [ratio],{class_label_exact(Q,'proportional to'),RQ='PATO:0000070'}. % '0'

% floret number per branch
trait5(Q that inheres_in(B) and towards(E2)) --> bearer(E2),number(Q),[per],bearer(B).

% carpel number
trait5(Q that towards(B)) --> bearer(B),number(Q).

number('PATO:0001555') --> [number].

% root dry weight
trait5(Q that inheres_in(B)) --> bearer_with_quality(B),quality(Q).
bearer_with_quality(B that has_quality(Q)) --> terminal(B),quality(Q).
bearer_with_quality(B that has_quality(Q)) --> spatial(Q),terminal(B). % (basal leaf sheath) color
term_label(B) --> bearer_with_quality(B). % need to be able to generate it for the round trip

bearer(B) --> continuant(B).
bearer(B) --> process(B).

% high brain weight
phenotype(Q that inheres_in(B)) --> value(V),terminal(B),attribute(A),{av2q(A,V,Q)}.

% todo - avoid dupes with mp
% eg increased+level = increased level
av2q(A,V,Q):-
        (   nonvar(A),
            nonvar(V)
        ->  concat_atom([A,V],' ',QN),
            class_label(Q,QN,_)
        ;   class_label(Q,QN,_),
            concat_atom([A,V],' ',QN)).

