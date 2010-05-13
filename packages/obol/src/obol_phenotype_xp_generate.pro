:- [bio(obol_obo_xp)].
%:- [bio(obol_quality_xp)].

:- multifile term_label/3.
:- multifile bearer/3.
:- multifile phenotype/3.

term_label(P) --> phenotype(P).
% degenerate forebrain ; atrophied X
phenotype(Q that inheres_in(B)) --> monadic_quality(Q),bearer(B),{entity_partition(Q,value_slim)}.

% forebrain morphology
phenotype(Q that inheres_in(B)) --> monadic_quality(Q),[of],bearer(B),{entity_partition(Q,attribute_slim)}.

monadic_quality(Q) --> any_kind_of(Q,'quality of a single physical entity').

%number_quality(Q) --> [number,of],{class_label_exact(Q,'has number of')}.
%number_quality(Q) --> [increased,number,of],{class_label_exact(Q,'has extra parts of type')}.
%number_quality(Q) --> [decreased,number,of],{class_label_exact(Q,'has fewer parts of type')}.

number_quality(Q) --> [count],{class_label_exact(Q,'has number of')}.
number_quality(Q) --> [increased,count],{class_label_exact(Q,'has extra parts of type')}.
number_quality(Q) --> [decreased,count],{class_label_exact(Q,'has fewer parts of type')}.

% increased number of inclusions in forebrain
%phenotype(Q that towards(B2) and inheres_in(B)) --> number_quality(Q),towards(B2),[in],bearer(B).
% forebrain, inclusion count increased
phenotype(Q that towards(B2) and inheres_in(B)) --> bearer(B),[','],towards(B2),number_quality(Q).
% TODO: currently sensitive to ordering...
phenotype(Q that inheres_in(B) and towards(B2)) --> phenotype(Q that towards(B2) and inheres_in(B)).

phenotype(Q that towards(B2)) --> towards(B2),number_quality(Q).

towards(X) --> bearer(X).

bearer(B) --> continuant(B).

