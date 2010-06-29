:- [bio(obol_obo_xp)].
:- [bio(obol_anatomy_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.
:- multifile bearer/3.
:- multifile gross_anatomical/3.
:- multifile phenotype/3.
:- multifile def/3.
:- multifile term_textdef/3.

term_label(P) --> phenotype(P).

term_textdef(P) --> def(phenotype(P)).


ivory(Q) -->
        {class(Q,ivory)},
        [sclerosis].

def(quality(Q)) -->
        quality_adj(Q),
        !,
        [appearance].
def(quality(Q)) -->
        quality_noun(Q).

% REWRITE RULE
def(phenotype(Q that inheres_in(P) and inheres_in_part_of(W))) -->
        def(phenotype(Q that inheres_in(P that part_of(W)))).

% REWRITE RULE
def(phenotype(Q that inheres_in_part_of(W) and inheres_in(P))) -->
        def(phenotype(Q that inheres_in(P that part_of(W)))).

def(phenotype('PATO:0000001' that inheres_in(B))) -->
        !,
        [abnormality,of,the],
        hpo_anatomical_continuant(B).

def(phenotype(Q that inheres_in(B))) -->
        ivory(Q),
        !,
        [of,the],
        hpo_anatomical_continuant(B),
        [', leading to an increased degree of radiopacity (white or ivory appearance) in X-rays'].

def(phenotype(Q that inheres_in(B))) -->
        def(quality(Q)),
        !,
        [of,the],
        hpo_anatomical_continuant(B).

hpo_quality('PATO:0000001') --> [abnormality].
hpo_quality(Q) --> quality(Q).

quality_adj(Q) --> quality(Q),{class(Q,QN),adj(QN),!}.
quality_adj(Q) --> quality(Q),{class(Q,QN),adj(QN,_),!}.
quality_adj(Q) --> quality(Q),{class(Q,QN),relational_adj_ra(_,QN,_),!}.
quality_noun('PATO:0000001') --> [abnormality].
quality_noun('PATO:0000051') --> [abnormal],[morphology].
quality_noun('PATO:0000117') --> [abnormal],[size].
quality_noun(Q) --> quality(Q).

% REWRITE
phenotype(Q that inheres_in(P) and inheres_in_part_of(W)) --> phenotype(Q that inheres_in(P that part_of(W))).

phenotype(Q that inheres_in(B) and qualifier(Ab)) -->
        qualifier(Ab),
        hpo_quality(Q),
        [of],
        bearer(B).


% curved X
phenotype(Q that inheres_in(B)) --> quality_adj(Q),bearer(B).

% increased size of X
phenotype(Q that inheres_in(B)) --> quality_noun(Q),[of],bearer(B).

bearer(P that part_of(W)) --> continuant(P),[of],continuant(W).
bearer(B) --> continuant(B).


hpo_anatomical_continuant(P that part_of(W)) --> hpo_anatomical_continuant(P),[of,the],hpo_anatomical_continuant(W).
hpo_anatomical_continuant('FMA:240707') --> ['epiphysis'].
hpo_anatomical_continuant(A) --> anatomical_continuant(A).

adj(conical).
adj(ivory).
adj(triangular).

