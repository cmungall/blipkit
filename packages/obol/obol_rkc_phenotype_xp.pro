:- [bio(obol_obo_xp)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.
:- multifile bearer/3.
term_label(P) --> phenotype(P).

% Cells are reniform (bean-shaped, kidney-shaped).
phenotype(Q that inheres_in(B)) --> bearer_plural(B),[are],q(Q).

% Helical cells have cristae
phenotype(Q that inheres_in(B) and towards(X)) --> bearer_plural(B),[have],bearer(X),{has_parts(Q)}.

% Cell has infundibulum
phenotype(Q that inheres_in(B) and towards(X)) --> bearer(B),[has],bearer(X),{has_parts(Q)}.

% Cells are circular in transverse section
phenotype(Q that inheres_in(S that part_of(B))) --> bearer_plural(B),[are],q(Q),[in],spatial(S).

% Cell contains gas vacuoles
phenotype(Q that inheres_in(B) and towards(X)) --> bearer(B),[contains],bearer(X),{has_parts(Q)}.

% cells contain chlorophyl
phenotype(Q that inheres_in(B) and towards(X)) --> bearer_plural(B),[contain],bearer(X),{has_parts(Q)}.

% Rod axis is straight
% Cell dorsal surface is flat
phenotype(Q that inheres_in(B)) --> bearer(B),[is],q(Q).

% Epicone is present
phenotype(Q that inheres_in(C) and towards(X)) --> bearer(X),[is],[present],{class_label_exact(C,cell),class_label_exact(Q,'having extra physical parts')}.

% Cell shape is caudate
phenotype(Q that inheres_in(B)) --> bearer(B),[shape],[is],quality(Q).

% Rod axis is curved in one plane
phenotype(Q that inheres_in(S that part_of(B))) --> bearer(B),[is],q(Q),[in,one,plane],{class_label_exact(S,'transverse section')}.
phenotype(Q that inheres_in(S that part_of(B))) --> bearer(B),[is],q(Q),[in],spatial(S).

% Helical cells have axial filaments
phenotype(Q that inheres_in(B that has_quality(QQ)) and towards(X)) --> quality(QQ),bearer_plural(B),[have],bearer(X),{has_parts(Q)}.

% second longest axis of each cell is 31-6.0 microns

% Cells are elliptic , with the ratio of  the long axis to the short axis between 6:1 and 3:1
% phenotype(Q that inheres_in(S that part_of(B))) --> bearer_plural(B),[are],q(Q),[',',with,the,ratio,of,the,long,axis,to,the,short,axis,between,X1,':',Y1,and,X2,':',Y2].

% L-Arginine is decarboxylated
% phenotype(Q that inheres_in(B)) --> chemical(C),[is],[decarboxylated],{class_label_exact()}.

% get rid of braces in pre-parse
%q(Q) --> quality(Q),bracketed(_).
%q(Q) --> force(quality(Q),quality).
q(Q) --> quality(Q).

bearer(S that part_of(B)) --> spatial(S),bearer(B).

% cell ventral surface
bearer(S that part_of(B)) --> terminal(B),spatial(S).

% transverse section of 
bearer(S that part_of(B)) --> spatial(S),[of],bearer(B).

bearer(B) --> continuant(B).
bearer(B) --> process(B).

bearer(CC that has_part(B)) --> continuant(B),[inclusions],{class_label_exact(CC,cellular_component)}.

%bearer(B) --> ['Cell'],{class_label_exact(B,cell)}.

bearer_plural(C) --> ['cells'],{class_label_exact(C,cell)}.
bearer_plural(C that has_quality(Q)) --> ['rods'],{class_label_exact(C,cell),class_label_exact(Q,'rod-shaped')}.

bracketed(X) --> ['('],tokens(X),[')'].

has_parts(Q):- class_label_exact(Q,'extra or missing physical and functional parts').

