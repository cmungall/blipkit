:- [bio(obol_obo_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.
:- multifile continuant/3.

:- discontiguous environment/3.

term_label(P) --> environment(P).

% forest soil
% NOT: fish pond
environment(P that part_of(W)) --> habitat(W),environmental_substance(P).

% fish pond
% sand desert
environment(W that has_part(P)) --> environmental_substance(P),environment(W).

% crater lake
% sea mount
% lake surface TODO (no is_a parent)
environment(P that located_in(W)) --> geographic_feature(W),geographic_feature(P).

% stream valley; river valley
% THIS CONFLICTS WITH THE ABOVE
environment(W that has_part(P)) --> geographic_feature(P),geographic_feature(W).

% saline lake
environment(E that has_quality(Q)) --> quality(Q),environment(E).

% coastal water; oceanic lake
environment(E that part_of(W)) --> e_relational_adj(W),environment(E).

% acid mine
environment(E that has_quality(Q)) --> [Noun],environment(E),{relational_adj_ra(Adj,Noun,attribute),class_label_exact(Q,Adj)}.


% chloropicrine-enriched soil
environment(E that has_quality(Q that towards(C))) --> continuant(C),['-'],enriched(Q),environment(E).
environment(E that has_quality(Q that towards(C))) --> continuant(C),enriched(Q),environment(E).

e_relational_adj(P) --> [Adj],{relational_adj_ra(Adj,Noun,environment),class_label_exact(P,Noun)}.

% grain of rocky sand
environment(E that part_of(W)) --> [grain,of],environment(W),{class_label_exact(E,'grain of sand')}.


% arsenic-rich mud
enriched(Q) --> [enriched],{class_label_exact(Q,'increased concentration')}.
enriched(Q) --> [rich],{class_label_exact(Q,'increased concentration')}.
enriched(Q) --> [contaminated],{class_label_exact(Q,'increased concentration')}. % TODO
% arsenate treated wood
enriched(Q) --> [treated],{class_label_exact(Q,'increased concentration')}. % TODO

% high temperature habitat - TODO

% flood plain - disposition?
% TODO

% lake
geographic_feature(E) --> any_kind_of(E,'geographic feature').

% forest
habitat(E) --> any_kind_of(E,'habitat').

% soil
environmental_substance(E) --> any_kind_of(E,'environmental substance').

environment(E) --> environment5(E).
environment5(E) --> terminal(E),{belongs(E,'ENVO')}.

% agricultual ==> agricultural region
environment5(E) --> [Token],{adj_class(Token,E)}.

adj_class(A,C):-
        (   var(C)
        ->  atom_concat(A,' region',N),
            class(C,N)
        ->  class(C,N),
            atom_concat(A,' region',N)).


%continuant(C) --> environment5(C).

