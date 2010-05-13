:- [bio(obol_obo_xp)].
%%:- [bio(obol_anatomy_xp)].
:- [bio(obol_cellular_component_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.
:- multifile bearer/3.
term_label(P) --> disease(P).

% Gastric Cancer Stage IA
disease(D that has_stage(S)) --> disease5(D),stage(S).
disease(D that has_stage(S)) --> stage(S),disease(D).
disease(D) --> [disorder],{class_label_exact(D,disease)}.

% malignant X
disease(D that has_quality(Q)) --> quality(Q),disease(D).

% secondary X
disease(D that has_quality(Q)) --> disease_adj(Q),disease(D).

% neoplasm of thyroid : todo - IC vs DC
disease(D that inheres_in(B)) --> disease5(D),[of],det,bearer(B).

det --> [a].
det --> [the].
det --> [].

stage(S) --> [stage],force(terminal(S),stage).

disease(D) --> disease5(D).

% syphilitic meningitis
disease5(D that has_agent(DQ)) --> disease_relational_adj(DQ),disease(D).

% spinal cord disease
disease5(D that occurs_in(A)) --> anatomy_relational_adj(A),disease(D).

% 
disease5(D that inheres_in(B)) --> bearer(B),disease(D).

% ovarian cancer
disease5(D that inheres_in(B)) --> anatomy_relational_adj(B),disease(D).

disease5(D that inheres_in(B)) --> [hemotopoietic],disease(D),{class_label_exact(B,'hematopoeitic system')}.

disease5(D that has_agent(A)) --> agent(A),disease(D).
disease(D that has_agent(A)) --> disease5(D),[','],agent(A).
agent(A) --> [meningococcal],{class_label_exact(A,'Neisseria meningitidis')}.
agent(A) --> [gonococcal],{class_label_exact(A,'Neisseria gonorrhoeae')}.
agent(A) --> terminal(A),{belongs(A,unknown)}. % massive hack - todo - give ncbitaxon its own namespace

disease5(Q) --> terminal(Q),{belongs(Q,disease_ontology)}.

% TODO - DO is v inconsistent here, but can probably treat them all as the same
disease5('DOID:162') --> ['neoplasm'].
disease5('DOID:162') --> ['tumor'].


% TODO: explicit ontologies; allow compositon. just hack for now..
%bearer(B) --> terminal(B),{\+belongs(B,disease)}.
bearer(B) --> anatomical_continuant(B).
bearer(B) --> process(B).

% todo: move?
% eg colorectal, overian, gastric
anatomy_relational_adj(P) --> [Adj],{relational_adj_ra(Adj,Noun,anatomy),class(P,Noun)}.

disease_relational_adj(D) --> [Adj],{relational_adj_ra(Adj,Noun,disease),class(D,Noun)}.

disease_adj(Q) --> [Adj],{adj(Adj,disease),class(Q,Adj)}.

