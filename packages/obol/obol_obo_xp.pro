:- use_module(bio(classdef_parser)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(tokenizer)).
:- use_module(bio(av_db)).

:- multifile continuant/3.
:- multifile term_label/3.
:- multifile term_textdef/3.
:- multifile def/3.
:- multifile cell/3.
:- multifile cell5/3.
:- multifile process/3,process5/3.
:- multifile molecular_function/3,molecular_function5/3.
:- multifile anatomical_continuant/3.
:- multifile gross_anatomical/3.
:- multifile gross_anatomical5/3.
:- multifile molecular_function/3.
:- multifile chemical/3.
:- multifile spatial/3.
:- multifile cellular_component/3.

det --> [a].
det --> [an].
det --> [the].

%% UPPER
term_label(X) --> continuant(X).
continuant(C) --> anatomical_continuant(C).

% TODO: move
terminal(Class,Label) --> {class_label_exact(Class,Label)},terminal(Class).
terminal(Class,Label,Abbrevs) --> {member(Abbrev,Abbrevs)},[Abbrev],{class_label_exact(Class,Label)}.

% example: force(anatomical_continuant(P),anatomy) on [liver]
% will try the DCG rule anatomical_continuant(P) as normal, but if there are no solutions, will
% bind P to anon(liver,anatomy)
force(DCGTerm,NS) -->
        {debug(obol,'force: ~w (ns: ~w)',[DCGTerm,NS])},
        (   DCGTerm *-> {true}
        ;   {DCGTerm=..[_,anon(Atom,NS)],
             (   nonvar(Atom)
             ->  tok_atom(Atom,Toks)
             ;   true)},
            tokens(Toks),
            {debug(obol,'  toks:~w',[Toks])},
            {(   nonvar(Toks)
             ->  stringify(Toks,Phrase),
                 normalize_word_form(Phrase,Atom),
                 \+adj(Phrase,_) % we do not force 'chronological' etc to be anon terms
             ;   true)}),
        {debug(obol,'forced: ~w (ns: ~w)',[DCGTerm,NS])}.

tokens([T]) --> [T].
tokens([T|TL]) --> [T],!,tokens(TL).

% fwd only
label(X) --> {nonvar(X),atom(X)},terminal(X).

% eg orvarian->ovary
normalize_word_form(Phrase,Atom):-
        relational_adj_ra(Phrase,Atom,_),
        !.
normalize_word_form(X,X).



% ONTOLOGIES

% MF
molecular_function(F) --> molecular_function5(F).
molecular_function5(F) --> terminal(F),{belongs(F,molecular_function)}.


%% -- CL --
%cell(Cell) --> force(cell5(Cell),cell). do this somewhere more specific...
cell(Cell) --> cell5(Cell).
cell5(Cell) --> terminal(Cell),{belongs(Cell,cell)}.

% CMP cell
cell5(Cell) --> terminal(Cell),[cell],{belongs(Cell,cell)}.
anatomical_continuant(C) --> cell(C).

%% BP
% transition 0->5
process(P) --> process5(P).
%% -- terminal: BP --
process5(P) --> process10(P).

% 10 is always terminal
%process10(P) --> terminal(P),{class_label_exact(PRoot,'biological_process'),\+ \+ subclassRT(P,PRoot)}. %dbl-negate to prevent backtracking TODO: belongs?
process10(P) --> terminal(P),{belongs(P,biological_process)}.

% CONTROVERSIAL?? In GO, MF terms represent the execution of a function
process(P) --> molecular_function(P).


%% -- CHEBI --
chemical(CE) --> terminal(CE),{belongs(CE,chebi_ontology)}.
chemical(CE) --> terminal(CE),{belongs(CE,'NIF_Molecule:')}.
continuant(C) --> chemical(C).


%% -- PRO --
% treat these as chemical enities for now
chemical(CE) --> terminal(CE),{belongs(CE,pro)}.

continuant(C) --> protein(C).
protein(CE) --> terminal(CE),{belongs(CE,pro)}.

%% SPATIAL
spatial(S) --> terminal(S),{belongs(S,spatial)}.


biosequence(S) --> terminal(S),{belongs(S,sequence)}.

unit(U) --> terminal(U),{belongs(U,'unit.ontology')}.


anatomical_continuant(C) --> cellular_component(C).
cellular_component(CC) --> cellular_component5(CC).
%cellular_component5(CC) --> terminal(CC),{belongs(CC,cellular_component),CC \= 'GO:0005623'}. % hacky way to use CL cell only...
cellular_component5(CC) --> terminal(CC),{belongs(CC,cellular_component)}. % need to use GO:cell for intra-GO xps
cellular_component5(CC) --> ['Golgi'],{class_label_exact(CC,'Golgi apparatus')}.


% todo - move??
biological_role(X) --> terminal(X),{belongs(X,biological_role)}.
stimulus(X) --> terminal(X),{belongs(X,stimulus)}.
anatomical_continuant(C) --> biological_role(C).
term_label(X) --> biological_role(X).


anatomical_continuant(C) --> gross_anatomical(C).
gross_anatomical(CC) --> gross_anatomical5(CC).
gross_anatomical5(CC) --> terminal(CC),{belongs(CC,Ont),anatomical_ontology(Ont)}.
gross_anatomical5(CC) --> terminal(CC),{belongs(CC,fma),subclassT(CC,'FMA:67165')}.

anatomy_relational_adj(P) --> [Adj],{relational_adj_ra(Adj,Noun,ObolAv),anatomical_av(ObolAv),class_label_exact(P,Noun)}.

% old OBOL avs - for rel adjs
anatomical_av(anatomy).
anatomical_av(cellular_component).
anatomical_av(cell).

% todo - use UBO/CARO mappings
anatomical_ontology(abstract_anatomy).
anatomical_ontology(plant_structure).
anatomical_ontology(zebrafish_anatomy).
anatomical_ontology(teleost_anatomy).
anatomical_ontology(fly_anatomy). % in-preparation..
anatomical_ontology(fungal_anatomy_ontology).
anatomical_ontology('fly_anatomy.ontology').
anatomical_ontology('adult_mouse_anatomy.gxd').
anatomical_ontology('Dictyostelium_discoideum_anatomy').
%anatomical_ontology(fma).
anatomical_ontology(cell).  % slight redundancy.. should we keep this gross anatomy?
anatomical_ontology(worm_anatomy).
anatomical_ontology(tick_anatomy).
anatomical_ontology(uberon).
anatomical_ontology(bila).
anatomical_ontology(xenopus_anatomy). % note - xenopus mixes sites and entities
anatomical_ontology(anatomical_homology_ontology).

anatomical_ontology(birnlex_anatomy).
anatomical_ontology('nif_anatomy:').
anatomical_ontology('nif_cell:').
anatomical_ontology('nif_subcellular:').
anatomical_ontology('nif_molecule:').
