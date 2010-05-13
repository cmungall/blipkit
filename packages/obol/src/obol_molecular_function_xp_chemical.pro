:- [bio(obol_biological_process_xp_chemical)].

:- multifile molecular_function/3,molecular_function5/3.
:- multifile term_label/3.
:- multifile term_textdef/3.

% See http://wiki.geneontology.org/index.php/Docs

term_label(P) --> molecular_function(P).
term_textdef(P) --> def(molecular_function(P)).

% methotrexate binding
molecular_function(P that results_in_binding_of(X)) --> binding(P),[of],continuant(X).
molecular_function(P that results_in_binding_of(X)) --> continuant(X),binding(P).
binding(P) --> [binding],{class_label_exact(P,binding)}. % used in: binding of sperm to zona pellucida

% glutathione transferase activity
molecular_function(P that has_input(X)) --> continuant(X),molecular_function(P).

% cardiolipin synthase activity
%molecular_function(F that executes(P that results_in_formation_of(X))) --> chemical(X),[synthase],[activity],{class_label_exact(F,'molecular_function'),class_label_exact(P,'biosynthetic process')}.
%%%% molecular_function(F that results_in_formation_of(X)) --> chemical(X),[synthase],[activity],{class_label_exact(F,'molecular_function')}.

% versicolorin reductase activity [DEF: "Catalysis of the reduction of versicolorin A to sterigmatocystin."]
% TODO: overlap with 
molecular_function(F that reduces(X)) --> chemical(X),[reductase],[activity],{class_label_exact(F,'molecular_function')}.


% (remember and get these in the same order as the corresponding processes! of then to/from)
% histone methylase activity
molecular_function(F that executes(P that results_in_addition_of(G) and results_in_addition_to(X))) -->
        chemical(X),[Enzyme],[activity],{reaction_element(PN,Enzyme,+,GN),debug(obol,'CAT: ~w',[PN-Enzyme-GN]),class_label_exact(G,GN),reaction_class_label(P,PN),class_label_exact(F,'catalytic activity')}.
% histone demethylase activity [todo: check why demethylation not in go]
molecular_function(F that executes(P that results_in_removal_of(G) and results_in_removal_from(X))) -->
        chemical(X),[Enzyme],[activity],{reaction_element(PN,Enzyme,-,GN),debug(obol,'CAT: ~w',[PN-Enzyme-GN]),class_label_exact(G,GN),reaction_class_label(P,PN),class_label_exact(F,'catalytic activity')}.

enzyme_class_label(P,_L):- class_label_exact(P,'catalytic activity').

% coenzyme transporter activity
molecular_function(F that executes(P that results_in_transport_of(X))) --> continuant(X),[transporter],[activity],{class_label_exact(P,transport),class_label_exact(F,'molecular_function')}.
% carbohydrate transmembrane transporter activity : "Catalysis of the transfer of carbohydrate from one side of the membrane to the other."
molecular_function(F that executes(P that results_in_transport_of(X) and results_in_transport_through(M))) -->
        continuant(X),[transmembrane],[transporter],[activity],{class_label_exact(P,transport),class_label_exact(F,'molecular_function'),class_label_exact(M,membrane)}.
% todo: uniporter. how is this differentiated from transmembrane transporter?



% mannose:hydrogen symporter activity
%  Catalysis of the transfer of a solute or solutes from one side of a membrane to the other according to the reaction: mannose + H+ = mannose + H+.
% tartrate:succinate antiporter activity
%  Catalysis of the transfer of a solute or solutes from one side of a membrane to the other according to the reaction: tartrate(out) + succinate(in) = tartrate(in) + succinate(out).
% todo: explicitly mention membrane?
% what about direction? we rely on genus for that
molecular_function(F that executes(F that results_in_transport_of(X)) and executes(F that results_in_transport_of(Y))) --> chemical(X),[':'],chemical(Y),porter(F).
porter(F) --> any_kind_of(F,'symporter activity').
porter(F) --> any_kind_of(F,'antiporter activity').

% GO:0043804 imidazolone hydrolase activity [DEF: "Catalysis of the reaction: N-formiminoglycine = imidazolone + H2O."] typo??

% GO:0043836 xanthine hydrolase activity [DEF: "Catalysis of the reaction: xanthine + H2O = 4-ureido-5-imidazole carboxylate."]
% todo: this is not quite right; not specific at the level of the actual bond....
molecular_function(F that executes(P that results_in_division_of(C)) and depends_on(Water)) -->
        chemical(C),[hydrolase],[activity],{class_label_exact(F,'molecular_function'),class_label_exact(P,'biological_process'),class_label_exact(Water,'water')}.


% DEFINITION
% eg "Catalysis of the reaction: ATP + L-lysine + tRNA(Lys) = AMP + diphosphate + L-lysyl-tRNA(Lys)."
% - this rule can be used for parsing
def(molecular_function(F that IDiffs and ODiffs)) -->
        ['Catalysis',of,the,reaction,':'],inputs(IDiffs),['='],outputs(ODiffs),opt_end,{class_label_exact(F,'catalytic activity')}.


inputs(X and Y) -->
        input(X),['+'],inputs(Y).
inputs(X) -->
        input(X).
input(has_input(N,X)) --> num(N),fchemical(X).
input(has_input(X)) --> fchemical(X).

outputs(X and Y) -->
        output(X),['+'],outputs(Y).
outputs(X) -->
        output(X).
output(has_output(N,X)) --> num(N),fchemical(X).
output(has_output(X)) --> fchemical(X).

%fchemical(C) --> force(terminal(C),chemical).
fchemical(C) --> chemical(C).
fchemical(C) --> protein(C).
% protein tyrosine phosphate
% fchemical(C) --> TODO.

opt_end --> ['.'],!,ignore.
opt_end --> !,[].
ignore --> [X],{nonvar(X)},!,ignore.
ignore --> !,[].

num(1) --> ['a'].
num(N) --> [A],{catch(atom_number(A,N),_,fail)}.


