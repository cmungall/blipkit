:- multifile phenotype/3.

% patterns specific to MP grammar:
% (we separate many of these because they slow down processing)

% absent eyelids
phenotype(Q that towards(P)) --> [absent],[Plural],{nonvar(Plural),atom_concat(Singular,s,Plural),class_label_exact(P,Singular),class_label_exact(Q,'lacking physical part')}.
