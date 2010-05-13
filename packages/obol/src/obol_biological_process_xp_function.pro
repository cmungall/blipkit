:- [bio(obol_biological_process_xp)].

:- multifile process/3,process5/3.

% regulation of MAPKKK activity
process(P that regulates(F)) --> regulation(P),[of],molecular_function(F).
process(P that negatively_regulates(F)) --> [negative],regulation(P),[of],molecular_function(F).
process(P that positively_regulates(F)) --> [positive],regulation(P),[of],molecular_function(F).

% retinoic acid receptor signaling pathway [need to truncate "binding"?]
process(P that triggered_by(F)) --> molecular_function(F),signal_transduction(P).
signal_transduction(P) --> any_kind_of(P,'signal transduction').
signal_transduction(P) --> [signal],[transduction],{class_label_exact(P,'signal transduction')}.

%  lysyl-tRNA aminoacylation == has_part lysyl-tRNA synthetase [syn: lysine-tRNA ligase]
process(P that has_part(F)) --> [X],['-'],[tRNA],[aminoacylation],{class_label_exact(P,'biological_process'),concat_atom([X,'-',tRNA,' ',synthetase,' ',activity],FN),class_label_exact(F,FN)}.


