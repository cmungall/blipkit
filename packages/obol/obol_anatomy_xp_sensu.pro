
:- multifile anatomical_continuant/3,gross_anatomical/3,term_label/3.

anatomical_continuant(C that part_of(S)) --> gross_anatomical(C),['('],[sensu],force(organism(S),organism),[')'].

organism(S) --> terminal(S).

