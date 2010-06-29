:- multifile term_label/3.
term_label(P) --> quality(P).

relational_quality(Q) --> any_kind_of(Q,'relational quality of continuant').
relational_quality(Q) --> any_kind_of(Q,'relational quality of occurrent').

qualifier(Q) --> terminal(Q),{belongs(Q,quality),class_label_exact(Q,abnormal)}.
quality(Q) --> terminal(Q),{belongs(Q,quality),\+class_label_exact(Q,abnormal),\+class_label_exact(Q,absent)}.


