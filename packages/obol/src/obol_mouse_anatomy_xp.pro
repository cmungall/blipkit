:- multifile anatomical_continuant/3,gross_anatomical/3,term_label/3,spatial/3.

gross_anatomical(Part that part_of(Whole)) --> gross_anatomical5(Whole),[muscle],{class_label_exact(Part,'musculature')}.
