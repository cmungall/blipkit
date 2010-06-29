:- multifile anatomical_continuant/3,gross_anatomical/3,term_label/3.

gross_anatomical(Part that part_of(Whole)) --> force(gross_anatomical5(Part),anatomy),[of],[the],force(gross_anatomical(Whole),anatomy).
