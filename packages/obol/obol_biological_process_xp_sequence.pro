:- [bio(obol_biological_process_xp_chemical)].

:- multifile chemical/3.
:- multifile process/3, process5/3.
:- multifile term_label/3.

% experiment: see what happens if we pretend all sequences are chemical entity like objects
% eg RNA transport, localization
% miRNA binding
chemical(X) --> biosequence(X).

% miRNA-mediated gene silencing, mRNA cleavage

% mRNA cleavage : Any process by which a pre-mRNA or mRNA molecule is cleaved at specific sites or in a regulated manner
process5(P that results_in_division_of(S)) --> biosequence(S),[cleavage],{class_label_exact(P,'biopolymer metabolic process')}.


/****************************************

  RNA editing
  
  ****************************************/

process5(P) --> editing(P).
% mRNA editing
editing(P that results_in_change_to(Nuc that part_of(S))) --> biosequence(S),[editing],{class_label_exact(P,'biopolymer metabolic process'),class_label_exact(Nuc,nucleotide)}.

% cytidine to uridine editing
editing(P that results_in_change_to(N1) and results_in_transformation_to(N2)) --> base(N1),[to],base(N2),[editing],{class_label_exact(P,'biopolymer metabolic process')}.

% tRNA wobble adenosine to inosine editing
editing(P that results_in_change_to(Wob that part_of(S)) and results_in_transformation_to(Base)) --> biosequence(S),wobble(Wob),[to],base(Base),[editing],{class_label_exact(P,'biopolymer metabolic process')}.

/****************************************

  splicing
  
  ****************************************/

process5(P that results_in_removal_of(Part that part_of(Whole))) --> biosequence(Whole),[splicing],{class_label_exact(P,'biopolymer metabolic process'),class_label_exact(Part,region)}.

/****************************************

  transcription
  
  ****************************************/


% mRNA transcription
process5(P that results_in_formation_of(S)) --> biosequence(S),transcription(P).
transcription(P) --> any_kind_of(P,transcription).

/****************************************

  modifications
  
  ****************************************/

% ncRNA polyadenylation
process(P that results_in_addition_of(PolyA) and results_in_addition_to(RNA)) --> biosequence(RNA),[polyadenylation],{class_label_exact(P,'metabolic process'),class_label_exact(PolyA,'polyA_sequence')}.
% tRNA wobble modification
% tRNA wobble uridine modification
process(P that results_in_change_to(Wob that part_of(RNA))) --> biosequence(RNA),wobble(Wob),[modification],{class_label_exact(P,'metabolic process')}.

% x-tRNA aminoacylation
% TODO

% mRNA capping
process(P that results_in_connection_of(Cap) and results_in_connection_of(S)) --> biosequence(S),[capping],{class_label_exact(P,'metabolic process'),class_label_exact(Cap,cap)}.

term_label(Wob) --> wobble(Wob).
%term_label(X that part_of(Y)) --> terminal(X),terminal(Y). 

wobble(Wob) --> [wobble],[base],{class_label_exact(Wob,'wobble_base_pair')}.
wobble(Wob) --> [wobble],{class_label_exact(Wob,'wobble_base_pair')}.
wobble(Wob that connected_to(Base)) --> [wobble],base(Base),{class_label_exact(Wob,'wobble_base_pair')}.

base(Base) --> chemical(Base).
