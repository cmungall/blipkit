%:- [bio(obol_biological_process_xp)].
:- [bio(obol_obo_xp)].

term_label(P) --> process(P).

:- multifile process/3,process5/3,stimulus/3.
:- multifile def/3.

process5(P that response_to(S)) --> [response],[to],stimulus(S),{class_label_exact(P,'response to stimulus')}.
process5(P that response_to(S)) --> [detection],[of],stimulus(S),{class_label_exact(P,'detection of stimulus')}.
