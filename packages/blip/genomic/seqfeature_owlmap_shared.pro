/* -*- Mode: Prolog -*- */


% j(Seq,Pos,Strand)
junction_uri(J,ID) :- J=..L,concat_atom(L,'__',ID).
