:- module(ontol_entailment_inverse,[]).

:- use_module(bio(ontol_db)).

% bit hacky: inverses have to be stated exactly the right way to avoid cycles
% consider using ontol_reasoner instead

:- multifile ontol_db:restriction/3.
%ontol_db:restriction(C,R,To):- inverse_of(RI,R),restriction(To,RI,C).
ontol_db:restriction(C,R,To):- inverse_of(R,RI),restriction(To,RI,C).
