:- module(ontol_entailment_class_inverse,[]).

:- use_module(bio(ontol_db)).

% bit hacky: inverses have to be stated exactly the right way to avoid cycles
% consider using ontol_reasoner instead

:- multifile ontol_db:restriction/3.
ontol_db:restriction(C,RI,To):- inverse_of_on_instance_level(R,RI),restriction(To,R,C).
