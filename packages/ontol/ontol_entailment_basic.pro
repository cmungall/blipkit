:- module(ontol_entailment_basic,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

% semi-deprecated
% parent/2 and parent/3 now take into account genus/diff

:- multifile ontol_db:subclass/2.
:- multifile ontol_db:restriction/3.
ontol_db:subclass(C,G):- genus(C,G).
ontol_db:restriction(C,R,To):- differentium(C,R,To).
ontol_db:restriction(ID,domain,X):- property_domain(ID,X).
ontol_db:restriction(ID,range,X):- property_range(ID,X).
%ontol_db:restriction(ID,xref,X):- entity_xref(ID,X).

