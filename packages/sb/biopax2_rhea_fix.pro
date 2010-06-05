:- module(biopax1_rhea_fix,[]).

:- use_module(bio(biopax1_db)).
:- use_module(bio(metadata_db)).

% Rhea double up CHEBI
metadata_db:entity_xref(E,ID):- xref(E,X),db(X,'ChEBI'),id(X,ID).


