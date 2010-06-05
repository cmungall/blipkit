:- module(interaction_from_go_annot,[]).

:- use_module(bio(interaction_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).

% blip -table_pred ontol_db:subclassT/2 io-convert -r go_assoc_local/fb -u interaction_from_go_annot -r go -to interaction_db:pro
interaction_db:interacts_with(G1,G2) :-
        curation_statement(A,G1,_,MF),
	subclassRT(MF,'GO:0005515'),
	curation_evidence(A,E),
	evidence_with(E,G2).


