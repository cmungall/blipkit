
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

suffix(' signaling pathway').
suffix(' protein signaling pathway').


gd(C,G,D) :-
	class_label_exact(C,N),
	suffix(S),
	concat_atom([Base,S],N),
	concat_atom([Base,' binding'],N2),
	class_label_exact(D,N2),
	subclass(C,G).
	
	
ontol_db:genus(X,G) :- gd(X,G,_).
ontol_db:differentium(X,starts_with,D) :- gd(X,_,D).

