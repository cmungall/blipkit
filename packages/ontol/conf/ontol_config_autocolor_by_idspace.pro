:- multifile user:graphviz_ontol_param/2.
:- use_module(bio(metadata_db)).

:- dynamic dyn_ont_color/2.


ont_color(X,Col) :-
	dyn_ont_color(X,Col),
	!.
ont_color(X,Col) :-
	nice_color(Col),
	\+ dyn_ont_color(_,Col),
	!,
	format(user_error,'~w~n',[X-Col]),
	assert(dyn_ont_color(X,Col)).

nice_color(lemonchiffon).
nice_color(steelblue2).
nice_color(peru).
nice_color(yellow).
nice_color(hotpink).
nice_color(green).


user:graphviz_ontol_param(node(_),style=filled).
user:graphviz_ontol_param(node(X),fillcolor=Col):- id_idspace(X,Ont),ont_color(Ont,Col).



