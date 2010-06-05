term_expansion(:-(module(_,[Item|Items])), :-(export(Tree))) :-
list_to_commas(Items, Item, Tree).

list_to_commas([], Last, Last).
list_to_commas([Item|Items], First, (First,Rest)) :-
	list_to_commas(Items, Item, Rest).

term_expansion(:- (use_module(Module,Items)), Imports) :-
list_to_imports(Items, Module, Imports).

list_to_imports([], _, []).
list_to_imports([Item|Items], Module,
                [:-(import(from(Item,Module))) | Imports]) :-
list_to_imports(Items, Module, Imports).
