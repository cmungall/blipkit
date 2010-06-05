:-module(suffixtree,
         [is_stree/1,
          stree_insert/3,
          stree_has/2,
          stree_insert_list/3]).



%% is_stree(+ST)
%
%  succeeds if ST is a suffix tree
is_stree([]).
is_stree([(Key,ST)|T]):-
	is_stree(T),
	is_stree(ST),
	%symbol(Key).
	atom(Key).

%% stree_insert(+SymList,+ST,?NewST)
%
%  Inserts a word into a suffix tree

stree_insert([],T,T).
stree_insert([Hd|Tl],[(Hd,St)|Rest],[(Hd,St2)|Rest]):-
        !,
        stree_insert(Tl,St,St2).
stree_insert(Word,[X|Rest],[X|Rest2]):-
        stree_insert(Word,Rest,Rest2).
stree_insert([Hd|Tl],[],[(Hd,Rest)]):-
        stree_insert(Tl,[],Rest).

%% stree_has(+SymList,+ST)
%  Tests if a string is a substring of some word inserted into the suffix tree
%% stree_has(?SymList,+ST)
%  Generate string is a substring of some word inserted into the suffix tree
stree_has([],[]).
stree_has([Hd|Tl],[(Hd,St)]):-!,stree_has(Tl,St).
stree_has([Hd|Tl],[(Hd,St)|_]):-stree_has(Tl,St).
stree_has(Word,[_|Rest]):-stree_has(Word,Rest).

testst(A,Z):-
        stree_insert([a,b,c],[],X),stree_insert([a,b,d],X,Y),stree_insert([b,c,d],Y,Z),stree_has(A,Z).

%% stree_insert_list(+SymListList,+Tree,?NewTree)
%  Insert a list of words into a suffix tree
%  
stree_insert_list([],T,T).
stree_insert_list([Hd|Tl],T1,T3):-stree_insert(Hd,T1,T2),stree_insert_list(Tl,T2,T3). 

/** <module>

The module implementing the generalized suffix trees of a given depth. A node of a tree (and a tree itself) is represented by a list of children with labels assigned to the edges. @pred{stree_insert/3} inserts a word (a substring) to the tree. @pred{stree_has/2} is testing if a given word is present in a given tree. 

adapted from probelog, by Bartosz Wilczyski*/