:- use_module(obolog_db,[type_type/1,class_instance_relation_pair/2]).

missing(R):-
        type_type(R),
        \+ class_instance_relation_pair(R,_).
