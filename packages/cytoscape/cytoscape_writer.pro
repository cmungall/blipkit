:- module(cytoscape_writer,
          [write_cytoscape_file/2]
          ).

write_cytoscape_file(Base+Fmt) :-
        atomic_list_concat([Base,'.',Fmt],F),
        write_cytoscape_file(F,Fmt).

write_cytoscape_file(F,Type) :-
        \+ is_stream(F),
        !,
        open(F,write,IO,[]),
        write_cytoscape_file(IO,Type),
        close(IO).
write_cytoscape_file(IO,na) :-
        forall(node_attribute(A),
               write_node_attributes(IO,A)).
write_cytoscape_file(IO,ea) :-
        forall(node_attribute(A),
               write_edge_attributes(IO,A)).
write_cytoscape_file(IO,sif) :-
        E=edge(_,_,_),
        forall(E,
               write_edge(IO,E)).

write_cytoscape_files(Base) :-
        write_cytoscape_file(Base+na),
        write_cytoscape_file(Base+ea),
        write_cytoscape_file(Base+sif).

write_node_attributes(IO,A) :-
        format('~w~n',[A]),
        forall(node_attribute_value(N,A,V),
               format('~w = ~w',[N,V])).
write_edge_attributes(IO,A) :-
        format('~w~n',[A]),
        forall(edge_attribute_value(N,A,V),
               format(IO,'~w\t(~w)\t~w = ~w\n',[S,R,T,V])).


write_edge(IO,edge(S,R,T)) :-
        format(IO,'~w\t~w\t~w\n',[S,R,T]).
write_edge(IO,edge(S,R,T,_)) :-
        write_edge(IO,edge(S,R,T)).

write_edge_attr(IO,A,edge(S,R,T,A,V)) :-
        format(IO,'~w\t(~w)\t~w = ~w\n',[S,R,T,V]).



