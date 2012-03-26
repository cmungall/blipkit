:- module(xgmml_writer,
          [write_xgmml_file/2]
          ).

:- use_module(cytoscape_db).

io:write_all(xgmml,_,_):-
        write_xgmml_file(F,[]).

write_xgmml_file(F,_Opts) :-
        open(F,write,IO,[]),
        doc( Doc ),
        xml_write(IO,[Doc],[nsmap([])]),
        close(IO).

elt( element('http://www.cs.rpi.edu/XGMML':node,
             [id=ID,
              label=Label],
             Atts) ) :-
        node(ID),
        (   node_attribute_value(ID,label,Label)
        ->  true
        ;   Label=ID),
        findall( Att,
                 subelt(node(ID),Att),
                 Atts).

elt( element('http://www.cs.rpi.edu/XGMML':edge,
             [id=ID,
              label=ID,
              source=S,
              target=T],
             Atts) ) :-
        edge(S,R,T),
        atomic_list_concat([S,R,T],' ',ID),
        findall( Att,
                 subelt(edge(S,R,T),Att),
                 Atts).

subelt(node(N),
       element('http://www.cs.rpi.edu/XGMML':att,
               [name=A,
                value=V],
               []) ) :-
        node_attribute_value(N,A,V).

subelt(edge(S,R,T),
       element('http://www.cs.rpi.edu/XGMML':att,
               [name=A,
                value=V],
               []) ) :-
        edge_attribute_value(S,R,T,A,V).
subelt(edge(_,R,_),
       element('http://www.cs.rpi.edu/XGMML':att,
               [name=interaction,
                value=R],
               []) ).

doc( element('http://www.cs.rpi.edu/XGMML':graph,
             [],
             Elts)) :-
     findall(Elt,elt(Elt),Elts).




