:- module(xgmml_writer,
          [write_xgmml_file/2]
          ).

:- use_module(cytoscape_db).
:- use_module(bio(bioprolog_util)).

io:write_all(xgmml,F,_):-
        write_xgmml_file(F,[]).

write_xgmml_file(F,_Opts) :-
        open(F,write,IO,[]),
        doc( Doc ),
        xml_write(IO,[Doc],[]),
        close(IO).

/*
                    [nsmap([xsi='http://www.w3.org/2001/XMLSchema-instance',
                          ns1='http://www.w3.org/1999/xlink',
                          dc='http://purl.org/dc/elements/1.1/',
                          rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#',
                          base='http://www.cs.rpi.edu/XGMML'])]
                         ),

  */

elt( element(node,
             [id=ID,
              label=Label],
             Atts) ) :-
        call_unique(inferred_node(ID)),
        (   node_attribute_value(ID,label,Label)
        ->  true
        ;   Label=ID),
        solutions( Att,
                   subelt(node(ID),Att),
                   Atts).

elt( element(edge,
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
       element(att,
               [name=A,
                value=V],
               []) ) :-
        node_attribute_value(N,A,V).

subelt(edge(S,R,T),
       element(att,
               [name=A,
                label=Type,
                value=V],
               []) ) :-
        edge_attribute_value(S,R,T,A,V),
        guess_type(V,Type).
subelt(edge(_,R,_),
       element(att,
               [name=interaction,
                value=R],
               []) ).

doc( element(graph,
             [id='1',
              label=test,
              'xmls:xsi'='http://www.w3.org/2001/XMLSchema-instance',
              'xmlsn:ns1'='http://www.w3.org/1999/xlink',
              'xmlns:dc'='http://purl.org/dc/elements/1.1/',
              'xmlns:rdf'='http://www.w3.org/1999/02/22-rdf-syntax-ns#',
              xmlns='http://www.cs.rpi.edu/XGMML'],
             Elts)) :-
        findall(Elt,elt(Elt),Elts).

guess_type(V,float) :- float(V),!.
guess_type(V,integer) :- integer(V),!.
guess_type(_,string).






