/* -*- Mode: Prolog -*- */


:- module(parser_nlp,
          [
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(phylo_db)).
:- [parser_general].

io:parse_atom_to_terms(nlp,Atom,Trees):-
        atom_codes(Atom,Codes),
        nlp_nodes(Trees,Codes,[]).
io:parse_stream_to_terms(nlp,IO,Trees):-
        read_stream_to_codes(IO,Codes),
        nlp_nodes(Trees,Codes,[]).

% assumes at least one tree per stream
nlp_nodes_plus([Tree|Trees]) --> nlp_node(Tree),!,ws_star,nlp_nodes(Trees).

nlp_nodes([Tree|Trees]) --> nlp_node(Tree),!,ws_star,nlp_nodes(Trees).
nlp_nodes([]) --> [].

nlp_node(node(nt,Type,SubNodes)) -->
        [0'(],not_ws_atom(Type),ws_plus,nlp_nodes_plus(SubNodes),!,[0')].
nlp_node(node(t,Type,Data)) -->
        [0'(],not_ws_atom(Type),ws_plus,not_wsts_atom(Data),!,[0')].

% not whitespace or token
not_wsts_atom(A) --> not_wsts(Cs),{Cs\=[],atom_codes(A,Cs)}.
not_wsts([C|Cs]) --> not_wst(C),!,not_wsts(Cs).
not_wsts([]) --> [].

not_wst(C) --> [C],{\+ws(C),C \= 0'(,C \= 0')}.


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test_nlp)=
      (   open('test.nlp',read,Stream,[]),
          io:parse_stream_to_terms(nlp,Stream,Terms))/Terms).
        
unittest(test(load_nlp_file,
             [Nodes=load(test_nlp)],
             (   writeln(Nodes)),
            true)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2005/07/11 19:59:01 $
  @license LGPL

  
  ---+ Name
  ---++ parser_nlp
- new hampshire extended format for phylogeny data

  ---+ Synopsis
  
  ==
  :- use_module(bio(parser_nlp)).
  ==

  ---+ Description

  Parses the output of lexical parsers; typically looks like this:
  
  ==
  (S
    (NP (NNS Hemizygotes))
    (VP (VBP die)
      (PP (IN as)
        (NP (NN pupae))))
    (. .))
  ==

  Generates a tree of nodes:

  @c
  Node = node(nt,Type,SubNodes) | node(t,Type,Word)
  
  
  */