/* -*- Mode: Prolog -*- */


:- module(parser_nhx,
          [
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).
:- use_module(bio(phylo_db)).
:- [bio(parser_general)].

io:parse_stream(nh,IO):-
        io:parse_stream(nhx,IO).
io:parse_stream(nhx,IO):-
        parse_stream_to_terms(IO,Trees),
        load_trees(Trees).

parse_stream_to_terms(IO,Trees):-
        read_stream_to_codes(IO,Codes),
        nhx_trees(Trees,Codes,[]).

load_trees(Trees):-
        forall(member(Tree,Trees),
               load_tree(Tree)).

load_tree(Node):-
        gensym(tree,TreeID),
        asserta(phylo_db:phylotree(TreeID)),
        gensym(root,Root),
        asserta(phylo_db:phylonode_tree(Root,TreeID)),
        load_node(TreeID,Root,Node).
load_node(TreeID,ParentID,Node):-
        Node = node(_,Name,Children,Len,Props),
        gensym(node,ID),
        asserta(phylo_db:phylonode(ID)),
        asserta(phylo_db:phylonode_tree(ID,TreeID)),
        asserta(metadata_db:entity_label(ID,Name)),
        asserta(phylo_db:phylonode_parent(ID,ParentID,Len)),
        forall(member(K=V,Props),
               (   K='Ev'
               ->  parse_ev(V,ID)
               ;   asserta(phylo_db:phylonodeprop(ID,K,V)))),
        (   member('T'=Tax,Props)
        ->  asserta(phylo_db:phylonode_taxon(ID,Tax))
        ;   concat_atom([_,Tax],'_',Name)
        ->  asserta(phylo_db:phylonode_taxon(ID,Tax))
        ;   true),
        forall(member(Child,Children),load_node(TreeID,ID,Child)),
        !.
load_node(ParentID,Node):-
        throw(failed(ParentID,Node)).

parse_ev(Ev,ID) :-
        concat_atom([Dup,Spec|_],'>',Ev), % TODO - rest
        atom_number(Dup,NumDup),
        atom_number(Spec,NumSpec),
        asserta(phylo_db:phylonode_duplications(ID,NumDup)),
        asserta(phylo_db:phylonode_speciations(ID,NumSpec)).

% assumes at least one tree per stream
% trees --> (node ';')+
nhx_trees([Tree|Trees]) --> nhx_tree(Tree),!,{debug(nhx,'Tree: ~w',[Tree])},nhx_trees(Trees).
nhx_trees([]) --> !,[].
nhx_tree(Tree) --> nhx_node(Tree),!,record_separator.

%% nodes --> (node ',')* node
nhx_nodes([Node|Nodes]) --> nhx_node(Node),",",!,nhx_nodes(Nodes).
nhx_nodes([Node]) --> nhx_node(Node).

%% node --> '(' nodes ')' branch props
%% Node = node(IsTerminal,Name,SubNodes,BranchLen,Props)
nhx_node(node(nt,'',Nodes,BrLen,Props)) -->
        [0'(],!,nhx_nodes(Nodes),[0')],branch_tag(BrLen),nhx_props(Props).
nhx_node(node(t,Name,[],BrLen,Props)) -->
        nodename(Name),!,{debug(nhx,'Term: ~w',[Name])},branch_tag(BrLen),nhx_props(Props).

% NH
nh_props(Prop|Props) --> nh_prop(Prop),!,nh_props(Props).
nh_props(Props) --> nhx_props(Props).
nh_props([]) --> [].

% NHX - must be preceeded by special codes:
nhx_props(Props) --> "[&&NHX",!,nhx_props1(Props),"]".
nhx_props([]) --> [].
nhx_props1([Prop|Props]) --> nhx_prop(Prop),!,nhx_props1(Props).
nhx_props1([]) --> [].

nh_prop(Prop) --> nhx_prop(Prop).
nhx_prop(Key=Val) --> ":",keyname(Key),"=",!,nhx_value(Val).

keyname(Atom) --> alphanumerics(Codes), {atom_codes(Atom,Codes)}.
nhx_value(Atom) --> nhx_valuecodes(Codes), {atom_codes(Atom,Codes)}.
nodename(Atom) --> namecodes(Codes),{atom_codes(Atom,Codes)}.

branch_tag(Len) --> ":",fnumber(Len),!.
branch_tag(0) --> "".

record_separator --> ";\n",!.
record_separator --> ";".

nhx_valuecodes([H|T]) --> nhx_valuecode(H), !, nhx_valuecodes(T).
nhx_valuecodes([]) --> [].

nhx_valuecode(A) --> [A],{A\=0'],A\=0'\:}.

namecodes([H|T]) --> namecode(H), !, namecodes(T).
namecodes([]) --> [].

namecode(A) --> alphanumeric(A).
namecode(A) --> [A],{A=0'.}.
namecode(A) --> [A],{A=0'-}.



% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test_nhx)=
      load_biofile(nhx,'test.nhx')/[]).
        
unittest(test(load_nhx_file,
             [_=load(test_nhx)],
             (   ensure_loaded(bio(phylo_db)),
                 findall(ID-Dist,
                         (   phylonode(ID,N,PID,BLen),
                             findall(P=V,phylonodeprop(ID,P,V),PVs),
                             phylonode_depth(ID,Dist),
                             writeln(node(ID,N,PVs,PID,BLen,Dist))),
                         IDDists),
                 length(IDDists,NumNodes)),
            (NumNodes=24))).

unittest(test(load_nhx_file,
             [_=load(test_nhx)],
             (   ensure_loaded(bio(phylo_db)),
                 phylonode(ID,'ADH3',_,_),
                 phylonodeprop(ID,'S',yeast),
                 phylonode_depth(ID,Depth)),
            (Depth=0.23))).


%test(X):-  nhx_trees(X,"(((ADH2:0.1[&&NHX:S=human:E=1.1.1.1],ADH1:0.11[&&NHX:S=human:E=1.1.1.1]):0.05[&&NHX:S=Primates:E=1.1.1.1:D=Y:B=100],ADHY:0.1[&&NHX:S=nematode:E=1.1.1.1],ADHX:0.12[&&NHX:S=insect:E=1.1.1.1]):0.1[&&NHX:S=Metazoa:E=1.1.1.1:D=N],(ADH4:0.09[&&NHX:S=yeast:E=1.1.1.1],ADH3:0.13[&&NHX:S=yeast:E=1.1.1.1],ADH2:0.12[&&NHX:S=yeast:E=1.1.1.1],ADH1:0.11[&&NHX:S=yeast:E=1.1.1.1]):0.1[&&NHX:S=Fungi])[&&NHX:E=1.1.1.1:D=N];",[]).
%test(X):-  nhx_trees(X,"(((hADH2:0.1,hADH1:0.11):0.05,nADHY:0.1,iADHX:0.12):0.1,(yADH4:0.09,yADH3:0.13,yADH2:0.12,yADH1:0.11):0.1);",[]).
/** <module>   parser for new hampshire extended format for phylogeny data

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).
  :- use_module(bio(phylo_db)).

  % show height of all trees in file
  demo:-
    load_biofile(nhx,'test.nhx'),
    forall(phylotree(Node),
           (   phylonode_height(Node,Height),
               format('tree ~w has height ~w~n',[Node,Height]))).
  ==

  ---+ Description

  see

  <http://www.genetics.wustl.edu/eddy/forester/NHX.html> NHX

@author  Chris Mungall
@version $Revision$
@see     README
@license License

  
  
  */
