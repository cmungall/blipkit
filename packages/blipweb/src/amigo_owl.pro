%:- rewrite(so:0001100,so:gene).

:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(bio(serval_rdf)).


:- rdf_register_ns(swrl,'http://www.blah.org/swrl#').

:- consult(amigo_core).

% serval params
form_method('GET').
init_page(main).

% amigo params
data_class(class).
% this is overridden in components
data_class_by_ont(_Ont,_DataClass):- fail.
data_class_window_size(class,100).

/************************************************************
  search
************************************************************/

amigo_query(class,search(Search),IDs):-
        findall(ID,
                rdfs_find(Search,class(_Class),_Props,substring,ID),
                IDs).

% exact substring word prefix like
mysearch(Search,Method,ID):-
        rdfs_find(Search,class(_Class),_Props,Method,ID).

/************************************************************
  rdf wrappers
************************************************************/

rdfs_direct_subclass_of(C,P):- rdf_has(C,rdfs:subClassOf,P).
rdf_type(R,T):- rdf_has(R,rdf:type,T).
obo_has_synonym(C,S):- rdf_has(C,oboInOwl:has_synonym,S).
owl_on_property(R,P):- rdf_has(R,owl:onProperty,P).
owl_some_values_from(R,C):- rdf_has(R,owl:someValuesFrom,C).

/************************************************************
  Main View
************************************************************/

main => page_dag.
entry_page(page_dag,'DAG').
page_dag =>
 doc:'TOP: Main OBD page: query box plus onto tree',
 outer('AmiGO-OWL',
       html:div(class=main,
                basic_query_form,
                rdf_expandable_tree)).

search_result_headers(term,summary,
                      tr(th([]),th('ID'),th('Name'),th('Cross-Product'))).

search_result_id_view(class,summary,ID) =>
  td(ID :: detail).

search_result_id_view(class,detail,ID) =>
  td(ID :: detail).

detail_page(class) =>
 doc:'TOP: details on one (or more?) terms',
 session(S),
 call((getparam(S,id,ID),
       (rdfs_label(ID,N)->true;N=''))),
 outer(['OBD: Term report for ',ID,'-',N],
       div(class=main,
           basic_query_form,
           ID :: detail)).

/************************************************************
  Tree explorer
************************************************************/

owl_link(Subj,Rel,Obj):-
        rdf_has(Subj,rdfs:subClassOf,Restriction),
        rdfs_individual_of(Restriction,owl:'Restriction'),
        rdf_has(Restriction,owl:someValuesFrom,Obj),
        rdf_has(Restriction,owl:onProperty,Rel).
owl_link(Subj,is_a,Obj):-
        rdf_has(Subj,rdfs:subClassOf,Obj).

rdf_root(Resource):-
        rdfs_individual_of(Resource,rdfs:'Class'),
        \+ owl_link(Resource,_,_).

rdf_expandable_tree =>
 doc:'expandable tree view of ontology',
 div(class=tree,
     getparam(open_ids,OpenResources),
     sform(dag,[],
           rdf_expandable_tree_node(root,Resource,OpenResources) forall rdf_root(Resource))).

rdf_expandable_tree_node(RelToParent,Resource,OpenResources) =>
 doc:'expandable node in DAG plus recursive children',
 ul(class=treeview,
    li(if(member(Resource,OpenResources),
          then:[                % Resource has been explicitly opened
                ilink(img_tree_down,close_id=Resource),
                rdf_link(RelToParent,Resource),
                ufindall(rdf_expandable_tree_node(Rel,ChildResource,OpenResources),
                         rdf_has(ChildResource,Rel,Resource))
               ],
          else:[
                if(\+(rdf_has(_,_,Resource)),
                   then:img_tree_leaf,
                   else:ilink(img_tree_right,open_id=Resource)),
                rdf_link(RelToParent,Resource),
                if(member(-Resource,OpenResources),
                   then:        % Resource is in search path
                  ufindall(rdf_expandable_tree_node(Rel,ChildResource,OpenResources),
                           (   member(-ChildResource,OpenResources),
                               rdf_has(ChildResource,Rel,Resource))))
               ])
      )).

rdf_link(Rel,Resource) =>
 doc:'show term with parent rel type',
 font(size='-2',data(Rel)),
 Resource :: link.

                
/************************************************************
  Helper sfuncs
************************************************************/

resource(_Resource) => foo.
zoom(Text,Resource) => ilink(Text,goto=detail_page,id=Resource).

/************************************************************
  Vanilla RDF interfacce
************************************************************/

%_Resource a owl:'Thing' :: detail ==>
%  foo.

Resource a rdfs:'Resource' :: detail ==>
  doc:'Instance, default description',
  div(class=instance_info,
      h1(generic),
      ul(tagval('ID',Resource :: link),
         tagval('Name',Label) forall rdfs_label(Resource,Label),
         tagval('Type',Type :: link) forall rdf_type(Resource,Type),
         Resource :: extra_tagvals),
      Resource :: attached_detail).

% generic links, one-step
%Resource a rdf:'Description' :: extra_tagvals ==>
%  tagval(Rel :: link, To :: link) forall rdf_has(Resource,Rel,To).

Resource a rdfs:'Resource' :: extra_tagvals ==>
  tagval(Rel :: link, To :: link) forall rdf_has(Resource,Rel,To),
  tagval(From :: link, Rel :: link) forall rdf_has(From,Rel,Resource).

% construct URL
Resource a rdf:'Description' :: link ==>
  zoom(Resource :: label,
       Resource).

Resource a rdfs:'Resource' :: link ==>
  zoom(Resource :: label,
       Resource).

%Resource a rdf:'Description' :: label ==>
%  if(rdfs_label(Resource,Label),
%     then: Label,else:Resource).

Resource a rdfs:'Resource' :: label ==>
  if(rdfs_label(Resource,Label),
     then: Label,else:Resource).

Restriction a owl:'Restriction' :: label ==>
  [' onP: ',Property :: label] where owl_on_property(Restriction,Property),
  [' svf: ',To :: label] where owl_some_values_from(Restriction,To).

_Resource a rdf:'Description' :: attached_detail ==>
    [].

_Resource a rdfs:'Resource' :: attached_detail ==>
    [].

literal(type(_,Text)) =>
  data(Text).

literal(type(_Type,Text)) a rdfs:'Literal' :: _ ==>
  data(Text).

literal(lang(_Lang,Text)) a rdfs:'Literal' :: _ ==>
  data(Text).

literal(Text) a rdfs:'Literal' :: _ ==>
  data(Text).

/************************************************************
  *** HOOKS ***
  ************************************************************/

/************************************************************
  Vanilla OWL interface
************************************************************/

Class a owl:'Class' :: detail ==>
    doc:'OWL Class detail page',
    div(class=class_info,
        h1(class),
        ul(tagval('ID',Class :: link),
           tagval('Name',Label) forall rdfs_label(Class,Label),
           tagval('Property',Property :: link) forall rdfs_class_property(Class,Property),
           tagval('DirectSubClassOf',SubClassOf :: link) forall rdfs_direct_subclass_of(Class,SubClassOf),
           tagval('SubClassOf',SubClassOf :: link) forall rdfs_subclass_of(Class,SubClassOf),
           tagval('Synonym',label(Synonym))
          forall obo_has_synonym(Class,Synonym),
           Class :: extra_tagvals),
        Class :: attached_detail).

Property a owl:'Property' :: detail ==>
    doc:'OWL Property detail page',
    div(class=class_info,
        h1(property),
        ul(tagval('ID',resource(Property)),
           tagval('Name',Label) forall rdfs_label(Property,Label),
           tagval('Synonym',label(Synonym))
          forall obo_has_synonym(Property,Synonym),
           Property :: extra_tagvals),
        Property :: attached_detail).

Restriction a owl:'Restriction' :: detail ==>
    div(class=class_info,
        ul(tagval('ID',resource(Restriction)),
           tagval('Name',Label) forall rdfs_label(Restriction,Label),
           tagval('Synonym',label(Synonym))
          forall obo_has_synonym(Restriction,Synonym),
           Restriction :: extra_tagvals),
        Restriction :: attached_detail).

Class a owl:'Class' :: attached_detail ==>
    div(class=instances,
        ul(li(Individual :: detail) forall rdfs_individual_of(Individual,Class))).

 
/************************************************************
  Bio
************************************************************/

Process a obr:pathological_process :: detail ==>
  div(class=genotype_phenotype_association,
      div(class=causes,
          ul(li(X :: short)
            forall rdfs_reachable(Process,obo_rel:has_cause,X))),
      div(class=effects,
          ul(li(X :: short)
            forall rdfs_reachable(Process,obo_rel:has_effect,X)))).

Gene a so:gene :: detail ==>
       div(class=gene,
           Gene :: info,
           tagval('Function',Function :: label)
          forall rdfs_reachable(Gene,obo_rel:has_function,Function)).

/************************************************************
  CONTROLLER
************************************************************/


% DAG management
%  expand a node
strans(main,S,(lgetparam(S,open_id,IDL1),IDL1\=[]),
       (   (getparam(S,open_ids,IDL2)
           ->  append(IDL1,IDL2,IDL)
           ;   IDL=IDL1)),
       add([[open_ids,IDL]])).
%  collapse a node
transition(_X,main,S,S2):-
        lgetparam(S,close_id,IDL1),
        IDL1\=[],
        !,
        (getparam(S,open_ids,IDL2)
        ->  findall(ID,(member(ID,IDL2),not(member(ID,IDL1))),IDL)
        ;   IDL=[]),
        add_session_data(S,[[open_ids,IDL]],S2).
