
excluded(has_part).
has_a_parent(ID,T):-
        parent_over_nr(T,ID,_),
        !.

term_lineage_boxes_panel(ID) =>
 doc:'horizontal panel, each panel a lineage tree',
 div(class=block,id=tree,
     h2('Term Lineage (by relation)'),
     table(class=term_lineage_table,
           border=1,
           call(solutions(T,(is_transitive(T),\+excluded(T),has_a_parent(ID,T)),Ts)),
           tr([map(T,
                   td(term_lineage_box(T,ID)),
                   [subclass|Ts])]))),
 div(class=block,id=tree,
     h2('Logical Definition'),
     table(class=term_lineage_table,
           border=1,
           tr(findall(td(genus_lineage_box(GID)),
                      genus(ID,GID)),
              findall(td(differentium_lineage_box(T,To)),
                      differentium(ID,T,To))))).

term_lineage_box(T,ID) =>
 doc:'shows lineage of a term ID over type T',
 div(class=term_lineage_box,
     call(relname(T,TN)),
     span(class=col_heading,b(TN)),
     call((closure_to_edgelist(parent_over_nr(T,_),ID,Edges),
           edgelist_to_trees(Edges,Trees))),
     div(class=treeview,
         map(Tree,term_lineage_list(T,Tree),Trees))).

genus_lineage_box(ID) =>
 doc:'shows lineage of a genus ID',
 div(class=term_lineage_box,
     span(class=col_heading,b(i('generic term'))),
     call((closure_to_edgelist(parent_over_nr(subclass,_),ID,Edges),
           edgelist_to_trees(Edges,Trees))),
     div(class=treeview,
         map(Tree,term_lineage_list(subclass,Tree),Trees))).

differentium_lineage_box(T,ID) =>
 doc:'shows lineage of a differetium T,ID',
 div(class=term_lineage_box,
     call(relname(T,TN)),
     span(class=col_heading,b(i('discriminating characteristics: '),TN)),
     call((closure_to_edgelist(parent_over_nr(subclass,_),ID,Edges),
           edgelist_to_trees(Edges,Trees))),
     div(class=treeview,
         map(Tree,term_lineage_list(T,Tree),Trees))).

term_lineage_list(T,node(R,ID,Nodes)) =>
 doc:'shows lineage tree',
 ul(%class=treeview,
    li(dag_rel_term('',ID),relationship_expl(R),
       map(Node,term_lineage_list(T,Node),Nodes))).

relationship_expl(parent_over_nr(T,Via)) =>
 if(Via=direct,
    then:if(T=subclass,then:[],else:b(direct)),
    else:show_via(Via)).

relationship_expl(_) => [].

show_via(subclassRT(ID)) =>
 ' via is_a:',href_id_as_label(term,ID).
show_via(parent(R,ID)) =>
 ' via :',noesc(R),href_id_as_label(term,ID).
