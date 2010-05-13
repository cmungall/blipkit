:- use_module(bio(graph)).

xp_matrix =>
 doc:'matrix',
 outer('ODB - Cross-Product Matrix',
       html:div(class=main,
                call((   solutions(G,(genus(_,G1),subclassRT(G1,G)),Gs),
                         solutions(DC,(differentium(G,_,DC1),subclassRT(DC1,DC)),DCs),
                         RowClasses=Gs,
                         ColClasses=DCs,
                         closure_to_edgelist(subclass,RowClasses,RowEdges),
                         edgelist_to_trees(RowEdges,RowTrees)
                         )),
                onto_xp_matrix(RowTrees,ColClasses,[row(genus)]))).

onto_xp_matrix(RowTrees,ColClasses,Opts) =>
 doc:'cross-product matrix',
 table(class=xp_matrix,
       onto_xp_matrix_header(ColClasses,Opts),
       map(RowTree,onto_xp_matrix_row(0,RowTree,ColClasses,Opts),RowTrees)).

onto_xp_matrix_header(ColClasses,Opts) =>
 doc:'header',
 tr(if(member(row(genus),Opts),
       then: th('rows=genus\\cols=diff'),
       else: th('rows=diff\\cols=genus')),
    map(ColClass,
        th(href_id_as_label(term,ColClass)),
        ColClasses)).
        
onto_xp_matrix_row(Indent,node(_,RowClass,SubNodes),ColClasses,Opts) =>
        call(Indent1 is Indent+1),
        tr(th(indent(Indent1),
              href_id_as_label(term,RowClass)),
           call(member(row(RowFor),Opts)),
           map(ColClass,
               onto_xp_matrix_cell(RowClass,ColClass,RowFor),
               ColClasses),
           map(Node,onto_xp_matrix_row(Indent1,Node,ColClasses,Opts),SubNodes)).

onto_xp_matrix_cell(RowClass,ColClass,genus) =>
        onto_xp_matrix_cell_1(RowClass,ColClass).
onto_xp_matrix_cell(RowClass,ColClass,_) =>
        onto_xp_matrix_cell_1(ColClass,RowClass).
onto_xp_matrix_cell_1(Genus,DiffClass) =>
        td(findall(div(class=xp_cell,
                       href_id_as_label(term,DefinedClass),
                       ' ',
                       span(class=relation_small,href_id_as_label(term,R)),
                       findall(['; also with: ',
                                span(class=relation_small,href_id_as_label(term,R2)),
                                '(',href_id_as_label(term,DiffClass2),')'],
                               (   differentium(DefinedClass,R2,DiffClass2),
                                   \+ ((R2=R,DiffClass=DiffClass2)))),
                       hr),
                   (   genus(DefinedClass,Genus),
                       differentium(DefinedClass,R,DiffClass)))).

indent(0) => [].
indent(N) => '#',call(N1 is N-1),indent(N1).

/************************************************************
  CONTROLLER
************************************************************/

strans(main,S,
       % pre: list of term IDs selected, posted
       (   submit_param(S,post_results),
           getparam(S,data_class,term),
           getparam(S,action,A),
           (A=replace_tree ; A=plus_node)),
       % post:
       post_results(S,A,IDL,IDopenL),
       % change:
       add([[open_ids,IDopenL],[highlighted_ids,IDL]])).

