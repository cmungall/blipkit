:- use_module(bio(serval)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(query_ontol)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(homol_bridge_from_ontol)).
:- use_module(bio(homol_db)).
:- use_module(bio(ontol_entailment_basic)).

% AmiGO specific useful predicates
:- use_module(amigo_util).
:- [amigo_core].

option_term_view(sources).

% TODO: move
variant_of(X,Y):- inst_rel(X,'OBO_REL:variant_of',Y).
variant_of(X,Y):- inst_rel(X,'OBO_REL:variant_of',Z),entity_xref(Y,Z). % weak
variant_of(X,Y):- inst_rel(X,'OBO_REL:contains_variant_of',Y).
variant_of(X,Y):- inst_rel(X,'OBO_REL:contains_variant_of',Z),entity_xref(Y,Z). % weak

variant_of_homologset(Su,Set):- homol_db:homologset_member(Set,G,_,_),variant_of(Su,G).
homologset_implied_annotation(Set,Ob):- variant_of_homologset(Su,Set),curation_statement(_,Su,_,Ob1),parentRT(Ob1,Ob).
homologset_annotation(Set,Ob):- variant_of_homologset(Su,Set),curation_statement(_,Su,_,Ob).

% OBD:homologene-74943 - Eya1
homol_annot(Set,G1-G2,Q-QN,X1-X1N,X2-X2N):-
        homol_db:homologset_member(Set,G1,_,_),
        homol_db:homologset_member(Set,G2,_,_),
        G1\=G2,
        variant_of(Su1,G1),
        curation_statement(_,Su1,_,Ob1),
        genus(Ob1,Q),
        variant_of(Su2,G2),
        curation_statement(_,Su2,_,Ob2),
        genus(Ob2,Q),
        Ob1\=Ob2,
        differentium(Ob1,_,X1),
        differentium(Ob2,_,X2),
        class(Q,QN),
        class(X1,X1N),
        class(X2,X2N).

% ==================== CONFIG ====================

node_auto_label(Node,Label):- entity_label(Node,Label),!.
node_auto_label(Node,Label):- entity_synonym(Node,Label),!.
node_auto_label(Node,Label):-
        genus(Node,Genus),
        node_auto_label(Genus,GenusLabel),
        findall(DiffLabel,
                (   differentium(Node,R,To),
                    node_auto_label(R,RLabel),
                    node_auto_label(To,ToLabel),
                    concat_atom([RLabel,' ',ToLabel],DiffLabel)),
                DiffLabels),
        concat_atom(DiffLabels,' and ',X),
        concat_atom([GenusLabel,' that ',X],Label),
        !.
node_auto_label(Node,'*'):- is_anonymous(Node),!.
node_auto_label(Node,Node):- !.


% (+,?) semidet
node_type(Node,Type):- inst_of(Node,Type),!.
node_type(Node,Type):- subclass(Node,Type),!.
node_type(_,unknown):- !.




%node_popupbox(Node,PopUp):-
%        sformat(PopUp,'ddrivetip(\'foo=bar ~w\',\'yellow\', 300)',[Node]).



% ==================== VIEW ====================

dtdd(Tag,Val) =>
 dt(Tag),
 dd(Val).

relation_icon(subclass) => '[i]'.
relation_icon(part_of) => '[p]'.
relation_icon(R) => node_info_label(R).

nodeid(Node) =>
 if(is_anonymous(Node),
    then: if(genus(Node,_),
             then: '<composed>'),
    else: [Node]).

noderef(Node) =>
 doc:'shows hyperlinked node label',
 call(atom_concat('?node=',Node,Link)),
 if(override_noderef(Node,Method),
    then:
   noderef(Method,Node,Link),
    else:
   a(class=info,href=Link,node_info_label(Node),
     node_info_mini(Node))).

% TODO: move
% used?
node_expansion_path(Gene,LinkPath):-
        class(GeneClass,gene),
        inst_of(Gene,GeneClass),
        LinkPath=(   node_link(Genotype,'OBO_REL:variant_of',Gene),
                     node_link(Genotype,_,_)).
                  

% TODO: move
override_noderef(Node,phenotype):-
        is_anonymous(Node),
        genus(Node,G),
        (   belongs(G,quality)
        ;   belongs(G,'MPheno.ontology')).

% TODO: move
noderef(phenotype,Node,Link) =>
  doc:'override: phenotype specific',
  div(class=phenotype_node,
      span(a(href=Link,'*')),
      span('qualifier:',noderef(Q)) forall differentium(Node,'OBOL:qualifier',Q),
      span('quality:',noderef(Q)) forall genus(Node,Q),
      span(noderef(X)) forall differentium(Node,'OBO_REL:towards',X),
      span('in:',noderef(X)) forall differentium(Node,'OBO_REL:inheres_in',X)).


noderef_plus(Node) =>
 doc:'with expandability',
 call(atom_concat('?node=',Node,Link)),
 a(class=info,href=Link,node_info_label(Node),
   node_info_mini(Node)),
 node_expand_widget(node=Node).

node_expand_widget(Param) =>
 node_expand_widget(Param,'').
                    
node_expand_widget(Param,Info) =>
 doc:'ajax widget for fetching more info',
 call(gensym(anchor,AnchorId)),
 call(atom_concat(AnchorId,'-content',AnchorContentId)),
 call(sformat(Js,'insertNodeHtml(\'~w\',\'~w\')',[AnchorId,Param])),
 html:button(id=AnchorId,
             onclick=Js,
             span(data('+'))),
 Info,
 span(id=AnchorContentId,span('')).


%% AJAX %%
innerNodeHtml(Node) =>
 doc:'ajax return value for a node html chunk',
 node_info_mini(Node)
 :: pre(S,
        (   getparam(S,show,innerHtml),
            getparam(S,node,Node))).


node_info_mini(Node) =>
  doc:'short info on a node',
  span(h2(data(Node),' : ',Label where entity_label(Node,Label)),
       div(h3('[Anonymous]')) where is_anonymous(Node),
       div(h3('[Obsolete]')) where entity_obsolete(Node,_),
       div(h3('Namespace'),
           p(data(Source))) where entity_resource(Node,Source),
       div(class=def,
           h3('Definition'),
           data(Def),
           ul(li(class=def,data(Xref)) forall def_xref(Node,Xref))) forall_unique def(Node,Def),
       div(class=comments,
           h3('Comments'),
           ul(li(data(Comment)) forall_unique entity_comment(Node,Comment))),
       div(class=synonym,
           h3('Synonyms'),
           ul(class=synonym,
              li(class=synonym,
                 data(Synonym),
                 data(Xref) forall entity_synonym_xref(Node,Synonym,Xref)) forall_unique entity_synonym(Node,Synonym))) where entity_synonym(Node,_)).

node_info_label(Node) =>
 if(node_auto_label(Node,Label),
    then: data(Label),
    else: idata(Node)).

node_detail_panel(Node) =>
 doc:'show a node in a graph',
 div(class=block,id=info,
     h2('Information'),
     dl(class='term-info',
        dtdd(id,Node) where (\+ is_anonymous(Node)),
        dtdd(namespace,Source) forall_unique entity_resource(Node,Source),
        dtdd(label,Label) forall_unique entity_label(Node,Label),
        dtdd(synonym,Synonym) forall_unique entity_synonym(Node,Synonym),
        dtdd(type,noderef(InstanceOf)) forall_unique inst_of(Node,InstanceOf))),
 div(class=block,id=info,
     h2('Image'),
     call(node_image(Node,Image,Dot)),
     img(src=Image),
     html:br,
     span(a(type='application/dot',href=Dot,'.dot'))),
 div(class=block,id=tree,
     h2('Links'),
     table(
           tr(th(''),th('Relation'),th('Object'),th(''),th('Source'),th('Provenance')),
           tr(td(''),td(noderef(R)),td(noderef(To)),annotation_info_cols(Node,R,To)) forall_unique node_link(Node,R,To),
           tr(td(''),td(noderef(R)),td(noderef(To))) forall_unique inst_sv(Node,R,To,_))),
 links_panel(Node,by_creator),
 div(class=block,id=annotations,
     h2('Annotations'),
     table(
           tr(td(noderef(AnnotatedEntity)),td(noderef(NodeDirect)),annotation_info_cols(Annot)) forall_unique curation_statementT(Annot,AnnotatedEntity,R,Node,NodeDirect))),
 div(class=block,id=tree2,
     h2('Reciprocal Links'),
     table(
           tr(td(noderef(From)),td(noderef(R))) forall_unique node_link(From,R,Node))),
 div(class=block,id=summary,
     h2('Summary'),
     annotation_summary_table(Node)),
 % extensibility:
 info_component(Method,Node) forall_unique info_component_type(Method),
 ''
 .




% todo: move
info_component_type(gene).
info_component(gene,Gene) =>
 if((inst_of(Gene,GeneClass),class(GeneClass,gene)),
    then:[
          if(homol_db:homologset_member(Set,Gene,_,_),
             then:
            span(
                 h2('Gene Set summary'),
                 homologset_summary(Set)),
             else:
            span(
                 h2('Gene summary'),
                 gene_summary(Gene)))]).

xxxinfo_component(gene,Gene) =>
 div(class=block,id=tree,
     call(debug(obd,'Gene: ~w GT: ~w',[Gene,Node])),
     h2(noderef(Node),
        table(
              tr(th(''),th('Relation'),th('Object'),th(''),th('Source'),th('Provenance')),
              call(debug(obd,'nodelink: ~w',[Node])),
              tr(td(''),td(noderef(R)),td(noderef(To)),annotation_info_cols(Node,R,To)) forall_unique node_link(Node,R,To))))
 forall_unique ( class(GTClass,'genotype'),
                 node_link(Node,_,Gene),
                 call(debug(obd,'   ?GT: ~w',[Node])),
                 inst_of(Node,GTClass)).

% todo: move
entry_page(genotype_summary_page,'Genotypes').
init_page(genotype_summary_page).
genotype_summary_page =>
 outer('Genotype summary',
       genotype_summary).

show_genotype_summary_page =>
 outer('Genotype summary',
       genotype_summary)
 :: pre(S,getparam(S,show,genotypes)).

% genotypes
genotype_summary =>
 doc:'annotation summary by genotype',
 call(all_curation_creator(Sources)),
 div(class=block,id=genotype,
     table(tr(th('Genotype'),td(font(size= -2,noderef(S))) forall member(S,Sources),th(colspan=2,'Congruence')),
           genotype_summary_by_gene(G,Sources) forall_unique (class(GTC,genotype),inst_of(GT,GTC),variant_of(GT,G)))).

genotype_summary_by_gene(G,Sources) =>
 tr(th(cols=10,noderef(G),' ',noderef(Tax) forall_unique inst_rel(G,'part_of_organism',Tax))),
 genotype_summary(GT,Sources) forall variant_of(GT,G).

genotype_summary(G,Sources) =>
 tr(td(noderef(G)),
    genotype_summary_by_source(G,S) forall member(S,Sources),
    call(annotation_congruence_for_subject(G,Total,Score)),
    td(Score),td(Total)).

genotype_summary_by_source(G,S) =>
 call(setof_count(A,(curation_statement(A,G,_,_),entity_resource(A,S)),Num)),
 debug(obd,'gsbs ~w ~w = ~w',[G,S,Num]),
 if(Num=0,then:td(b(0 )),else:td(Num)).

% todo: DRY
gene_summary(G) =>
 call(all_curation_creator(AllSources)),
 if(annotation_minimal_summary_by_predicate(Su,variant_of(Su,G),ObSrcs),
    then:
   table( tr(th(colspan=2,'Class'),
             td(font(size= -2,noderef(S))) forall member(S,AllSources)),

          tr(td(nodeid(Ob)),
             td(noderef(Ob)),
             td( if(member(S,Srcs),
                    then:
                   '+',
                    else:
                   '-')) forall member(S,AllSources))
        forall member(Ob-Srcs,ObSrcs))).
 
homologset_summary(Set) =>
 call(all_curation_creator(AllSources)),
 span('Genes: ',
      noderef(G) forall homologset_member(Set,G,_,__)),
 if(annotation_minimal_summary_by_predicate(Su,(homol_db:homologset_member(Set,G,_,_),variant_of(Su,G)),ObSrcs),
    then:
   table( tr(th(colspan=2,'Class'),
             td(font(size= -2,noderef(S))) forall member(S,AllSources)),

          tr(td(nodeid(Ob)),
             td(noderef(Ob)),
             td( if(member(S,Srcs),
                    then:
                   '+',
                    else:
                   '-')) forall member(S,AllSources))
        forall member(Ob-Srcs,ObSrcs))).
 

% --end

annotation_summary_table(Su) =>
 doc:'tabular summary of informative nodes',
 call(all_curation_creator(AllSources)),
 if(annotation_minimal_summary_for_subject(Su,ObSrcs),
    then:
   table( tr(th(colspan=2,'Class'),
             td(font(size= -2,noderef(S))) forall member(S,AllSources)),

          tr(td(nodeid(Ob)),
             td(noderef(Ob)),
             td( if(member(S,Srcs),
                    then:
                   '+',
                    else:
                   '-')) forall member(S,AllSources))
        forall member(Ob-Srcs,ObSrcs))).

links_panel(Node,by_creator) =>
 div(class=block,id=tree,
     h2('Links by creator'),
     call(solutions(NS,(curation_statement(Annot,Node,_,_),entity_resource(Annot,NS)),NSs)),
     map(NS,
         [h3(data(NS)),
          table(tr(td(noderef(R)),td(noderef(To)),annotation_info_cols(Annot)) forall_unique (curation_statement(Annot,Node,R,To),entity_resource(Annot,NS)))],
         NSs)).

annotation_info_cols(Node,R,To) =>
 if(curation_statement(Annot,Node,R,To),
    then: annotation_info_cols(Annot),
    else: td('')).

annotation_info_cols(Annot) =>
 call(debug(obd,'Annot: ~w',[Annot])),
 td(noderef(Annot)),
 td(noderef(NS) where entity_resource(Annot,NS)),
 td(noderef(Pub) where inst_rel(Annot,'oboMetaModel:provenance',Pub)).

source_summary_panel =>
 doc:'all onts',
 ul(li(source_summary(Source)) forall_unique Source in belongs(_,Source)).

source_summary(Source) =>
 h3(data(Source)),
 call(setof_count(C,entity_resource(C,Source),Num)),
 p('number of entities: ',Num),
 node_expand_widget(roots_by=Source).

node_image(Node,FmtImgUrl,DotImgUrl):-
        Fmt=png,
        %get_www_document_root(DocRoot),
        ensure_loaded(bio(ontol_writer_dot)),
        ontology_segment([Node],Edges,_OutNodes,[]),
        node_safepath(Node,Local),
        concat_atom(['/amigo2/cache/',Local],ImgPathLocalBase),
        concat_atom(['/Library/WebServer/Documents',ImgPathLocalBase],ImgPathFullBase),
        base_url(BaseUrl),
        concat_atom([BaseUrl,'/',ImgPathLocalBase],ImgUrlBase),
        (   write_edges_via_dot(Fmt,Edges,ImgPathFullBase)
        ->  true
        ;   format(standard_error,'cannot write .dot')),
        concat_atom([ImgUrlBase,Fmt],'.',FmtImgUrl),
        concat_atom([ImgUrlBase,dot],'.',DotImgUrl).

% TODO
node_safepath(_Node,foo).

roots_by_response(Source) =>
 doc:'ajax return value for a node html chunk',
 div(class=treeview,
     node_in_tree(root,Node) forall noparent(Node,Source))
 :: pre(S,
        (   getparam(S,show,innerHtml),
            getparam(S,roots_by,Source))).

%% TREE BROWSER %%

node_in_tree(Rel,Node) =>
 ul(li(node_expand_widget(children_of=Node,
                          [relation_icon(Rel),
                           noderef(Node),
                           node_in_tree_extra(Node)]))).

ajax_response_to_children_of(Node) =>
 doc:'ajax return value for a node html chunk',
 node_in_tree(Rel,ChildNode) forall parent(ChildNode,Rel,Node)
 :: pre(S,
        (   getparam(S,show,innerHtml),
            getparam(S,children_of,Node))).

node_in_tree_extra(Node) =>
 span(class=node_info,
      'annotated_entities:',
      call(count_by(Type,AE,(curation_statementT(_,AE,_,Node),node_type(AE,Type)),CountTerms)), % todo: transitive
      map(Type-Count,
          [noderef(Type),' count:',Count,node_expand_widget(statements_using=Node)],
          CountTerms)).

%% PAGE %%
node_detail_page(Node) =>
 doc:'shows a single node in detail',
 call((entity_label(Node,Label)->true ; Label = '')),
 outer(['Node Info: ',Node,' ',Label],
       node_detail_panel(Node))
 :: pre(S,getparam(S,node,Node)).
 
source_summary_page =>
 doc:'shows all ontologies/sources',
 outer('Ontologies',
       source_summary_panel)
 :: pre(S,getparam(S,show,sources)).

 
match_xp_summary_page =>
 doc:'shows all xps',
 xp_summary_page('-')
 :: pre(S,getparam(S,show,xps)).
 
xp_summary_page( Ont)=>
 doc:'shows all xps',
 call(sformat(Title,'XP Defs for ~w',[Ont])),
 html(page_head_element(Title),
      body(div(id=searchbar,
               ul(id=menuToggle,
                  li(a(href='http://www.berkeleybop.org/obol','Obol')),
                  li(a(href='http://www.berkeleybop.org/ontologies#logical_definitions','XPs')))),
           div(id=xp_main,
               h1(Title),
               p(class=info,
                 'Each table row shows a term and its proposed',a(href='http://www.bioontology.org/wiki/index.php/XP:Main_Page',i('genus-differentia definition'))),
               html:br,
               call(setof(Genus,Class^genus(Class,Genus),Genuses)),
               call(findmax(Class,NumDiffs,(genus(Class,_),setof_count(D,differentium(Class,_,D),NumDiffs)),_,MaxDiffs)),
               call(solutions(X,between(1,MaxDiffs,X),Nums)),
               p('Click on column heading to sort (may take some time)'),
               table(class=sortable,id=xp_table,
%                     tr(th(''),
%                        th(''),
%                        map(Num,th(colspan=2,'Discriminating characteristic'),Nums)),
                     tr(th(class=xp_term,'Defined term'),
                        th(class=xp_genus,'Core (genus) term'),
                        map(_Num,[th(class=xp_diff_rel,'[D-Rel'),th(class=xp_diff_term,'D-Term]')],Nums)),
                     xp_summary_by_genus_rows(Genus) forall_unique member(Genus,Genuses))))).


xp_summary_by_genus_rows( Genus) =>
 doc:'shows all xps',
 %tr(th(class=genus,colspan=8,a(name=Genus,'-'))),
 call(setof(Class,genus(Class,Genus),Classes)),
 xp_summary_row(Class) forall member(Class,Classes).

%xp_summary_by_relation_rows(R) =>
% doc:'shows all xps',
% tr(th(colspan=8,span(class=table_break,noderef(R)))),
% call(setof(Class,To^differentium(Class,R,To),Classes)),
% xp_summary_row(Class) forall member(Class,Classes).

xp_summary_row(Class) =>
 doc:'shows a row in an xp summary table',
 if(genus(Class,Genus),
    then:
   [tr(td(class=xp_term,noderef(Class)),
       td(class=xp_genus,noderef(Genus)),
       [td(class=xp_diff_rel,noderef(Rel)),
        td(class=xp_diff_term,noderef(To))] forall_unique differentium(Class,Rel,To))]).

%td(xp_summary_differentia_box(Class)))]).

%    tr(td(ul(class=synonym,
%             li(class=synonym,data(Synonym)) forall entity_synonym_xref(Class,Synonym,'OBOL:automatic'))))]).

% DEP?
xp_summary_differentia_box(Class) =>
 doc:'lists differentia from a class',
 div(class=differentia,
     span(class=differentium,
          noderef(Rel),
          noderef(To)
          )
    forall_unique differentium(Class,Rel,To)).

entailment_list(Type) =>
 doc:'Shows all entailments',
 div(class=list,
     ul(li(entailment_explanation_box(X,Type)) forall entailed_by(X,Type))).

%entailment_explanation_box(Fact,RuleName) =>
% span(class=explanation).

      
