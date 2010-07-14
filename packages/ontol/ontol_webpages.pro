:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_restful)).
:- use_module(bio(metadata_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(serval)).
:- use_module(bio(quickterm)).
:- use_module(bio(bioprolog_util)).


has_info(ID) :-
        entity_label(ID,_).
has_info(IDs) :-
        member(ID,IDs),
        entity_label(ID,_).

downloadbar(ID)=>
 if(has_info(ID),
    then:
   ['Download: [', downloadfmt(ID,Fmt) forall download(Fmt), ']'],
    else: []).


downloadfmt(ID,Fmt) =>
 call(id_url(ID,Fmt,URL)),
 a(href=URL, Fmt).

downloadfmt(ID,Fmt,Txt) =>
 call(id_url(ID,Fmt,URL)),
 a(href=URL, Txt).

hide_instance(X) :- inst_sv(X,type,_,_).

entry_page =>
 outer('OBO',
       [h2('Ontologies'),
        div(class=floatL,
            table(class='tagval_table',
                  tdpair(Ont,
                         [hlink(X),' ',
                          '[',downloadfmt(X,metadata),'] ',
                          downloadfmt(X,xps) where inst_sv(Ont,type,logical_definitions,_)
                         ]) forall_unique ont_idspace(Ont,X)))]).

mappings_entry_page =>
 outer('OBO Mappings',
       [h2('Mappings'),
        div(class=floatL,
            table(class='tagval_table',
                  tdpair(Ont,
                         [data(T) where inst_sv(Ont,type,T,_),
                          hlink(X),' ',
                          '[',downloadfmt(X,metadata),'] ',
                          downloadfmt(X,xps) where inst_sv(Ont,type,logical_definitions,_)
                         ]) forall_unique ont_idspace(Ont,X)))]).

basic(ID) =>
  outer(ID,
        span(downloadbar(ID),
	     entity_info(ID))).

multiple2(IDs) =>
 outer(IDs,
       span(downloadbar(IDs),       
	    entity_info(ID) forall_unique (member(ID,IDs)))).

entity_info(ID) =>
  div(class=floatL,
      table(class='tagval_table',
	    tdpair('ID',noesc(ID)),
	    tdpair('ID Space',hlink(X)) forall parse_id_idspace(ID,X),
	    tdpair('URL',a(href=X,X)) forall id_exturl(ID,X),
	    tdpair('Name',Name) forall metadata_db:entity_label(ID,Name),
	    tdpair('Ontology',Ont) forall_unique belongs(ID,Ont),
	    tdpair('Instance of',hlink(X)) forall_unique inst_of(ID,X),                     
	    tdpair('Subset',X) forall_unique entity_partition(ID,X),
	    tdpair('Definition',[Def,' ',
				 bestlink(X) forall_unique def_xref(ID,X)]) forall_unique def(ID,Def),                     
	    tdpair('Comment',X) forall_unique entity_comment(ID,X),                     
	    tdpair('Xref',[hlink_with_id(X),
			   hlinklist([X,ID],compare) where knowsabout(X)
			  ]) forall_unique entity_xref(ID,X),                     
	    tdpair('', hlinklist([ID|Xs],'compare all')) where setof(X,(entity_xref(ID,X),knowsabout(X)),Xs),
	    tdpair(hlink(R),X) forall_unique inst_sv(ID,R,X,_),                     
	    tdpair(hlink(R),hlink(X)) forall_unique inst_rel(ID,R,X),                     
	    tdpair([i(b(X)),' Synonym'],
		   [Synonym,
		    i(' type:',b(T)) forall_unique entity_synonym_type(ID,T,Synonym)
		   ]) forall_unique synonym(ID,X,Synonym),
	    tdpair('Disjoint from',hlink(X)) forall_unique disjoint_from(ID,X),                     
	    tdpair('Domain',hlink(X)) forall_unique property_domain(ID,X),                     
	    tdpair('Range',hlink(X)) forall_unique property_range(ID,X),                     
	    tdpair('Property',X) forall_unique metaproperty(ID,X),                     
	    tdpair('Genus',hlink(X)) forall_unique genus(ID,X),                     
	    tdpair('Differentia',rel(R,X)) forall_unique differentium(ID,R,X),                     
	    tdpair('is_a',hlink(X)) forall_unique subclass(ID,X),                     
	    tdpair(hlink(R),hlink(X)) forall_unique restriction(ID,R,X),                     
	    ''),
      class_children(ID),
      meta_search_button(ID),
      div(id=what_links_here,
	  call(id_url(ID,revlinks,RevURL)),
	  call(sformat(JS,'JavaScript:replaceContents(document.getElementById(\'what_links_here\'),\'~w\');',[RevURL])),
	  html:input(type=button,
		     onClick=JS,
		     value='What links here?'))
     ),
  graphimg(ID),
  images_box(ID),
  wikipedia_info(ID,X) forall_unique id_wikipage(ID,X).

meta_search_button(ID) =>
 div(id=meta_search,
     call(id_url(ID,meta_search,SearchURL)),
     call(sformat(SearchJS,'JavaScript:replaceContents(document.getElementById(\'meta_search\'),\'~w\');',[SearchURL])),
     html:input(type=button,
                onClick=SearchJS,
                value='Meta-Search')).

multirow(Col,Val,Goal,Var,List) =>
 tr(th(Col),
    td([Val] forall_unique [html:p]/Goal) forall member(Var,List)).

% show multiple IDs together
multiple(IDs) =>
 outer(IDs,
       span(downloadbar(IDs),       
	    multiple_entity_info(IDs))).

% shows each ID in its own column
multiple_entity_info(IDs) =>
  div(class=floatL,
      table(class='comparison_table',
	    multirow('ID',data(ID),true,ID,IDs),
	    multirow('Link',hlink(ID),true,ID,IDs),
	    multirow('ID Space',hlink(X),parse_id_idspace(ID,X),ID,IDs),
	    multirow('Name',Name,metadata_db:entity_label(ID,Name),ID,IDs),
	    multirow('Ontology',Ont,belongs(ID,Ont),ID,IDs),
	    multirow('Subset',X,entity_partition(ID,X),ID,IDs),
	    multirow('Definition',[Def,' ',
				   hlink(X) forall_unique def_xref(ID,X)],def(ID,Def),ID,IDs),
	    multirow('Comment',X,entity_comment(ID,X),ID,IDs),                     
	    multirow('Xref',hlink_with_id(X),entity_xref(ID,X),ID,IDs),
	    %multirow(hlink(R),X,inst_sv(ID,R,X,_),ID,IDs),                     
	    %multirow(hlink(R),X,inst_rel(ID,R,X),ID,IDs),
	    call(solutions(X,(member(ID,IDs),synonym(ID,X,_)),Xs)),
	    multirow([i(X),' synonym'],
		     [Synonym,
		      i(' type:',b(T)) forall_unique entity_synonym_type(ID,T,Synonym),
		      ' '
		     ],synonym(ID,X,Synonym),ID,IDs) forall_unique member(X,Xs),
	    multirow('Disjoint from',hlink(X),disjoint_from(ID,X),ID,IDs),
	    multirow('Domain',hlink(X),property_domain(ID,X),ID,IDs),                     
	    multirow('Range',hlink(X),property_range(ID,X),ID,IDs),                     
	    multirow('Property',X,metaproperty(ID,X),ID,IDs),                     
	    multirow('Genus',hlink(X),genus(ID,X),ID,IDs),                     
	    multirow('Differentia',rel(R,X),differentium(ID,R,X),ID,IDs),                     
	    multirow('is_a',hlink(X),subclass(ID,X),ID,IDs),
	    call(solutions(R,(member(ID,IDs),restriction(ID,R,_)),Rs)),
	    multirow(hlink(R),[hlink(X)],restriction(ID,R,X),ID,IDs) forall_unique (member(R,Rs)),
	    html:br),
      call(concat_atom(IDs,'+',IDListAtom)),
      graphimg(IDListAtom,img)).




rel(R,X) =>
  hlink(R),' ',hlink(X).


pageify(Template,Goal,MaxItems) =>
  call((solutions(Template,Goal,Items),
        length(Items,NumItems))),
  if(NumItems =< MaxItems,
     then: Items,
     else: [
            getparam_as_num(page,Page,1),
            pagebar(Page,NumItems,MaxItems),
            call((Start is (Page-1)*MaxItems,
                  End is Page*MaxItems -1)),
            Item forall ((between(Start,End,Index),
                          nth0(Index,Items,Item)))
           ]).

 

xps(ID) =>
  outer(['xps in ',ID],
        [h1('Cross product set ',hlink(ID)),
         table(tr(th('Term'),
                  th('Genus'),
                  th(colspan=2,'Differentia')),
               gdrow(X) forall_unique genus(X,_))]).

gdrow(ID) =>
  call((solutions(R-X,differentium(ID,R,X),Diffs),
        length(Diffs,NumDiffs),
        _NumRows is NumDiffs-1,
        Diffs=[R1-X1|DiffsRest])),
  tr(
     td(rowspan=NumDiffs,hlink(ID)),
     td(rowspan=NumDiffs,hlink(X) forall_unique genus(ID,X)),
     td(hlink(R1)),
     td(hlink(X1))),
  tr(td(''),td(''),td(hlink(R)),td(hlink(X))) forall_unique member(R-X,DiffsRest).


basic_search_form =>
  getparam(search_term,Val,''),
  form(input(type=textfield,
             name=search_term,
             value=Val),
       input(name=submit,type=submit,value=search)).

ontology(Ont) =>
  outer(Ont,
        [h1(hlink(Ont)),
         basic_search_form,
         h3('Fetch: ',downloadfmt(Ont,metadata)),
         table(basicrow(ID) forall class(ID),
               basicrow(ID) forall property(ID),
               basicrow(ID) forall (inst(ID),entity_label(ID,_))
              )
        ]).
%        [ul(li(hlink(X)) forall class(X))]).

ontology_filtered(Ont,S,L) =>
  outer(Ont,
        [h1(hlink(Ont),' filter:',S),
         basic_search_form,
         h3('Fetch: ',downloadfmt(Ont,metadata)),
         table(basicrow(X) forall member(X,L))]).

ontology_query(Ont,Query,Results) =>
  outer([Ont,' ',Query],
        [h1(hlink(Ont),' query'),
         div(class=queryForm,
             form(p('Query:'),textarea(name=query,rows=10,cols=64,[Query]),
                  p('Select:'),input(type=textfield,name=select,cols=32,[]),
                  input(name=submit,type=submit,value=query))),
         div(class=queryOutput,
             h3('Query results:'),
             p(Query),
             table(query_result_row(X) forall member(X,Results)))]).

query_result_row(Result) =>
  if(Result=..[Pred|Args],
     then: tr(td(data(Pred)),
              td(query_result_colval(X)) forall member(X,Args)),
     else: if(atom(Result),
              then: td(td(query_result_colval(Result))),
              else: td(td(data(Result))))).

query_result_colval(X) =>
 if((atom(X),concat_atom([_,_],':',X)),
    then: hlink(X),
    else: data(X)).

noresults(Ont,S) =>
  outer(Ont,
        [h1(hlink(Ont),' filter:',S),
         basic_search_form,
         h2('Your search produced no results')]).

basicrow(ID) =>
  tr(td(if(id_idspace(ID,S),
           then: S,
           else: '-')),
     td(hlink(ID)),
     td(data(X) forall_unique def(ID,X))).

% ----------------------------------------
% QUICKTERM
% ----------------------------------------

quickterm_outer(N,P) =>
 html:html(
           head(title(N),
                html:meta('http-equiv'='content-type', content='text/html; charset=utf-8',
                          html:meta(name=html_url, 
                                    link(href='/amigo2/css/formatting.css', rel=stylesheet, type='text/css'),
                                    %link(href='http://amigo.berkeleybop.org/amigo/js/org/bbop/amigo/ui/css/autocomplete.css', rel=stylesheet, type='text/css'),
				    script(type='text/javascript', src=X) forall_unique javascript(X),
				    html:style(type='text/css',CSS) forall_unique css(CSS)
                                    %script(type='text/javascript',
                                    %       'jQuery(document).ready(function(){ new org.bbop.amigo.ui.autocomplete({id:"target", narrow:"true", search_type:"term", ontology: "biological_process", completion_type:"completion"}); })')
                                    ))),
           
           html:body(div(id=header, a(class='logo floatR', href='search.cgi',
                                      img(src='http://amigo.geneontology.org/amigo/images/logo-sm.png', alt='AmiGO logo', title='AmiGO front page')),
                         h1(id=top, a(href='http://www.geneontology.org/', title='GO website', 'the Gene Ontology'))),
                     
                     div(class=contents,
                         P)
                    )).


quickterm('',S) =>
  call(ensure_loaded(bio(quickterm))),
  quickterm_outer(['QuickTerm Request: ',S],
        div(h2(hlink(S)),
            h3('QuickTerm Request: ',S),
            div(class=chooser,
                p('Select a template'),
                form(id=template_selection,
                     select(name=template,
                             option(value=T,T) forall (valid_qtt(T,S))
                            ),
                     input(name=submit,type=submit,value=proceed))))).


quickterm(T,S) =>
  call(ensure_loaded(bio(quickterm))),
  quickterm_outer(['QuickTerm Request: ',S,' template: ',T],
        div(h2(hlink([quickterm,S])),
            h3('QuickTerm Request in ',S),
            div(class=chooser,
                h4('Template: ',T),
                p(Desc) where qtt_description(T,Desc),
                quickterm_form(T),
                html:br))).

quickterm_form_input(P,Size,Desc) =>
 getparam(P,V,''),
 tr(th(if(is_list(P),
          then: join(' or ',P),
          else: P)),
    td(input(type=text,
             size=Size,
             name=P,
             style='outline: #3875D7 solid 1px;',
             value=V)),
    td(i(Desc))).

quickterm_form(T) =>
 form(input(type=hidden,
            name=template,
            value=T),
      input(type=hidden,
            name=request,
            value=true),
      
      if(qtt_wraps(T,_),
         [lgetparam(subtemplate,STs),
          if(member(W,STs),
             then:
            input(type=checkbox,
                  checked=yes,
                  name=subtemplate,
                  value=W),
             else:
            input(type=checkbox,
                  name=subtemplate,
                  value=W)),
          W,
          html:br] forall (qtt_wraps(T,W))),
      div(A,':',
          getparam(A,Val,''),
          input(class=term,
                type=text,
                name=A,
                value=Val,
                id=A,
                size=30,
                style='outline: #3875D7 solid 1px;'),
          '(',Dom,')') forall (qtt_arg_type(T,A,Dom)),

      if(\+qtt_wraps(T,_),
           then:
         [
          h4('Optional arguments'),
          table(quickterm_form_input(name,20,'Name (optional - a default name with standard syntax will be chosen if you leave this blank)'),
                quickterm_form_input(def,50,'Definition (optional - a standard textual def with genus-differentia syntax is chosen by default)'),
                quickterm_form_input(def_xref,10,'Definition Xref (optional)'),
                quickterm_form_input(comment,50,'Comment (optional)'))
         ],
          else: []),

      html:br,
      input(type=checkbox,
            name=commit,
            value=true),
      'Commit',
      input(name=submit,type=submit,value=submit),

      html:br,
      getparam(username,User,''),
      'Username: ',
      input(type=text,
            size=20,
            name=username,
            style='outline: #3875D7 solid 1px;',
            value=User),
      i(' (must be filled in if submit is selected)'),

      html:br).


quickterm_results(T,S,Msgs) =>
  quickterm_outer(['QuickTerm Request: ',S,' ',T],
        div(h2(hlink([quickterm,S])),
            quickterm_result_msgs(Msgs),
            quickterm_form(T))).

quickterm_errors(T,S,Errs) =>
  quickterm_outer(['QuickTerm Request: ',S,' ',T,' FAIL'],
        div(h2('FAILED'),
            h3('There was a problem with this request'),
            ul(li(quickterm_error(X)) forall member(X,Errs)),
            p('Please correct this and try again'),
            quickterm_form(T))).

quickterm_error(no_match(X)) => 'No ontology class found with name or exact synonym: ',i(X).
quickterm_error(not_in_domain(X,D)) => 'The specified value ',b(X),' ',i(hlink(X)),' is not in the domain: ',i(D).
quickterm_error(constraint_violation(D,G)) => 'Constraint rule violated: ',i(D),' Debug info: ',pre(data(G)).
quickterm_error(X) => 'Error: ',data(X).


quickterm_result_msgs(Msgs) =>
 if(is_list(Msgs),
    then: [h2('This is a multi-part request. Below are individual reports:'),
           quickterm_result_msgs(Msg) forall member(Msg,Msgs)],
    else: [quickterm_result_msg(Msgs)]).




quickterm_result_msg(error(E)) =>
  call( E=.. [Type|Args]),
  h3('Error: ',Type),
  ul(li(A,[' ',AN where entity_label(A,AN)]) forall member(A,Args)),
  p('Try again with different values').



quickterm_result_msg(ok(ID,Status,Msg)) =>
  if(Status=committed,
     then: [
            h3('Request Granted, ID=',ID),  
            p('This ID has been committed to the submission ontology. The ID is stable and can be used in annotation. ',
              'You can use the form below to submit other similar terms'),
            h4('What happens next?'),
            p('Within 1 hour your request will be visible in CVS. It will not be added to the main ontology until seen
             by a curator.')
           ],
     else: [
            h3('Request valid but not committed'),
            
            p('This request is valid. You can go ahead and add commit this by selecting the commit box below.',
              'This will submit a term with the ID ',b(ID),
              if(entity_label(ID,Name),
                 then: [' and name ',i(Name)],
                 else: []))
           ]),
  h4('Raw OBO Format:'),
  pre(noesc(Msg)).

              

% ----------------------------------------
% tree browing
% ----------------------------------------

ontology_browsable_tree(S) =>
  outer(['Browse: ',S],
        div(h2(hlink(S)),
            %relation_toggler,
            table(id=browsetbl,
                  border=1,
                  tbody(id=browsetbl_tbody,
                        browser_node(1,ID,open) forall (class(ID),id_idspace(ID,S),
                                                         \+((subclass(ID,P),id_idspace(P,S)))))
                 ))).

% table row
browser_node(Depth,ID,Open) =>
  call(sformat(OpenEltID,'open-~w',[ID])),
  tr(id=OpenEltID,
     browser_node_cols(Depth,ID,Open)),
  call(DepthPlus1 is Depth+1),
  if(Open=open,
     then: [browser_node(DepthPlus1,Y,close) forall subclass(Y,ID)],
     else: []
    ).

browser_node_cols(Depth,ID,Open) =>
  call(sformat(OpenEltID,'open-~w',[ID])),
  call(id_url(open_node/ID/Depth,OpenURL)),
  call(sformat(OpenJS,'JavaScript:clickTreeBrowserNode(\'browsetbl_tbody\',\'~w\',\'~w\');',[OpenEltID,OpenURL])),
  call(sformat(CloseEltID,'~w-close',[OpenEltID])),
  call((   Open=open
       ->  Img='/amigo2/images/minus.gif',
           OnClick=''
       ;   Img='/amigo2/images/plus.png',
           OnClick=OpenJS)),
  call(Dist is 22-Depth),
  td(' ') forall between(1,Depth,_),
  td(if(subclass(_,ID),
        then: [html:input(id=CloseEltID,
                          class=openme,
                          type=image,
                          onclick=OnClick,
                          src=Img,
                          alt=open)
              ],
        else: [img(src='/amigo2/images/dot.png')]
       )),
  td(colspan=Dist,
     span(hlink(ID))),
  browser_node_info(ID).

% TODO - make more elegant in serval
browser_subnodes_json(Depth,ID) =>
 call(DepthPlus1 is Depth+1),
 '[',
 [call(sformat(OpenEltID,'open-~w',[Y])),
  '{"id":',json_atom(OpenEltID),', ',
  '"html":',json_atom(browser_node_cols(DepthPlus1,Y,close)),
  '}, '] forall subclass(Y,ID),
 ']'.


browser_node_info(ID) =>
 td(''),
 td(span(class=textdef, data(X)) where def(ID,X)).

relation_toggler =>
 call(solutions(R,restriction(_,R,_),Rs)),
 %in(Params,call(params_drels_crels(Params,DRels,_CRels))),
 a(id=relation_form_toggler,
   href='#',
   onClick='toggleTable(\'relation_form\',\'Show relation controller\',\'Hide\');return false;',
   'Show relation controller'),
 form(id=relation_form,
      style='display:none',
      ul(li(checkbox(rel,R),
            hlink(R)) forall_unique member(R,Rs))).


% ----------------------------------------
% tree browing OLD
% ----------------------------------------
/*
ontology_browsable_tree(S) =>
  outer(['Browse: ',S],
        div(h2(hlink(S)),
            div(class=treeview,
                ul(browser_node(ID) forall (class(ID),id_idspace(ID,S),
                                            \+((subclass(ID,P),id_idspace(P,S)))))))).


browser_node(ID) =>
  call(sformat(OpenEltID,'open-~w',[ID])),
  call(id_url(open_node/ID,OpenURL)),
  call(sformat(JS,'JavaScript:replaceContents(document.getElementById(\'~w\'),\'~w\');',[OpenEltID,OpenURL])),
  li(id=OpenEltID,
     span(class=treetbl_left,
          if(subclass(_,ID),
             then: [html:input(type=image,
                               onClick=JS,
                               src='http://amigo.berkeleybop.org/amigo/images/plus.png',
                               alt=open)
                   ],
             else: [img(src='http://amigo.berkeleybop.org/amigo/images/dot.png')]
            ),
          hlink(ID)),
     browser_node_info(ID)).

% in-line replacement of a node
browser_open_node(ID) =>
  call(sformat(CloseEltID,'open-~w',[ID])),
  call(id_url(open_node/ID,CloseURL)),
  % TODO: close
  call(sformat(_JS,'JavaScript:replaceContents(document.getElementById(\'~w\'),\'~w\');',[CloseEltID,CloseURL])),
  li(span(id=CloseEltID,
          span(class=treetbl_left,
               img(src='http://amigo.berkeleybop.org/amigo/images/minus.gif'),
               hlink(ID)),
          browser_node_info(ID),
          ul(browser_node(Y) forall (subclass(Y,ID))))).

browser_node_info(ID) =>
  span(class=treetbl_right,
       data(X) where def(ID,X)).
*/

% ----------------------------------------
% entry point for an ontology
% ----------------------------------------
ontology_entry(S) =>
  outer(['Ontology ',S],
        div(h2(hlink(S)),
            basic_search_form,
            table(tr(td('Browse this ontology'),td(hlink([tree,S]))),
                  tr(td('View all classes'),td(hlink([ontology,S]))),
                  tr(td('Get metadata'),td(hlink([metadata,S])))))).

ontology_table(S) =>
  outer(['IDSpace ',S],
        [h2(hlink(S)),
         table(tr(td(hlink(ID)),
                  td(data(X) forall_unique entity_synonym(ID,X)),
                  td(data(X) forall_unique def(ID,X)))
              forall class(ID))]).

ontology_statements(Ont) =>
  outer(Ont,
        [h1(hlink(Ont)),
         h3('Fetch: ',downloadfmt(Ont,metadata)),
         table(tr(td(hlink(X)),
                  th(is_a),
                  td(hlink(Y))) forall_unique subclass(X,Y),
               tr(td(hlink(X)),
                  th(hlink(R)),
                  td(hlink(Y))) forall_unique restriction(X,R,Y),
               tr(td(hlink(X)),
                  th(xref),
                  td(hlink(Y))) forall_unique entity_xref(X,Y))
        ]).

ontology_relationships(Ont,R) =>
  outer(Ont,
        [h1(hlink(Ont)),
         h3('Fetch: ',downloadfmt(Ont,metadata)),
         table(tr(td(hlink(X)),
                  th(hlink(R)),
                  td(hlink(Y))) forall_unique parent(X,R,Y))
        ]).


ontology_metadata(S) =>
  outer(['Metadata for: ',S],
        [
         h2(hlink(S)),
         [h3(Ont),
          table(class='tagval_table',
                tdpair(R,X) forall_unique inst_sv(Ont,R,X))
         ] forall ont_idspace(Ont,S),
         h3('Derived Metadata:'),
         table(class='tagval_table',
               tdpair('Terms', N) where setof_count(X,class(X),N),
               tdpair('Obsolete Terms', N) where setof_count(X,obsolete_class(X,_),N),
               tdpair('Terms with definitions', N) where setof_count(X,(class(X),def(X,_)),N),
               tdpair('Terms with logical definitions', N) where setof_count(X,(class(X),genus(X,_)),N),
               tdpair('Relationships', N) where setof_count(X-R-Y,
                                                             parent(X,R,Y),
                                                             N),
               ntdpair('Differentia', N) where setof_count(X-R-Y,
                                                           differentium(X,R,Y),
                                                           N),
%               if( (solutions(XO,(parent(X,Y),belongs(X,XO),belongs(Y,YO),XO\=YO),XOs),XOs=[_,_|_]),
%                   then: tdpair(rels,table(
%                                           tr( t
               %ntdpair([i(R),' differentia'], N) forall_unique (property(R),setof_count(X-Y,differentium(X,R,Y),N)),
               ntdpair([pagelink([relationships,S,R],R),' relationships'], N) forall_unique ((R=subclass;property(R)),setof_count(X-Y,parent(X,R,Y),N)))
        ]).

meta_search_urls_table(Pairs) =>
  div(id=meta_search_results,
      p('Search various websites using ontology-expanded URL'),
      table(class='tagval_table',
        tr(th('Site/Engine'),th('URL')),
        tr(td(Engine),td(a(href=URL,data(URL)))) forall member(Engine-URL,Pairs))).

what_links_here_table(ID) =>
  table(class='tagval_table',

        th(hlink(ID)),html:td,html:td,td('Source'),
        fwdlink(ID,'Xref',hlink(X),entity_xref(ID,X)),                     
        fwdlink(ID,hlink(R),X, inst_rel(ID,R,X)),                     
        fwdlink(ID,'Domain',hlink(X), property_domain(ID,X)),                     
        fwdlink(ID,'Range',hlink(X), property_range(ID,X)),                     
        fwdlink(ID,'Genus',hlink(X), genus(ID,X)),                     
        fwdlink(ID,'Differentium',rel(R,X), differentium(ID,R,X)),                     
        fwdlink(ID,'is_a',hlink(X), subclass(ID,X)),                     
        fwdlink(ID,hlink(R),hlink(X), restriction(ID,R,X)),

        html:td,html:td,th(hlink(ID)),td('Source'),
        revlink('Xref',hlink(X),entity_xref(X,ID)),                     
        revlink(hlink(R),X, inst_rel(X,R,ID)),                     
        revlink('Domain',hlink(X), property_domain(X,ID)),                     
        revlink('Range',hlink(X), property_range(X,ID)),                     
        revlink('Genus',hlink(X), genus(X,ID)),                     
        revlink('Differentium',rel(R,X), differentium(X,R,ID)),                     
        revlink('is_a',hlink(X), subclass(X,ID)),                     
        revlink(hlink(R),hlink(X), restriction(X,R,ID)),
        

        %invtdpair('Xref',hlink(X)) forall_unique entity_xref(X,ID),                     
        %invtdpair(hlink(R),X) forall_unique inst_rel(X,R,ID),                     
        %invtdpair('Domain',hlink(X)) forall_unique property_domain(X,ID),                     
        %invtdpair('Range',hlink(X)) forall_unique property_range(X,ID),                     
        %invtdpair('Genus',hlink(X)) forall_unique genus(X,ID),                     
        %invtdpair('Differentium',rel(R,X)) forall_unique differentium(X,R,ID),                     
        %invtdpair('is_a',hlink(X)) forall_unique subclass(X,ID),                     
        %invtdpair(hlink(R),hlink(X)) forall_unique restriction(X,R,ID),
        '').

revlink(Prop,Val,Goal) =>
  tr(
     td(Val),
     th(Prop),
     th('"'),
     td(hlink(Source)) forall_unique fact_clausesource(Goal,Source))
%        ' ',
%        hlink(Val,Source)) forall_unique fact_clausesource(Goal,Source))
  forall_unique Goal.

fwdlink(ID,Prop,Val,Goal) =>
  tr(
     th('"'),
     th(Prop),
     td(Val),
     td(hlink(Source),
        ' ',
        hlink(ID,Source)
        ) forall_unique fact_clausesource(Goal,Source))
  forall_unique Goal.

wikipedia_info(_ID,Page) =>
 call(ensure_loaded(bio(web_fetch_wikipedia))),
 call(sformat(EditURL,'http://en.wikipedia.org/w/index.php?title=~w&action=edit',[Page])),
 div(id=wikipedia,
     h3('Wikipedia'),a(href=EditURL,'Edit wikipedia entry'),html:br,
     div(id=wpData,class=controlTabContent,
	 noesc(Body) forall (        %format(user_error,'Fetching ~w~n',[Page]),
				     web_search_wikipedia(Page,Results,[]),
				     member(Body,Results))),
     noesc('<!-- The code to extract wikipedia entries was kindly provided by the Rfam group -->')).

images_box(ID) =>
 div(id=images,
     img(src=URL,'') forall id_imgurl(ID,URL)).

graphimg(ID) =>
 graphimg(ID,floatR).

class_imgurl(ID,Hidden,ImgURL) :-
        sformat(ImgURL,'/obo/~w.png?~w',[ID,Hidden]).

embedded_graph_img(ID,Hidden) =>
 call(class_imgurl(ID,Hidden,ImgURL)),
 img(id=main_img,
     src=ImgURL).
       
graphimg(ID,CssClass) =>
 in(Params,call(params_hidden(Params,Hidden))),
 %call(sformat(ImgUrlAll,'/obo/~w.png?rel=all',[ID])),
 call(solutions(R,restriction(_,R,_),Rs)),
 in(Params,call(params_drels_crels(Params,DRels,CRels))),
 span(class=CssClass,
      embedded_graph_img(ID,Hidden),
      html:br,
      a(id=imgform_toggler,
        href='#',
        onClick='toggleTable(\'imgform\',\'Show graph config panel\',\'Hide\');return false;',
        'Show graph config panel'),
      form(id=imgform,
           style='display:none',
           table(class=small,
                 tr(th('show?'),th(relation),th('contain?')),
                 tr(td(checkbox(rel,R,(member(R,DRels);DRels=[];DRels=[all]))),
                    td(hlink(R)),
                    td(checkbox(cr,R,member(R,CRels)))) forall_unique member(R,Rs),
                 tr(td(''),
                    td(i('is_a')),
                    td(checkbox(cr,subclass,member(subclass,CRels))))),
           call(sformat(JS,'JavaScript:fetch_graph_image(\'~w\',document.forms.imgform);',ImgUrl)),
           html:input(type=button,
                      onClick=JS,
                      value='Redraw'),
           call(sformat(JSAll,'JavaScript:fetch_graph_image_all_relations(\'~w\');',ImgUrl)),
           html:input(type=button,
                      onClick=JSAll,
                      value='Show All'))).

class_parents(ID) =>
  ul(li(hlink(R),' ',hlink(X)) forall_unique parent(ID,R,X)).
class_children(ID) =>
  ul(li(hlink(R),'[rev] ',hlink(X)) forall_unique parent(X,R,ID)).

pagelink(L,N) =>
 in(Params,
    [call((concat_atom(L,'/',X),
          id_params_url(X,Params,URL))),
     a(href=URL,N)]).

hlink([X|L]) =>
 call(concat_atom([X|L],'/',A)),
 hlink(A).
  
hlink(X) =>
 if(parse_id_idspace(X,'Image',Local),
    then: a(href=Local,img(height=80,src=Local)),
    else: in(Params,
             [call(id_params_url(X,Params,URL)),
              a(href=URL,if(entity_label(X,Label),then:Label,else:X))])).

hlink_with_id(X) =>
 if(parse_id_idspace(X,'Image',Local),
    then:a(href=Local,img(height=80,src=Local)),
    else: in(Params,
             [call(id_params_url(X,Params,URL)),
              a(href=URL,if(entity_label(X,Label),then:[X,' ',Label],else:X))
             ])).

hlink(X,Context) =>
 in(Params,
    [call(id_params_url(X,Params,URL,Context)),
     i(a(href=URL,'view in context'))
    ]).

%hlink(X) =>
% call(id_url(X,URL)),
% a(href=URL,if(entity_label(X,Label),then:Label,else:X)).

bestlink(ID) =>
 if((id_idspace(ID,S),ont_idspace(_,S)),
    then: hlink(ID),
    else: extlink(ID)).

extlink(ID) =>
 if(id_exturl(ID,U),
    then: a(href=U,U),
    else: [ID]).


hlinklist(Xs,Title) =>
 call(concat_atom(Xs,'+',X)),
 call(id_url(X,URL)),
 a(href=URL,Title).

help_page =>
 outer('OBO Browser Documentation',
       div(div(h2('OBO Browser'),
               p('This is an experimental browser for OBO ontologies')),
           div(h3('Page URLs'),
               p('Each ontology class has its own page, with a URL of the form',
                 html:code('http://berkeleybop.org/obo/',b('[OBO-ID]')),
                 'For example:',
                 ul(li(hlink('FBbt:00005106'),' -- neuron, in fly anatomy'),
                    li(hlink('FMA:7088'),' -- heart, in FMA'))),
               p('You can also use the primary label as the ID, for example: ',
                 hlink('CL:neuron'),' -- neuron, in CL')),
           div(h3('Graph Views'),

               p('A graph view is shown for each class. The default
               behavior is to show the closure of the subclass
               relation. For some ontologies the defauly behavior is
               different. For example, many anatomy ontologies will by
               default show the partonomy and the ontogeny (develops
               from relations). Ontologies that follow strict single
               isa inheritance (such as the FMA), the default behavior
               may be to show subclass relationships via
               ',i(containment),' -- this can be seen for example with
               the FMA class "Heart" (shown)', embedded_graph_img('FMA:7088',_)),

              p('In all cases the default behavior can be overridden
              by clicking on the ',i('graph config panel'),' which
              allows the display of additional relations, and the
              selection of a containment relation. Note that behavior
              is undefined when a multiple parentage relation is
              selected for containment')),

          div(h3('Wikipedia Integration'),

              p('If an ontology has wikipedia cross-references
              (e.g. GO, UBERON), then the wikipedia page will be
              embedded in the OBO page. See for example ',hlink('UBERON:0001474'))),

          div(h3('Alternative downloads'),

              p('A number of download options are available, including
              obo, owl and owl2 (the latter uses the new
              purl.obolibrary.org URI scheme, and IAO for ontology
              metadata). The default approach is to perform the
              subclass closure, but this can be overridden by
              appending "?rel=<REL>" to the URL, or "?rel=all" for all
              relations. In future there will be a more intuitive
              interface for this. Some examples follow:'),

             ul(li(hlink('CL:0000540.owl2?rel=all'),' -- neuron in OWL, with all relations followed'),
                li(hlink('CL:0000540.obo?rel=all'),' -- the same in obo'))),

          div(h3('Multiple class view'),

              p('It is possible to view multiple classes together
               using URLs such as ',hlink('CL:0000084+CL:0000236')),

             p('This is also possible for classes in different
              ontologies. For example:
              ',hlink('UBERON:0000019+ZFA:0000107+MA:0000261')),

             p('URLs such as this are constructed automatically as
              links from xrefs. See for example any Uberon page (e.g. ',hlink('UBERON:eye'),').'),
          
             p('Eye in Uberon, ZFA and MA:',embedded_graph_img('ZFA:0000107+MA:0000261+UBERON:0000019.png?rel=part_of',_))),
           
           div(h3('Meta-search'),

               p('Allows search of google, pubmed and in future other
               resources using ontology-based term
               expansion. Currently only subclass relationships are
               used')),

           div(h3('What links here?'),

               p('One of the goals of the OBO Foundry is to have all
               ontologies integrated, interoperable and
               interconnected. Clicking this button will show classes
               in external ontologies that reference this one. Logical
               definition bridge files are also searched here.'),

              p('See for example ',hlink('MP:0000100?import=MP_XP'),'
              -- abnormal ethmoidal bone morphology, with logical definitions from PATO and MA loaded')),
          
          div(h3('Ontology metadata'),
              p('See for example: ',hlink('metadata/CARO'))),

          div(h3('Ontology search'),

              p('See for example: ',hlink('CARO'),' and type in a
              search term to the text box. Search is currently quite
              primitive. You can also use the OBO firefox plugin or
              search via URLs such as ',
              a(href='http://amigo.berkeleybop.org/cgi-bin/obo/stoc?query=ethmoid','this
              one'))),
          
          div(h3('Advanced queries'),

              p('Advanced prolog queries can be executed from the query page for each ontology. E.g. ',
              hlink('query/FBbt'),' -- see blipkit ontol_db module for details.'),
              p('In future DL queries and Thea POPL transformations will be possible')),

           html:hr)).


javascript('http://yui.yahooapis.com/2.3.1/build/yahoo-dom-event/yahoo-dom-event.js').
javascript('http://amigo.geneontology.org/amigo/js/all.js').
javascript('/amigo2/js/obo.js').
javascript('/amigo2/js/dojo.js').
% for autocomplete
javascript('http://amigo.berkeleybop.org/amigo/js/com/jquery-1.4.2.min.js').
javascript('http://amigo.berkeleybop.org/amigo/js/org/bbop/amigo.js').
javascript('http://amigo.berkeleybop.org/amigo/js/org/bbop/amigo/go_meta.js').
javascript('http://amigo.berkeleybop.org/amigo/js/org/bbop/amigo/opensearch.js').
javascript('http://amigo.berkeleybop.org/amigo/js/org/bbop/amigo/ui/autocomplete.js').


css('#front-nav ul { margin: 0 }
   #front-nav li { background: #e9effa; color: #3875D7; margin: 1em 200px; border: 1px dotted #006; text-align: center; }
   #front-nav .h1 { font: 3em/1.0 "trebuchet ms", "lucida grande", arial, sans-serif; padding: 1em 0; }
   #front-nav a { border: none; display: block; padding: 1em; }
   #front-nav fieldset { color: #000; }
   #front-nav legend { display: inline; }').

%pagelink(search,'Search','Advanced search').
pagelink('/obo/','Ontologies','All ontologies').
pagelink('/obo/help','Help','Documentation').

download(obo).
download(obox).
download(owl).
download(owl2).
download(chado).
download(pro).
download(json).
download(png).

outer(N,P) =>
 doc:'wraps HTML in main template; this is the main look and feel template',
 html:html(
           head(title(N),
                html:meta('http-equiv'='content-type', content='text/html; charset=utf-8',
                          html:meta(name=html_url, content='http://amigo.geneontology.org/amigo',
                                    %link(href='http://amigo.geneontology.org/amigo/css/formatting.css', rel=stylesheet, type='text/css'),
                                    link(href='/amigo2/css/formatting.css', rel=stylesheet, type='text/css'),
                                    link(href='http://amigo.berkeleybop.org/amigo/js/org/bbop/amigo/ui/css/autocomplete.css', rel=stylesheet, type='text/css'),
                                    link(href='http://rfam.sanger.ac.uk/static/css/wp.css', rel=stylesheet, type='text/css'),
				    script(type='text/javascript', src=X) forall_unique javascript(X),
				    html:style(type='text/css',CSS) forall_unique css(CSS),
                                    script(type='text/javascript',
                                           'var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
                                          document.write(unescape("%3Cscript src=\'" + gaJsHost + "google-analytics.com/ga.js\' type=\'text/javascript\'%3E%3C/script%3E"));'),
%                                    script(type='text/javascript',
%                                           'jQuery(document).ready(function(){ new org.bbop.amigo.ui.autocomplete({id:"target", narrow:"true", search_type:"term", ontology: "biological_process", completion_type:"completion"}); })'),
                                    script(type='text/javascript',
                                          'try {
                                           var pageTracker = _gat._getTracker("UA-11782828-2");
                                           pageTracker._trackPageview();
                                          } catch(err) {}')
				   ))),
           
           html:body(div(id=header, a(class='logo floatR', href='search.cgi',
                                      img(src='http://amigo.geneontology.org/amigo/images/logo-sm.png', alt='AmiGO logo', title='AmiGO front page')),
                         h1(id=top, a(href='http://www.obofoundry.org/', title='OBO Foundry website', 'the OBO Library'))),
                     
                     div(id=searchbar,
                         ul(id=menuToggle,
                            li(a(href=Link,title=LinkTitle,Text)) forall pagelink(Link,LinkTitle,Text))),

                     div(class=contents,
                         P),
                     div(id=footer,
                              html:font(size='-2','--  --'))
                    )).




% ========================================
% utility (general)
% ========================================

tdpair(Tag,Val) =>
 doc:'amigo style tag:val in table list',
 html:tr(html:th(Tag),html:td(Val)).
invtdpair(Tag,Val) =>
 doc:'amigo style tag:val in table list',
 html:tr(html:td(Val),html:th(Tag)).
ntdpair(Tag,Val) =>
 doc:'amigo style tag:val in table list',
 if(Val>0,html:tr(html:th(Tag),html:td(Val))).

checkbox(Name,Val) => checkbox(Name,Val,(1=0)).
checkbox(Name,Val,Expr) =>
 if(Expr,
    then: html:input(type=checkbox,name=Name,value=Val,checked=on),
    else: html:input(type=checkbox,name=Name,value=Val)).
