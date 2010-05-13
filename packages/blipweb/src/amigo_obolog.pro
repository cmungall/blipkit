:- use_module(bio(serval)).
:- use_module(bio(obolog_db)).

relation_summary_axioms([transitive/1,symmetric/1,anti_symmetric/1,reflexive/1,holds_between/4]).
relation_detail_axioms([subrelation/2,transitive_over/2,holds_over_chain/3,homeomorphic_for/2,inverse_of/2,reciprocal_relation/2,all_some/2,all_some_all_times/2,all_some_in_reference_context/3,related_synonym/2,exact_synonym/2,comment/2,example/3,equivalent_to/2,equivalent_to_chain/2,text_definition/2,text_definition_xref/2]).
relation_summary_axiom(A):- relation_summary_axioms(L),member(A,L).
relation_detail_axiom(A):- relation_detail_axioms(L),member(A,L).


%displayed_relation(R):- relation(R),\+ inverse_of(R,_),\+ reciprocal_relation(R,_),type_type(R).
%displayed_relation(R):- relation(R),\+ inverse_of(R,_),type_type(R).

% all type level relations
% displayed_relation(R):- relation(R),\+( (class_instance_relation_pair(R,PR),inverse_of(PR,PRInv),normative_direction_for(PRInv,_)) ),type_type(R).
% all instance relations lacking type counterparts
% displayed_relation(R):- relation(R), \+ type_type(R), \+ class_instance_relation_pair(_,R), \+( (inverse_of(R,RInv),normative_direction_for(RInv,_)) ).

% all type level relations
displayed_relation(type,R):- relation(R),type_type(R),\+( (class_instance_relation_pair(R,PR),inverse_of(PR,PRInv),normative_direction_for(PRInv,_)) ).
% all instance relations lacking type counterparts
displayed_relation(instance,R):- relation(R), \+ type_type(R), \+( (inverse_of(R,RInv),normative_direction_for(RInv,_)) ).
displayed_relation(relation,R):- metarelation(R).

relation_page =>
  relation_page(ro).

relation_page(Ont) =>
 html(
      head(
           title('OBO Relation Ontology: ',Ont),
           header_links),
      body(include_file('www/header.html'),
           include_file('www/menu.html'),
           div(id=main_container,
               div(id=main_content,
                   include_file('www/intro.html'),
                   download_table(Ont),
                   relation_summary_tables,
                   a(id=formats,[]),
                   include_file('www/formats.html')),
               h1('Relation Details'),
               p('Note both class ', i(' and '), ' instance level relations are listed here for completeness'),
               relation_detail_table),
           div(id=sidebar_container,
               div(id=sidebar_content,
                   include_file('www/links.html'))))).


header_links =>
 html:meta(http-equiv='Content-Type',content='text/html; charset=UTF-8'),
 link(rel='stylesheet',href='stylesheet.css',type='text/css'),
 link(rel='stylesheet',href='../stylesheet.css',type='text/css'),
 html:style(type='text/css',CSS) forall_unique css(CSS),
 %link(href='http://www.obofoundry.org/css/main.css',media='all',rel='Stylesheet',type='text/css'),
 %link(href='http://www.obofoundry.org/css/table.css',media='all',rel='Stylesheet',type='text/css'),
 %link(href='http://www.obofoundry.org/css/menu.css',media='all',rel='Stylesheet',type='text/css'),
 %link(href='http://www.obofoundry.org/css/detail.css',media='all',rel='Stylesheet',type='text/css'),
 %link(href='http://www.obofoundry.org/css/list.css',media='all',rel='Stylesheet',type='text/css'),
 link(rel='shortcut icon',href='http://www.obofoundry.org/images/favicon.ico',type='image/x-icon').

css(' ul { list-style-type: none; }
 li { list-style-type: none; }
.detail td.axiom{ width: 80%; text-align: left; background-color: #ccddcc; }
.detail td.axiom a:hover { color: #ffffff; background: #1b5c92 }').



download_format(kif,'.kif').
download_format(obo,'.obo').
download_format('obo [I]','-ii.obo').
download_format('obo [C]','-cc.obo').
download_format(owl,'.owl').
download_format(prover9,'.prover9').

download_format(_,kif,'.kif').
%%%%%download_format(_,obo,'.obo').
download_format(instance,'obo [I]','-ii.obo').
download_format(type,'obo [C]','-cc.obo').
download_format(instance,owl,'.owl').
download_format(_,prover9,'.prover9').

download_table(Ont)=>
 div(class=download,
     'Download: ',
     [ call(concat_atom([Ont,Suffix],URL)),
       span('[',a(href=URL,b(Fmt)),']')] forall_unique download_format(Fmt,Suffix),
     ' :: See ',a(href='#formats',formats)).


relation_summary_tables =>
 div(id=summary,
     type_summary_table('Types'),
     relation_summary_table(relation,'Meta level relations'),
     relation_summary_table(type,'Type level relations'),
     relation_summary_table(instance,'Instance level relations')).

type_summary_table(Title) =>
  table(class=themeTable,
        td(class=contentRow,colspan=8,'Types ',Title,' : Summary Table'),
                                % column headings
        type_summary(T) forall_unique type(T)).

type_summary(T) =>
 call(newRow(RowClass)),
 tr(td(class=RowClass,labelurl(T)),
    map(Ax,relation_summary_colval_td(T,Ax,RowClass),AxL) where relation_summary_axioms(AxL)).


relation_summary_table(T,Title) =>
 if(displayed_relation(T,_),
    table(class=themeTable,
          td(class=contentRow,colspan=8,'OBO Relations : ',Title,' : Summary Table'),
                                % column headings
          tr(th(relation),map(Ax/_,th(Ax),AxL) where relation_summary_axioms(AxL)),
                                % cell values
          relation_summary(R) forall_unique displayed_relation(T,R),
                                % repeat column headings
          tr(th(relation),map(Ax/_,th(Ax),AxL) where relation_summary_axioms(AxL)))).

relation_summary(R) =>
 call(newRow(RowClass)),
 tr(td(class=RowClass,labelurl(R)),map(Ax,relation_summary_colval_td(R,Ax,RowClass),AxL) where relation_summary_axioms(AxL)).

relation_summary_colval_td(R,Ax,RowClass) =>
 td(class=RowClass,
    relation_summary_colval(R,Ax)).



relation_summary_colval(R,example/3) =>
 div(fol_infix(ExFun),' [',Src,']') forall_unique get_example(R,ExFun,Src).

get_example(R,F,Src):-
        example(R,F,Src).
get_example(R,F,'-'):-
        example(R,F).

relation_summary_colval(R,Ax/1) =>
 call(Goal=..[Ax,R]),
 if(Goal,
    then: trueValue(R),
    else:''),
 axiom_annotation(Goal).

relation_summary_colval(R,Ax/2) =>
 call(Goal=..[Ax,R,V]),
 if(Goal,
    then: [data(V),axiom_annotation(Goal)],
    else:'').

relation_summary_colval(R,holds_between/4) =>
 span(label(X),i(' and '),label(Y),' ',html:sup(T)) forall_unique holds_between(R,X,Y,T).



relation_summary_colval(R,Ax/N) =>
 call( (functor(Goal,Ax,N),Goal=..[Ax,R|L]) ),
 if(Goal,
    then: [L,axiom_annotation(Goal)],
    else:'').


relation_detail_table =>
 div(id=detail,
     write_relation_detail_page(R) forall_unique (relation(R) ; metarelation(R)),
     write_type_detail_page(T) forall_unique type(T)).

write_relation_detail_page(R) =>
 call(localurl(R,Loc)),
 write_file(Loc,relation_detail_page(R)),
 call((localurl(R,F,kif),
       tell(F),
       ensure_loaded(bio(obolog_writer_kif)),
       forall(relation_axiom_direct(R,_,Ax),
              write_formula(Ax)),
       forall(relation_axiom_indirect(R,_,Ax),
              write_formula(Ax)),
       told)).

write_type_detail_page(R) =>
 call(localurl(R,Loc)),
 write_file(Loc,type_detail_page(R)).

relation_detail_page(R) =>
  html(
      head(
           title('OBO Relation Ontology : ',label(R)),
           header_links),
      body(include_file('www/header.html'),
           include_file('www/menu.html'),
           div(id=main_container_aux,
               div(id=main_content_aux,
                   relation_detail(R))))).

type_detail_page(R) =>
  html(
      head(
           title('OBO Relation Ontology : ',label(R)),
           header_links),
      body(include_file('www/header.html'),
           include_file('www/menu.html'),
           div(id=main_container_aux,
               div(id=main_content_aux,
                   type_detail(R))))).

unary_display_property(R,Prop):-
        relation_summary_axiom(Prop/1),
        Test=..[Prop,R],
        Test.


relation_detail(R) =>
 a(name=R),
 div(class=catch_phrase,label(R),
     ' (',data(R),')',
     if(type_type(R), i(' -- class level'))),
 div(class=detail,
     p(class=definition,
       markup(Text),
       span(class=xref,'source: ',labelurl(X)) forall_unique text_definition_xref(R,X)) forall_unique text_definition(R,Text),
     p(class=comment,markup(Text)) forall_unique formula(comment(R,Text)),
     h3('Examples'),
     ul(li(fol_infix(ExFun),' [',Src,']') forall_unique get_example(R,ExFun,Src)),
     h3('Other relations'),
     (   all_some_all_times(R,RI)
     ->  p(class=axiom,'This relation holds in a ',i('all-some-all-times'),' over the instance-level relation: ',labelurl(RI))
     ;   (   all_some_tr(R,RI)
         ->  p(class=axiom,'This relation holds in a ',i('all-some-restricted-times'),' over the instance-level relation: ',labelurl(RI))
         ;   (   all_some_in_reference_context(R,RI,RR)
             ->  p(class=axiom,'This relation holds in a ',i('all-some'),' fashion in a ',i('reference context'),' over the instance-level relation: ',labelurl(RI), ' for the reference context relation: ',labelurl(RR))
             ;   []))),
     p(class=axiom,
       (   inverse_of(R,InvR)
       ->  p('This relation is the inverse of ',labelurl(InvR))
       ;   'This relation has no inverse relations declared')),
     (   class_instance_relation_pair(R,RP),
         inverse_of(RP,RPI),
         class_instance_relation_pair(RR,RPI)
     ->  p(class=axiom,'Note that on the instance-level, ',labelurl(RP),' is the inverse of ',labelurl(RPI),', the instance form of ',labelurl(RR))
     ;   []),
     table(
           tr(td(id),td(class=data,R)),
           tr(td(name),td(class=data,label(R))),
           tr(td(properties),td(class=data, ul(li(data(Prop)) forall_unique unary_display_property(R,Prop)))),
           tr(td(aliases),td(class=data, ul(li(data(Text),' [',data(X),']') forall_unique scoped_synonym(X,R,Text)))),
           relation_detail_tag(R,Ax) forall_unique relation_summary_axiom(Ax),
           relation_detail_tag(R,Ax) forall_unique relation_detail_axiom(Ax)),
                                %tr(td(colspan=2,halign=left,
                                %      p('Axioms for this relation:'))),
                                %tr(td(colspan=2,halign=left,
                                %            p('Axioms that refer to this relation:'))),
     p('Axioms for this relation:'),
     table(axiom(T,Ax) forall_unique relation_axiom_direct(R,T,Ax)),
     p('Axioms that refer to this relation:'),
     table(axiom(T,Ax) forall_unique relation_axiom_indirect(R,T,Ax)),
     img(src=ImgURL,[]) forall_unique image_xref(R,ImgURL)).

type_detail(R) =>
 a(name=R),
 div(class=catch_phrase,label(R),
     ' (',data(R),')',
     if(type_type(R), i(' -- class level'))),
 div(class=detail,
     p(class=definition,
       markup(Text),
       span(class=xref,'source: ',labelurl(X)) forall_unique text_definition_xref(R,X)) forall_unique text_definition(R,Text),
     p(class=comment,markup(Text)) forall_unique formula(comment(R,Text)),
     h3('Examples'),
     ul(li(fol_infix(ExFun),' [',Src,']') forall_unique get_example(R,ExFun,Src)),
     table(
           tr(td(id),td(class=data,R)),
           tr(td(name),td(class=data,label(R))),
           tr(td(properties),td(class=data, ul(li(data(Prop)) forall_unique unary_display_property(R,Prop)))),
           tr(td(aliases),td(class=data, ul(li(data(Text),' [',data(X),']') forall_unique scoped_synonym(X,R,Text))))),
     p('Axioms for this type:'),
     table(axiom(T,Ax) forall_unique relation_axiom_direct(R,T,Ax)),
     p('Axioms that refer to this type:'),
     table(axiom(T,Ax) forall_unique relation_axiom_indirect(R,T,Ax)),
     img(src=ImgURL,[]) forall_unique image_xref(R,ImgURL)).


axiom(_T,Ax) =>
 tr(td(axiom),
    td(class=axiom,
       if(formula_comment(Ax,Comment),
          i(noesc(Comment))),
       fol(Ax))).

infix(R):- formula(html_infix_symbol(R,_)).



fol_infix(X) => call(X=..[P|L]),folj([' ',labelurl(P),' '],L).

fol('<=>'(X,Y)) =>
 ul(li(fol(X),noesc(' &harr; ')),
    li(fol(Y))).
fol('=>'(X,Y)) =>
 ul(li(fol(X),noesc(' &rarr; ')),
    li(fol(Y))).
fol('='(X,Y)) =>
 fol(X),noesc(' = '),fol(Y).
fol(exists(X,Y)) =>
 noesc(' &exist; '),fol(X),'[',fol(Y),']'.
fol(forall(X,Y)) =>
 noesc(' &forall; '),fol(X),'[',fol(Y),']'.
fol(not(X)) =>
 noesc(' &not; '),'(',fol(X),')'.
fol([]) => [].
fol([H]) => fol(H).
fol([H|T]) => fol(H),', ',fol(T).

fol(X) =>
 if(atomic(X),
    then: if(atom_concat('?',Var,X),
             then: i(b(Var)),
             else: folsymbol(X)),
    else: [call(X=..[P|Args]),
           fol(P,Args)]).
fol(intersection_of,L) => ul(folj(noesc(' &and; '),L)).
fol(union_of,L) => ul(folj(noesc(' &or; '),L)).
fol(and,L) => ul(folj(noesc(' &and; '),L)).
fol(or,L) => folj(noesc(' &or; '),L).
folj(_,[]) => [].
folj(_,[H]) => li(fol(H)).
folj(P,[H|T]) => li(fol(H),P),folj(P,T).

%fol(_,[]) => [].
%fol(_,[H]) => fol(H).
fol(P,L) =>
  if( (debug(obolog,'infix?: ~w',[P]),infix(P),L=[X,Y]),
      then: [fol(X),labelurl(P),fol(Y)],
      else: [labelurl(P),'(',fol(L),') ']).

folsymbol(X) =>
  labelurl(X).

axiom_annotation(Goal) =>
 debug(ro,'G: ~w',Goal),
 if(obolog_db:comment(Goal,Comment),
    then: [
           a(href='',alt=Comment,'*')],
    else: '').

relation_detail_tag(R,equivalent_to/2) =>
 tr(td('logical definition'),
     td(class=data,
        fol(RX))) forall_unique equivalent_to(R,RX).
relation_detail_tag(R,equivalent_to_chain/2) =>
 tr(td('logical definition'),
     td(class=data,
        fol(R1),b(' . '),fol(R2))) forall_unique equivalent_to_chain(R,R1,R2).

relation_detail_tag(R,Ax/1) =>
 call(Goal=..[Ax,R]),
 tr(td(Ax),
    td(class=data,
       if(Goal,
          then: data('true'),
          else: ''))).

relation_detail_tag(R,Ax/2) =>
 call(Goal=..[Ax,R,V]),
 tr(td(Ax),
    td(class=data,
       info(V))) forall_unique obolog_db:Goal.


relation_detail_tag(R,holds_over_chain/3) =>
 tr(td('holds_over'),
     td(class=data,
        info(R1),b('.'),info(R2))) forall_unique holds_over_chain(R,R1,R2).

relation_detail_tag(R,Ax/N) =>
 tr(td(Ax),
    td(class=data,
       relation_summary_colval(R,Ax/N))).

trueValue(_R) =>
 b('+').

old__trueValue(R) =>
 '+',
 if(type_type(R),
    then: html:sup('C'),
    else: html:sup('I')).

cPlus => '+',html:sup('C').
iPlus => '+',html:sup('I').

markup(Text) => noesc(Text).
%markup(Text) =>
% call((atom_chars(Text,Tokens))),
% markup_list(Tokens).

markup_list(['@'|L]) =>
 markup_list(L,markup([])).

markup_list([H|L]) =>
 noesc(H),
 markup_list(L).

markup_list([]) => [].

% in-markup
markup_list(['@'|L],markup(ML)) =>
 call((reverse(ML,RML),concat_atom(RML,W))),
 labelurl(W),
 markup_list(L).

markup_list([H|L],markup(ML)) =>
 markup_list(L,markup([H|ML])).

markup_list([],markup(L)) => call(throw(unterminated_markup(L))).


/*
markup(Text) =>
 call((atom_chars(Text,Chars),
       dcg_markup(MarkedUpChars,Chars,[]),
       concat_atom(MarkedUpChars,MarkedUpText))),
 data(MarkedUpText).

dcg_markup([E|L]) --> markup_element(E),!,dcg_markup(L).
dcg_markup([]) --> [].

markup_element(W) --> ['@'],markup_word(W),['@'],!.
markup_element(E) --> [E].
markup_word(W) --> nonws_tokens(L),{atom_chars(W,L)}.
nonws_tokens([H|T]) --> [H],{H\=' ',H\='@'},!,nonws_tokens(T).
nonws_tokens([]) --> [].
*/

info(R) =>
 if(relation(R),
    then: labelurl(R),
    else: data(R)).

labelurl(R) =>
 if(type_type(R),
    then: i(labelurl2(R)),
    else: b(labelurl2(R))).

html_symbol(R,HTML):- formula(html_symbol(R,HTML)).
html_symbol(R,HTML):- formula(html_infix_symbol(R,HTML)).

labelurl2(R) =>
 if(html_symbol(R,HTML),
    then: locallink(R,noesc(HTML)),
    else:
   if(label(R,Label),
      then: locallink(R,Label),
      else: locallink(R,R))).
%      else: R)).

locallink(R,Label) =>
 call(fullurl(R,URL)),
 a(href=URL,Label).

fullurl(R,URL):-
        localurl(R,LocalURL),
%        URL=LocalURL.
%        atom_concat('/ro/',LocalURL,URL).
        atom_concat('/~cjm/ro/',LocalURL,URL).
localurl(R,URL):-
        (   concat_atom([DB,LocalID],':',R)
        ->  concat_atom([DB,'/',LocalID,'.html'],URL)
        ;   concat_atom(['OBO_REL_TEMP','/',R,'.html'],URL)).
localurl(R,URL,Fmt):-
        (   concat_atom([DB,LocalID],':',R)
        ->  concat_atom([DB,'/',LocalID,'.',Fmt],URL)
        ;   concat_atom(['OBO_REL_TEMP','/',R,'.',Fmt],URL)).


old__labelurl(R) =>
 call(atom_concat('#',R,URL)),
 a(href=URL,label(R)).

     
label(R) =>
 if(label(R,Label),
    then: Label,
    else: R).


% this is REALLY hacky     
:- dynamic oddRow/0.

newRow(oddRow):-
        oddRow,
        !,
        retractall(oddRow).
newRow(evenRow):-
        !,
        assert(oddRow).

        
            

