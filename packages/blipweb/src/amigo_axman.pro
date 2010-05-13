:- use_module(bio(serval)).
:- use_module(bio(obolog_db)).
:- [amigo_core].

init_page(main).

allowed_tab(predicates).
allowed_tab(entities).
allowed_tab(sentences).
allowed_tab(modules).
allowed_tab(texts).

argpos(0-0,'Predicate').
argpos(1-1,1).
argpos(2-2,2).
argpos(3-99,'3+').

tab(entities)=>
  doc:'Shows all entities',
  localhref(X,X) forall argpos(_,X),
  div(class=block,
      a(name=N),
      h3(N),
      html:div(ul(li(labelurl(E)) forall_unique (named_entity(E,Pos),
                                                 Pos>=From,
                                                 Pos=<To))))
  forall argpos(From-To,N).

tab(predicates)=>
  doc:'Shows all predicates',
  html:div(ul(li(labelurl(X)) forall_unique atomic_predicate(X))).

tab(sentences)=>
  doc:'Shows all axioms',
  html:div(ul(li(axiom(X)) forall_unique formula(X))).

tab(_) =>
  'uh oh??'.


main =>
 outer('Axiom Manager',
       div(query_box,
           getparam(tab,Tab,predicates),
           tabbar(Tab),
           menu(Tab),
           in(S,
              if(getparam(S,entity,E),
                 then: view_entity(E,Tab),
                 else: tab(Tab))),
           footer)).

footer => br,p('----').

http_param_label(commit_sentence,'Commit').
add_sentence =>
  add_sentence('').

add_sentence( Msg )=>
 outer('Add sentence',
       div(add,
           form(name=sentence,
                input(name=comment),
                br,
                edit_button('&harr;'),
                edit_button('&rarr;'),
                edit_button('&exist;'),
                edit_button('&forall;'),
                br,
                textarea(id=body,name=body,style='height: 100px; width: 400px;'),
                a(href='javascript:launchWithTextarea(615,620,\'body\')',open),
                Msg,
                submit(commit_sentence)))).

edit_button(X) =>
 %call(sformat(JS,'JavaScript:document.forms[&quot;sentence&quot;].body.append(\'~w\');',[X])),
 %call(sformat(JS,'JavaScript:alert(document.forms[&quot;sentence&quot;].body);',[])),
 call(sformat(JS,'JavaScript:document.forms[\'sentence\'].body.value+=\'~w\';',[X])),
 %call(sformat(JS,'JavaScript:alert(\'xxx\');',[])),
 html:input(type=button,
            onClick=noesc(JS),
            value=noesc(X)).




tabbar(CurrTab)=>
  ul(class=filter_tab,
     if(Tab=CurrTab,
        then: li(class=active_filter_tab,Tab),
        else: li(class=inactive_filter_tab,ilink(Tab,tab=Tab)))
       forall_unique allowed_tab(Tab)).

query_box =>
  html:input(name=search).

view_entity(E,_Tab) =>
 div(class=infobox,
     table(class=tagval_table,
           tdpair('Entity',E),
           summarize('References',(formula(S),formula_references(S,E)),S),
           summarize('References as pred',predicate_formula(E,S),S))).

menu(_) =>
  ul(li(ilink('Add sentence',action=add_sentence))).

summarize(Desc,Goal,Template) =>
  call(setof_count(Template,Goal,Num)),
  tdpair(Desc,Num).

% cut and pasted from amigo_obolog
axiom(Ax) =>
 tr(td(axiom),
    td(class=data,
       fol(Ax),
       a(href='',onClick=edit,'[ed]'))).

infix(R):- formula(html_infix_symbol(R,_)).

fol_infix(X) => call(X=..[P|L]),folj([' ',labelurl(P),' '],L).

fol('<=>'(X,Y)) =>
 fol(X),noesc(' &harr; '),fol(Y).
fol('=>'(X,Y)) =>
 fol(X),noesc(' &rarr; '),fol(Y).
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
fol(and,L) => folj(noesc(' &and; '),L).
fol(or,L) => folj(noesc(' &or; '),L).
folj(_,[]) => [].
folj(_,[H]) => fol(H).
folj(P,[H|T]) => fol(H),P,folj(P,T).

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


labelurl(Entity) =>
 doc:'shows hyperlinked entity label',
 getparam(tab,Tab,entities),
 call(sformat(Link,'?entity=~w&tab=~w',[Entity,Tab])),
 a(class=info,href=Link,Entity).

% CONTROLLER

strans(_-A,S-S,
       % pre:
       getparam(S,action,A),
       % post:
       true).

strans(_-add_sentence(Msg),S-S,
       % pre:
       submit_param(S,commit_sentence),
       % post:
       (   getparam(S,body,Text),
           (   parse_sentences(Text,Sents),
               maplist(add_sentence,Sents)
           ->  Msg='sentence added successfully'
           ;   Msg='Cannot parse'))).

parse_sentences(Text,Sent):-
        ensure_loaded(bio(sxpr_parser)),
        atom_codes(Text,Codes),
        codes_sxprs(Codes,Sxprs),
        maplist(sxpr_prolog,Sxprs,Sents).

add_sentence(Sent):-
        assert_formula(Sent).



