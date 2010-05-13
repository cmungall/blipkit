:- use_module(bio(serval)).
:- use_module(bio(obolog_db)).

nl => '\n'.
para(Text) => Text,nl,nl.
tex(Type,Text) => data('\\'),Type,'{',Text,'}',nl.
tex(Type,X,Text) => data('\\'),Type,'[',X,']{',Text,'}',nl.
emph(X) => data('\\emph{'),latex(X),'}'.
document(L) => tex(begin,data(document)),L,tex(end,data(document)).
( \X ) => data('\\'),data(X),' '.
sdefun(section(Title,Text),
       [tex(data('section'),Title),Text,nl]).
sdefun(subsection(Title,Text),
       [tex(data('subsection'),Title),Text,nl]).
sdefun(subsubsection(Title,Text),
       [tex(data('subsubsection'),Title),Text,nl]).
clist([]) => [].
clist(L) => tex(begin,clist),L,tex(end,clist).
item(X) => \item, X, nl.
block(Type,X) => tex(begin,Type),X,nl,tex(end,Type).
verbatim(X) => block(verbatim,X).

axiombox(A) => block('eqnarray*',A).
%axiombox(A) => '\\[',nl,A,'\\]',nl.
%axiombox(A) => '$$',nl,A,'$$',nl.
markupvar(X) => noesc(X).
pr(X) => data(' \\pr{'),latex(X),'}'.
abbrevs => ''.


relation_summary_axioms([transitive/1,symmetric/1,anti_symmetric/1,functional/1,reflexive/1,domain/2,range/2,holds_between/4]).
relation_detail_axioms([subrelation/2,homeomorphic_for/2,inverse_of/2,reciprocal_relation/2,all_some/2,all_some_all_times/2,related_synonym/2,exact_synonym/2,comment/2,example/3,text_definition/2]).
relation_summary_axiom(A):- relation_summary_axioms(L),member(A,L).
relation_detail_axiom(A):- relation_detail_axioms(L),member(A,L).

unary_display_property(R,Prop):-
        relation_summary_axiom(Prop/1),
        Test=..[Prop,R],
        Test.

% all type level relations
displayed_relation(unary,R):- unary(R).
displayed_relation(type,R):- relation(R),type_type(R),\+( (class_instance_relation_pair(R,PR),inverse_of(PR,PRInv),normative_direction_for(PRInv,_)) ).

% all instance relations lacking type counterparts
displayed_relation(instance,R):- relation(R), \+ type_type(R), \+( (inverse_of(R,RInv),normative_direction_for(RInv,_)) ).
displayed_relation(relation,R):- metarelation(R).

% literate-programming style: write each formula in order
all_formulae =>
 lp_formula(X) forall formula(X).

lp_formula(function(X)) => subsubsection(['Function: ',label(X)],''),nl.
lp_formula(relation(X)) => subsubsection(['Relation: ',label(X)],''),nl.
lp_formula(unary(X)) => subsubsection(['Unary predicate: ',label(X)],''),nl.
lp_formula(metarelation(X)) => subsubsection(['Meta-Relation: ',label(X)],''),nl.
lp_formula(text_definition(_,X)) => latex(X),nl,nl.
lp_formula(comment(_,X)) => latex(X),nl,nl.
lp_formula(abstract(X)) => tex(begin,abstract),latex(X),tex(end,abstract).
lp_formula(section(T,X)) => section(T,latex(X)).
lp_formula(subsection(T,X)) => subsection(T,latex(X)).
lp_formula(subsubsection(T,X)) => subsubsection(T,latex(X)).
lp_formula(example(_,X)) => 'Example: ',axiom(X).
lp_formula(provable_example(X)) => 'Example: ',axiom(X).
lp_formula(X) => axiom('',X).

relation_document =>
  relation_document(ro).

relation_document(Ont) =>
 tex(documentclass,'11pt',article),
 tex(input,logicmacros),
 tex(title,Ont),
 tex(author,auto),
 abbrevs,
 document([
           \maketitle,
           \tableofcontents,
           if(formula(abstract(Abstract)),
              section('Abstract',
                     latex(Abstract))),
           relation_sections,
           section('Acknowledgements',
                   [blah])
           ]).

relation_sections =>
 relation_detail_sections(relation,'Meta level relations'),
 relation_detail_sections(instance,'Instance level relations'),
 relation_detail_sections(type,'Type level relations').

relation_sections_combined =>
 relation_detail_section(R) forall displayed_relation(_,R).


relation_detail_sections(T,Title) =>
 if(displayed_relation(T,_),
    section(Title,
            [
             relation_detail_section(R) forall displayed_relation(T,R)
            ])).

relation_detail_section(R) =>
 subsection([label(R)],
            [
             para([emph(X),if(text_definition_xref(R,XX),['(',XX,')'])]) forall
            text_definition(R,X),

             para(latex(X)) forall formula(comment(R,X)),

             if((get_example(R,_,_)),
                subsubsection('Examples',
                              clist(example(X,O)
                                   forall get_example(R,X,O)))),

             subsubsection('Axioms and Theorems',''),
             %subsubsection('Other relations',
             (   all_some_all_times(R,RI)
                           ->  'This relation holds in a \\emph{all-some-all-times} over the instance-level relation: ',label(RI)
                           ;   (   all_some_tr(R,RI)
                              ->  'This relation holds in a \\emph{all-some-restricted-times} over the instance-level relation: ',label(RI)
                               ;   (   all_some_in_reference_context(R,RI,Context)
                                   ->  'This relation holds in a \\emph{all-some} fashion in a \\emph{reference context} ',label(Context),' over the instance-level relation: ',label(RI)
                                   ;   []))),
             
            %subsubsection('Properties',
             [
              if(unary_display_property(R,_),
                 clist(item(Prop) forall unary_display_property(R,Prop))
                ),
              (   inverse_of(R,InvR)
              ->  'This relation is the inverse of ',label(InvR)
              ;   '')
              %;   'This relation has no inverse relations declared')
             ],
             nl,
             (	 class_instance_relation_pair(R,RP),
                 inverse_of(RP,RPI),
                 class_instance_relation_pair(RR,RPI)
             ->  'Note that on the instance-level, ',label(RP),' is the inverse of ',label(RPI),', the instance form of ',label(RR)
             ;   []),
             
            %tr(td(id),td(class=data,R)),
            %tr(td(name),td(class=data,label(R))),
            %tr(td(aliases),td(class=data, ul(li(data(Text),' [',data(X),']') forall scoped_synonym(X,R,Text)))),
            %relation_detail_tag(R,Ax) forall relation_summary_axiom(Ax),
            %relation_detail_tag(R,Ax) forall relation_detail_axiom(Ax)),
             
             if(relation_axiom_direct(R,_,_),
		%subsubsection('Axioms for this relation',
                axiom(T,Ax) forall_unique relation_axiom_direct(R,T,Ax)),
             if(relation_axiom_indirect(R,_,_),
		%subsubsection('Axioms that refer to this relation',
                axiom(T,Ax) forall_unique relation_axiom_indirect(R,T,Ax))
	    ]).
            %img(src=ImgURL,[]) forall image_xref(R,ImgURL)).


get_example(R,F,Src):-
        example(R,F,Src).
get_example(R,F,'-'):-
        example(R,F).

example(Ex,Ont) =>
 item([latex(Ont),' : ',axiom(_,Ex)]).
example(Ex) =>
 item([axiom(_,Ex)]).


relation_detail_tag(R,text_definition/2) =>
 if(text_definition(R,D),
    [latex(D),
     if(text_definition_xref(R,X),
        X),
     nl,nl]).

relation_detail_tag(R,comment/2) =>
 if(comment(R,X),
    [latex(X),nl,nl]).

relation_detail_tag(R,Ax/2) =>
 call(Goal=..[Ax,R,V]),
 if(Goal,
    [
     latex(Ax),':',
     label(V) forall_unique obolog_db:Goal,
     nl,
     nl]).

relation_detail_tag(R,Ax/1) =>
 call(Goal=..[Ax,R]),
 if(Goal,[latex(Ax),nl,nl]).


relation_detail_tag(_R,Ax/_N) =>
 latex(Ax),
 nl,
 nl.

axiom(_,Ax) =>
 axiom(Ax).
axiom(Ax) =>
 if(formula_comment(Ax,Comment),
    para(emph(Comment))),
 axiombox(fol(Ax)).


newcol => noesc('&').
fmtnl(rhs) => '\\\\',nl,newcol,newcol.
fmtnl(_) => '\\\\',nl.

infix(R):- formula(latex_infix_symbol(R,_)).

fol_infix(X) => call(X=..[P|L]),folj([' ',label(P),' '],L).

fol(X) => fol(top,X).

fol(top,'<=>'(X,Y)) =>
 fol(lhs,X),newcol,\dimp,newcol,fol(rhs,Y).
fol(D,'<=>'(X,Y)) =>
 fol(D,X),\dimp,fol(D,Y).
fol(top,'=>'(X,Y)) =>
 fol(lhs,X),newcol,\imp,newcol,fol(rhs,Y).
fol(D,'=>'(X,Y)) =>
 fol(D,X),\imp,fol(D,Y).
fol(D,'='(X,Y)) =>
 fol(D,X),noesc(' = '),fol(D,Y).
fol(D,exists(X,Y)) =>
 noesc(' \\E '),call(X=..L),fol(D,L),'[',fol(D,Y),']'.
fol(D,forall(X,Y)) =>
 noesc(' \\A '),call(X=..L),fol(D,L),'[',fol(D,Y),']'.
fol(D,not(X)) =>
 noesc(' \\n'),'(',fol(D,X),')'.
fol(_,[]) => [].
fol(D,[H]) => fol(D,H).
fol(D,[H|T]) => fol(D,H),', ',fol(D,T).

fol(D,X) =>
 if(atomic(X),
    then: if(atom_concat('?',Var,X),
             then: markupvar(Var),
             else: folsymbol(X)),
    else: [call(X=..[P|Args]),
           fol(D,P,Args)]).
fol(D,and,L) => folj(D,[\con,fmtnl(D)],L).
fol(D,or,L) => folj(D,\dis,L).
folj(_,_,[]) => [].
folj(D,_,[H]) => fol(D,H).
folj(D,P,[H|T]) => fol(D,H),P,folj(D,P,T).

%fol(_,[]) => [].
%fol(_,[H]) => fol(H).
fol(D,P,L) =>
  if( (infix(P),L=[X,Y]),
      then: [fol(D,X),label(P),fol(D,Y)],
      else: [fol(D,P),'(',fol(D,L),') ']).

folsymbol(X) =>
  label(X).

axiom_annotation(Goal) =>
 debug(ro,'G: ~w',Goal),
 if(obolog_db:comment(Goal,Comment),
    then: [
           a(href='',alt=Comment,'*')],
    else: '').


trueValue(_R) =>
 b('+').


latex_symbol(R,LATEX):- formula(latex_symbol(R,LATEX)).
latex_symbol(R,LATEX):- formula(latex_infix_symbol(R,LATEX)).

atom_latex(A,AX):-
        concat_atom(Toks,'_',A),
        concat_atom(Toks,'\\_',AX).



latex(A) => call(atom_latex(A,X)),noesc(X).
        
label(R) =>
 if(formal_label(R,Label),
    then: hi(Label),
    else:
   if(label(R,Label),
      then: hi(Label),
      else: hi(R))).

hi(R) =>
 if(type_type(R),
    then: \emph(R),
    else: pr(R)).

