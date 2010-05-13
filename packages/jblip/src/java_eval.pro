:- use_module(library(jpl)).
:- use_module(bio(bioprolog_util)).

:- op(600,xfy,(::)).
:- op(700,xfy,(:=)).

% jblip syntax
% TODO - nested calls; eg X::methodA::methodB(Foo)
goal_expansion( (V := (Ob :: M )), jpl_new(Ob,Args,V)):-
        M =.. [new|Args],
        !.
goal_expansion( (V := (Ob :: M )), jpl_call(Ob,N,Args,V)):-
        !,
        M =.. [N|Args].
goal_expansion( (Ob :: M ), jpl_call(Ob,N,Args,_)):-
        !,
        M =.. [N|Args].

:- dynamic blipvar/2.

vget(V,X):-
        blipvar(V,X).
vget(V,X,Def):-
        (   blipvar(V,X)
        ->  true
        ;   X=Def).
vset(V,X):-
        retractall(blipvar(V,_)),
        assert(blipvar(V,X)).

xxaction_callback(X):-
        vget(label,Label),
        writeln(l=Label),
        atom_number(A,X),
        Label::setText(A),
        format('Button callback: ~w ~w~n',[X,Label]).

click_button(Label):-
        writeln(l=Label),
        vget(foonum,Num),
        Num2 is Num+1,
        vset(foonum,Num2),
        atom_number(A,Num2),
        Label::setText(A),
        format('Button callback: ~w ~w~n',[Num,Label]).
        
        
action_callback(ID):-
        writeln(cb=ID),
        vget(action(ID),Act),
        writeln(act=Act),
        Act.

register_listener(Act,ID,L):-
        L := 'ButtonListener'::new,
        vget(ear_id,ID1,0),
        ID is ID1+1,
        vset(ear_id,ID),
        L::setNum(ID),
        vset(action(ID),Act).

term_expansion(swing(C),jfunc(N,swing(C),[])):-
        downcase_atom(C,N).
term_expansion(swing(C,L),jfunc(N,swing(C),L)):-
        downcase_atom(C,N).

% Swing components. Automatically expanded to jfunc/3
swing('JFrame').
swing('JMenuBar',[parent=setJMenuBar]).
swing('JMenu').
swing('JMenuItem',[callback]).
swing('JButton',[callback]).
swing('JLabel').
swing('JTable').
swing('JScrollPane').

jatom(X):- atom(X).
jatom(@(_)).
funcargs_newargs(Args,P,NewArgs):-
        findall(Arg2,
                (   member(Arg,Args),
                   \+ is_list(Arg),
                    jeval(Arg,P,Arg2)),
                NewArgs).

funcargs_kids(Args,Arg):-
        member(Arg,Args),
        is_list(Arg),
        !.
funcargs_kids(_,[]).

:- discontiguous jdefun/2, jdefun/3, jdefun/4.

%% jdefun(+Term,+Parent,?ChildTerms,?ExpandedTerm)
jdefun(X=Term,P,[],Eval):-
        jeval(Term,P,Eval),
        vset(X,Eval).
        %X=Term.
jdefun(vset(X=Term),P,[],Eval):-
        jeval(Term,P,Eval),
        vset(X,Eval).

% container is a special symbol that refers to the parent
jdefun(container,P,[],P).

jdefun(callback(Goal),P,[],''):-
        register_listener(Goal,_,Ear),
        P::addActionListener(Ear).
% Swing
jdefun(Term,P,Kids,Obj):-
        Term=..[N|Args],
        jfunc(N,swing(C),Opts),
        !,
        writeln(C-Term/Args),
        concat_atom(['javax.swing.',C],JC),
        funcargs_newargs(Args,P,NewArgs),
        funcargs_kids(Args,Kids),
        writeln(C-kids=Kids),
        writeln(C-new=JC-NewArgs),
        jpl_new(JC,NewArgs,Obj),
        writeln(C-obj=Obj),
        (   member(callback,Opts)
        ->  register_listener(callback(N,Args),_,Ear),
            Obj::addActionListener(Ear)
        ;   true),
        (   P=''
        ->  true
        ;   (   member(parent=Add,Opts)
            ->  true
            ;   Add=add),
            writeln(jpl_call(P,Add,Obj,_)),
            jpl_call(P,Add,[Obj],_)).

jdefun(jpanel(Kids),
       Parent,
       Kids,
       P):-
        jeval(swing('JPanel'),P),
	CP := Parent::getContentPane,
        CP::add(P).

jdefun(jpanel(Arg,Kids),
       Parent,
       Kids,
       P):-
        jeval(Arg,Parent,Arg2),
        jeval(swing('JPanel',Arg2),P),
	CP := Parent::getContentPane,
        CP::add(P).

jdefun(swing(C,Arg),
       P,
       [],
       JC::new(Arg2)):-
        jeval(Arg,P,Arg2),
        concat_atom(['javax.swing.',C],JC).
jdefun(swing(C),
       [],
       JC::new):-
        concat_atom(['javax.swing.',C],JC).
jdefun(Obj::Meth,
       P,
       [],
       Result):-
        jeval(Obj,P,Obj2),
        Meth=..[Name|Args],
        findall(Arg2,
                (   member(Arg,Args),
                    jeval(Arg,P,Arg2)),
                Args2),
        writeln(jcalling(Obj2,Name,Args2)),
        (   Name=new
        ->  jpl_new(Obj2,Args2,Result)
        ;   jpl_call(Obj2,Name,Args2,Result)),
        writeln(jcall(Obj2,Name,Args2)=Result).

% (+,+,?,?)
jdefun1(Head,P,Kids,Body):-
        jdefun(Head,P,Kids,Body),
        !.
jdefun1(Head,_,Kids,Body):-
        jdefun(Head,Kids,Body),
        !.

jeval(Term,Eval):-
        jeval(Term,'',Eval).
jeval(Term,P,Eval):-            % nonterminal
        %writeln(in=Term),
        jdefun1(Term,P,Kids,Body),
        !,
        %writeln(body=Body),
        jeval(Body,P,Eval),
        %writeln(eval(Term)=Eval),
        forall(member(Kid,Kids),
               jeval(Kid,Eval,_)).
jeval(Term,_,Term):-             % terminal
        %\+ compound(Term),
        !.
        %writeln(terminal=Term).
jeval(Term,P,_):-
        throw(cannot_eval(Term,in(P))).


% (+,?)
jframe(N,F):-
	F := 'javax.swing.JFrame'::new(N).

jmenubar(F,MenuBar):-
        MenuBar := 'javax.swing.JMenuBar'::new,
        F::setJMenuBar(MenuBar).

jmenu(MenuBar,N,Menu):-
        Menu := 'javax.swing.JMenu'::new(N),
        MenuBar::add(Menu).

jmenuitem(Menu,N,Item):-
        Item := 'javax.swing.JMenuItem'::new(N),
        Menu::add(Item).

jlabel(P,N,L):-
	L := 'javax.swing.JLabel'::new(N),
        P::add(L).

jbutton(P,N,B):-
	B := 'javax.swing.JButton'::new(N),
        P::add(B).
%	CP := F::getContentPane,
%	CP::add(B).

jpanel(CP,P):-
        GL := 'java.awt.GridLayout'::new(0,1),
        P := 'javax.swing.JPanel'::new(GL),
        CP::add(P).

% (+,?,?)
jlist(F,L,DLM):-
        jlist(F,L,DLM,[]).

% (+,?,?,+)
jlist(F,L,DLM,Es):-
	DLM := 'javax.swing.DefaultListModel'::new,
	L := 'javax.swing.JList'::new(DLM),
	CP := F::getContentPane,
	CP::add(L),
        forall(member(E,Es),
               DLM::addElement(E)).
        
/** <module> Functional-style wrapper for Java

  ---+ Synopsis

==
:- use_module(bio(java_eval)).

% 
demo:-
  nl.
  

==

---+ Details



@author  Chris Mungall
@version $Revision$
@see     README, JPL
@license License


*/


