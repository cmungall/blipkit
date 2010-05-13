:- [java_eval].

:- use_module(bio(ontol_db)).
:- use_module(bio(io)).

callback(menuitem,N):-
        writeln(selected=N).
callback(button,N):-
        writeln(button=N).
callback_load(_Frame,R):-
        load_bioresource(R).
chooser_open_file(N):-
        writeln(open(var=N)),
        vget(N,F),
        writeln(f=F),
        jpl_list_to_array([a,b],Hdr),
        writeln(xx=Hdr),
        jpl_new('javax.swing.JFileChooser',[],FC),
        %FC := 'javax.swing.JFileChooser'::new,
        writeln(fc=FC),
        RV := FC::showOpenDialog(F),
        writeln(ret=RV),
        File := FC::getSelectedFile,
        FilePath := File::getPath,
        writeln(fc=FilePath),
        consult(FilePath).

show_classes(FrameName):-
        vget(FrameName,Frame),
        writeln('showing props'),
        FClass := Frame::toString,
        JL := 'javax.swing.JLabel'::new('foo!'),
        writeln(jl=JL),
        Frame::add(JL).

show_properties(FrameName):-
        vget(FrameName,Frame),
        writeln('showing props'),
        FClass := Frame::toString,
        writeln(frame=Frame/FClass),
        forall(property(P),
               (   writeln(p=P),
                   JL := 'javax.swing.JLabel'::new(P),
                   writeln(jl=JL),
                   Frame::add(JL))).



callback(X,N):-
        writeln(X=N).

wibble(X):-
        writeln(wibble(X)).

t:-
        'javax.swing.JFrame'::setDefaultLookAndFeelDecorated(@(true)),
        jpl_list_to_array([a,b],Hdr),
        jpl_list_to_array([one,two],Row),
        jpl_list_to_array([Row],Arr),
        jeval(jframe(myframe,
                     [
                      vset(myframe=container),
                      jmenubar([
                                jmenu('File',
                                      [
                                       jmenuitem('Consult File',
                                                 [
                                                  callback(chooser_open_file(myframe))
                                                 ]),
                                       jmenuitem('Load GO',
                                                 [callback(callback_load(myframe,go))]),
                                       jmenuitem('Load CL',
                                                 [callback(callback_load(myframe,cell))]),
                                       jmenuitem('Consult Module')
                                      ]),
                                jmenu('View',
                                      [
                                       jmenuitem('Show classes',
                                                 [callback(show_classes(mainpanel))]),
                                       jmenuitem('Show properties',
                                                 [callback(show_properties(mainpanel))]),
                                       jmenuitem(item2b)
                                      ])
                               ]),
                      mainpanel=jpanel(
                             [
                              %vset(mainpanel=container),
                              %container::setBorder('javax.swing.BorderFactory'::createEmptyBorder(30,30,10,30)),
                              jlabel(yahello),
                              container::add(jscrollpane(jtable(Arr,Hdr))),
                              button=jbutton(pushmeyabas,
                                             [
                                                  callback(chooser_open_file(myframe))
                                              ])
                             ]),
                      container::pack,
                      container::setSize(800,container::getHeight),
                      container::setVisible(@(true))
                     ]),
              F),
        vget(button,Button,foo),
        writeln(b=Button),
        %FC := 'javax.swing.JFileChooser'::new,
        %FC::showOpenDialog(F),
        ExitListener := 'ExitListener'::new,
        F::addWindowListener(ExitListener).
        

t2:-
        'javax.swing.JFrame'::setDefaultLookAndFeelDecorated(@(true)),
        jpl_list_to_array([a,b],Hdr),
        jpl_list_to_array([one,two],Row),
        jpl_list_to_array([Row],Arr),
        jeval(jframe(myframe,
                     [
                      vset(myframe=container),
                      jmenubar([
                                jmenu('File',
                                      [
                                       jmenuitem('Consult File',
                                                 [
                                                  %callback(chooser_open_file(myframe))
                                                 ])
                                       ])
                               ]),
                      jpanel(%'java.awt.GridLayout'::new(1,3),
                             [
                              %container::setBorder('javax.swing.BorderFactory'::createEmptyBorder(30,30,10,30)),
                              jlabel(yahello),
                              container::add(jscrollpane(jtable(Arr,Hdr))),
                              button=jbutton(pushmeyabas,
                                             [
                                                  callback(chooser_open_file(myframe))
                                              ])
                             ]),
                      container::pack,
                      container::setSize(800,container::getHeight),
                      container::setVisible(@(true))
                     ]),
              F),
        vget(button,Button,foo),
        writeln(b=Button),
        %FC := 'javax.swing.JFileChooser'::new,
        %FC::showOpenDialog(F),
        ExitListener := 'ExitListener'::new,
        F::addWindowListener(ExitListener).
        

t6:- jpl_jbutton_demo.

        
jpl_jbutton_demo :-
        'javax.swing.JFrame'::setDefaultLookAndFeelDecorated(@(true)),
        jeval(jframe(menuframe,
                     [
                      jmenubar([
                                jmenu(menu1,
                                      [
                                       jmenuitem(item1a),
                                       jmenuitem(item1b)
                                      ]),
                                jmenu(menu2,
                                      [
                                       jmenuitem(item2a),
                                       jmenuitem(item2b)
                                      ])
                               ])
                     ]),
              F),
        %jframe(testbutton,F),
        %jmenubar(F,MenuBar),
        %jmenu(MenuBar,myMenu,Menu),
        %jmenuitem(Menu,item,_Item),
	CP := F::getContentPane,
        jpanel(CP,JPanel),
        jlabel(JPanel,hello,Label),
        vset(label,Label),
        vset(foonum,0),
        jlabel(JPanel,bye,_),
        jbutton(JPanel,mybutton,B),
        Border := 'javax.swing.BorderFactory'::createEmptyBorder(30,30,10,30),
        JPanel::setBorder(Border),
        F::pack,
        H := F::getHeight,
        F::setSize(150,H),
        F::setVisible(@(true)),
        ExitListener := 'ExitListener'::new,
        F::addWindowListener(ExitListener),
        register_listener(click_button(Label),_,Ear),
        B::addActionListener(Ear).
        %BListener := 'ButtonListener'::new,
        %BListener::setNum(1),
        %BListener::setObj('a'),
        %(   BListener::setTerm(['b'])
        %->  writeln(ok)
        %;   writeln('can not pass terms directly to java!')),
        %B::addActionListener(BListener).

t5:- jpl_table_demo.

jpl_table_demo :-
        jpl_list_to_array([a,b],Hdr),
        jpl_list_to_array([one,two],Row),
        jpl_list_to_array([Row],Arr),

        % DOESNT WORK:
        jeval(jframe(myframe,
                     [
                      jmenubar([
                                jmenu('File',
                                      [
                                       jmenuitem('Consult File')
                                      ])
                               ])
                     ]),
              F),
	%jpl_new( 'javax.swing.JFrame', ['current_prolog_flag'], F),
	jpl_call( F, getContentPane, [], CP),
	jpl_new( 'javax.swing.JPanel', [], Panel),
	jpl_call( CP, add, [Panel], _),
	jpl_new( 'javax.swing.JButton', [pressme], Button),
        jpl_new( 'ButtonListener',[], Handler),
        jpl_call( Handler, setNum, [-1], _),
        vset(action(-1),mychooser(F)),
        jpl_call( Button, addActionListener, [Handler], _),
        jpl_call( Panel, add, [Button], _),
	jpl_new( 'javax.swing.JTable', [Arr,Hdr], T),
	jpl_new( 'javax.swing.JScrollPane', [T], SP),
	jpl_call( Panel, add, [SP], _),
	jpl_new( 'javax.swing.JLabel', [hello], Label),
        jpl_call( Panel, add, [Label], _),
	jpl_call( F, setSize, [600,400], _),
	jpl_call( F, setVisible, [@(true)], _).

mychooser(F):-
        writeln(frame=F),
        jpl_new('javax.swing.JFileChooser',[],FC),
        jpl_call(FC,showOpenDialog,[F],RV),
        writeln(ret=RV).

