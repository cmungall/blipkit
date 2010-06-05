/* -*- Mode: Prolog -*- */

:- module(blipfun,
          [
           call_func/2,
           func_goal/3,
           func_goal/4,
           op(1000,xfy,and),
           op(1100,xfy,where)
           ]).

:- op(1000,xfy,and).
:- op(1100,xfy,where).

call_func(T,X):-
        func_goal(T,G,X),
        G.

func_goal(F,G,X):-
        func_goal(F,G,_,X).
func_goal(F,Goal,In,Out):-
        F=..[PN|Args],
        functor_args_goal(PN,Args,Goal,In,Out).


% 0-ary => primitive
functor_args_goal(X,[],true,_,X):-
        number(X).
        
% 0-ary => binary
% eg label
% change of context, from subj to obj
functor_args_goal(PN,[],Goal,In,Out):-
        !,
        Goal=..[PN,In,Out].

% todo: =
functor_args_goal(text,[X],(true),_,X):- !. % returns value, ignores context
functor_args_goal(id,[X],(true),X,X):- !. % sets context


% count(X)
functor_args_goal(count,[XT],((setof(X1,XG^XG,X1s)->length(X1s,Len) ; Len=0)),In,Len):-
        !,
        func_goal(XT,XG,In,X1).

% where 
functor_args_goal(where,[T,X],(Goal,XG),In,XO):-  % query opt: constr must be first coz of aggregates..
        !,
        func_goal(X,XG,In,XO),
        Goal=..[T,XO].

% unary func -> binary goal
% change of scope??
xxxxfunctor_args_goal(PN,[X],(Goal,XG),In,XO):-
        unary(PN),
        !,
        func_goal(X,XG,In,XO),
        Goal=..[PN,XO].

% ex: subclassOf(...)
functor_args_goal(PN,[XT],(XG,Goal),Ob,Su):-
        !,
        Goal=..[PN,Su,Ob],      % ex: subclass(Su,Ob)
        func_goal(XT,XG,Ob,_).   % constraints on Ob

% ex: name
%functor_args_goal(PN,[X],(XG,Goal),In,X0):-
%        Goal=..[PN,In,Out],
%        func_goal(X,XG,Out,X0).

% binary func
% AND
functor_args_goal(and,[X,Y],(XG,YG),In,In):-
        !,
        func_goal(X,XG,In,_),
        func_goal(Y,YG,In,_).

% 'access' functor
% changes scope
functor_args_goal(*,[X,Y],(XG,YG),In,Out):-   % depr? use /
        !,
        func_goal(X,XG,In,Z),
        func_goal(Y,YG,Z,Out).
functor_args_goal(/,[X,Y],(XG,YG),In,Out):-
        !,
        func_goal(X,XG,In,Z),
        func_goal(Y,YG,Z,Out).
        
% ex: =
% context is preserved
functor_args_goal(Op,[X,Y],(XG,YG,G),In,In):-
        comparison_op(Op),
        !,
        func_goal(X,XG,In,XO),
        func_goal(Y,YG,In,YO),
        G=..[Op,XO,YO].
%functor_args_goal((>),[X,Y],(XG,YG,XO>YO),In,In):-
%        !,
%        func_goal(X,XG,In,XO),
%        func_goal(Y,YG,In,YO).

% ex: restriction(X,Y)
functor_args_goal(PN,[X,Y],(XG,YG,Goal),In,In):-
        func_goal(X,XG,_,XO),
        func_goal(Y,YG,_,YO),
        Goal=..[PN,In,XO,YO].

comparison_op(=).
comparison_op(>).
comparison_op(>=).
comparison_op(<).
comparison_op(=<).
comparison_op(\=).

unary(property).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest:testq(Q,R,Len):-
        ensure_loaded(bio(ontol_db)),
        ensure_loaded(bio(metadata_db)),
        func_goal(Q,G,X),
        format('Q: ~w :: ~w~n',[Q,G]),
        findall(X,G,Xs),
        maplist(writeln,Xs),
        length(Xs,Len),
        member(R,Xs),
        !.

unittest:testq2(Q,R,Len):-
        ensure_loaded(bio(nodeq)),
        func_goal(Q,G,X),
        format('Q: ~w :: ~w~n',[Q,G]),
        findall(X,nodeq:G,Xs),
        maplist(writeln,Xs),
        nl,
        length(Xs,Len),
        (   X=[]
        ;   member(R,Xs)),
        !.

unittest(test(goq,
            [],
            (   
                ensure_loaded(bio(blipfun)),
                load_bioresource(go),
                %testq( subclass(entity_label=text('cellular process')) ,_,_),
                %testq( subclass(entity_label=text('cellular process')) * entity_label,_,_),
                testq( count(subclass(entity_label=text('cellular process'))) ,_,_),
                %testq( parent(entity_label=text(apoptosis))*entity_label ,_,_),
                nl
            ),
            true)).

unittest(test(goq2,
            [],
            (   
                ensure_loaded(bio(blipfun)),
                load_bioresource(go),
                testq2( superclassOf(label=text(apoptosis))*label ,_,_),
                testq2( subclassOf(label=text(apoptosis))*label ,_,_),
                testq2( transitiveSuperclassOf(label=text(apoptosis))*label ,_,_),
                testq2( transitiveSuperclassOf(label=text('cellular process'))*label ,_,_),
                testq2( class(label=text(apoptosis)) ,_,_),
                testq2( class(label=text(apoptosis)) * subclassOf ,_,_),
                testq2( class(label=text(apoptosis)) * subclassOf * label ,_,_),
                testq2( count(transitiveSuperclassOf(label=text(apoptosis))) ,_,_),
                testq2( count(transitiveSuperclassOf(label=text('cellular process'))) ,_,_),
                %testq2( count(transitiveSuperclassOf(isClass)) ,_,_),
                testq2( class(count(subclassOf) > text(4)) ,_,_),
                testq2( linkTo(label=text(mitosis)),_,_),
                testq2( linkTo(label=text(mitosis)) * relation,_,_),
                testq2( linkTo(label=text(mitosis)) * node * label,_,_),
                testq2( linkTo(label=text(mitosis)) * relation(label=part_of ),_,_),
                writeln(done),
                nl
            ),
            true)).

unittest(test(goq3,
            [],
            (   
                ensure_loaded(bio(blipfun)),
                load_bioresource(go),
                testq2(
                       class where label=text(apoptosis)
                      ,_,_),
                testq2(
                       (class where label=text(apoptosis)) / label
                      ,_,_),
%                testq2(
%                       class where superclassOf(label=text(apoptosis))
%                      ,_,_),
%                testq2(
%                       (   class where superclassOf(label=text(apoptosis)) ) / label
%                      ,_,_),
                testq2( subclassOf(label=text(apoptosis))*label ,_,_),
                testq2( transitiveSuperclassOf(label=text(apoptosis))*label ,_,_),
                testq2( transitiveSuperclassOf(label=text('cellular process'))*label ,_,_),
                testq2( class where label=text(apoptosis) ,_,_),
                testq2( (class where label=text(apoptosis)) * subclassOf ,_,_),
                testq2( (class where label=text(apoptosis)) * subclassOf * label ,_,_),
                testq2( count(transitiveSuperclassOf(label=text(apoptosis))) ,_,_),
                testq2( count(transitiveSuperclassOf(label=text('cellular process'))) ,_,_),
                %testq2( count(transitiveSuperclassOf(isClass)) ,_,_),
                testq2( class where count(subclassOf) > text(4) ,_,_),
                testq2( linkTo(label=text(mitosis)),_,_),
                testq2( linkTo(label=text(mitosis)) * relation,_,_),
                testq2( linkTo(label=text(mitosis)) * node * label,_,_),
                testq2( linkTo(label=text(mitosis)) * relation(label=part_of ),_,_),
%                testq2( linkViaTo(label='part_of',label=text(mitosis)) * relation(label=part_of ),_,_),
                writeln(done),
                nl
            ),
            true)).


/*

  TODO: search AND replace
  
  examples:
  
func_goal(class(entity_label=text(apoptosis) and subclassRT(entity_label=text(biological_process))) * def,G,_,X),G.

class[name='apoptosis' and subclassRT[name='bp']]/def

func_goal(class(entity_label=text(mitosis) and subclassRT(entity_label=text(biological_process))) * restriction(property(entity_label=text('part of')),belongs=text('biological_process')),G,_,X),G.

class[name='mitosis' and subclassRT[name='bp']]/restriction[...]

what to return....?

solution: stripe, returning terms for links

% prolog:
link(ID,link(R,To)):- restriction(ID,R,To).

  class(subclass(name=x) & link(to=ID))

  class[subclass@'x' & link[to='y']]
  class[subclass@'x' & link[relation='ro:part_of' & to='y']]

  also

  class[...]/link

  call_func(count(subclassRT(id('GO:0006915'))),X).
  count[subclassRT[@'x']]

  counts subclasses (anything that is a subclass of apoptosis)

  call_func(class(count(subclass) > text(4)),X).
  class[count[subclass] > 4]

  class[not(?is_anonymous) & suclassRT(x)]

  cycles:

  class[subclassT=.]

  isa diamond?

  'foo' => 'foo' OR
  'foo' => lambda(_) {'foo'}

  label => lambda(x y) {..}

  a  := b => lambda(?x) { a(x) = a(y) }   # ':=' not real equals = returns a lambda-eq function.

  label := 'foo' => lambda(?x) {...}   # ':=' not real equals = returns a lambda-eq function.
  
  class( label='foo' ) => lambda(?x) { 
  
*/
