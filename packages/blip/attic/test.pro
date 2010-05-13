
:- module(test,
          [
           foo/2
          ]).




:- transitive foo/2.
foo(X,Y):- write(X,Y).
 
/** <module>
  @pred foo(X,Y)
  @mode foo(+,?)
  where blah
  */