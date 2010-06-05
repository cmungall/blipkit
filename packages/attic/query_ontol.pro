
/* ******************************************************

  query_ontol

   deprecated - use dbmeta instead
   
****************************************************** */

:- module(query_ontol,
          [
           lclass_query/2,
           class_query/2
          ]).

:- use_module(bio(ontol_db)).

% -----------------------------------
% QUERYING
% -----------------------------------
lclass_query(Q,IDL):-
        (setof(ID,class_query(Q,ID),IDL)
        ->  true
        ;   IDL=[]).
class_query(search(S),_):-
        var(S),
        !,
        throw(search_term_must_be_instantiated).
class_query(search(''),ID):-
        class(ID,_).
class_query(search(ID),ID):-
        ID \= '',
        class(ID,_).
class_query(search(S),ID):-
        S \= '',
        class(ID,S).
class_query(search(S),ID):-
        S \= '',
        downcase_atom(S,Slc),
        class_query(search_lc(Slc),ID).
class_query(search_lc(Slc),ID):-
        class(ID,N),
        downcase_atom(N,Nlc),
        sub_atom(Nlc,_,_,_,Slc).
class_query(search_lc(Slc),ID):-
        def(ID,N),
        downcase_atom(N,Nlc),
        sub_atom(Nlc,_,_,_,Slc).
% use wildcard_match(Slc,N) ?


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

foo:class_query(ID,SubAtom):-
        class(ID,N),
        sub_atom(N,_,_,_,SubAtom).

unittest(load(all)=
      (   load_bioresource(go),
          load_bioresource(cell),
          load_bioresource(fly_anatomy),
          load_bioresource(mouse_anatomy))
      /[]).

unittest(test(query_generic,
            [_=load(all)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                writeln(generic),
                setof(ID,
                      N^pred_query(ontol_db:class(ID,N),N,contains(trans)),
                      IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(setof,
            [_=load(all)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                pred_query_bagof(ontol_db:class(ID,N),
                                 N,
                                 contains(trans),
                                 ID,
                                 IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(i,
            [_=load(all)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                setof(ID,
                      N^pred_query(ontol_db:class(ID,N),N,contains(trans,i)),
                      IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(ibag,
            [_=load(all)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(dbmeta)),
                pred_query_bagof(ontol_db:class(_,N),N,contains(trans,i),
                                 N,IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

unittest(test(query_nongeneric,
            [_=load(all)],
            (   ensure_loaded(bio(ontol_db)),
                writeln(nongeneric),
                setof(ID,
                      foo:class_query(ID,trans),
                      IDs),
                length(IDs,NumIDs),
                writeln(found(NumIDs))),
            NumIDs>1)).

