% tail +2 1471-2164-7-187-s1.txt | perl -npe 's/\r//' | tbl2p -p terminf > goinf.pro

:- use_module(bio(ontol_db)).
:- use_module(bio(tabling)).


incons(Child-SpecChild,Parent-SpecParent):-
        terminf(Child,_,_,SpecChild),parentT(Child,Parent),terminf(Parent,_,_,SpecParent),SpecParent > SpecChild.
:- table_pred(incons/2).

minimal_incons(Child-SpecChild/NChild,Parent-SpecParent/NParent):-
        incons(Child-SpecChild,Parent-SpecParent),
        \+ ((   parentT(SubChild,Child),
                incons(SubChild-_,Parent-_))),
        \+ ((   parentT(Parent,SuperParent),
                incons(Child-_,SuperParent-_))),
        class(Child,NChild),
        class(Parent,NParent).


