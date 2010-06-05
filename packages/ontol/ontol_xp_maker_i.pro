:- module(ontol_xp_maker_i,
          [
           write_xps/0,
           write_cdefs/0
           ]).

:- use_module(ontol_db).
:- use_module(bio(curation_db)).
:- use_module(bio(tabling)).

xp_annotation(Genus,Rel,Target,I) :-
        inst_of(I,Genus),
        inst_rel_type(I,Rel,Target).

xp_annotations(Genus,Rel,Target,Genes) :-
        setof(Gene,xp_annotation(Genus,Rel,Target,Gene),Genes).
:- table_pred(xp_annotations/4).

make_xp(Genus,Rel,Target) :-
        xp_annotations(Genus,Rel,Target,Genes),
        \+ more_specific_match(Genus,Rel,Target,Genes).

more_specific_match(Genus,Rel,Target,Genes) :-
        subclass(Genus2,Genus), % assumes ontol_reasoner
        Genus2\=Genus,
        xp_annotations(Genus2,Rel,Target,Genes).

more_specific_match(Genus,Rel,Target,Genes) :-
        subclass(T2,Target),
        Target\=T2,
        xp_annotations(Genus,Rel,T2,Genes).

write_xps :-
        forall((make_xp(Genus,Rel,Target),xp_annotations(Genus,Rel,Target,L)),
               format('~w^~w(~w) => ~w~n',[Genus,Rel,Target,L])).

cdef_annotation(CDef,I) :-
        CDef=cdef(Genus,Diffs),
        inst_of(I,Genus),
        forall(member(R=T,Diffs),
               inst_rel_type(I,R,T)).

cdef_annotations(CDef,Genes) :-
        setof(Gene,cdef_annotation(CDef,Gene),Genes).
:- table_pred(cdef_annotations/2).


% differentia lists of arbitrary length - just add more clauses
init_diffs([]).
init_diffs([_]).
%init_diffs([_,_]).

% TODO: allow nesting.
% e.g. cdef(permeability,[inh=cdef(mem,[po,nuc])])
% doesnt require skolems with birn, as already instances
% need way of avoiding cycles.
% OR we can just run a few times
% still avoid cycles..
fill_diffs([]).
fill_diffs([R=T|L]) :-
        uniq(R^T,inst_rel_type(_,R,T)),
        fill_diffs(L).

init_cdef(cdef(Genus,Diffs)) :-
        uniq(Genus,inst_of(_,Genus)),
        init_diffs(Diffs),
        fill_diffs(Diffs).
        
make_cdef(CDef) :-
        var(CDef),
        !,
        setof(CDef,init_cdef(CDef),CDefs),
        member(CDef,CDefs),
        make_cdef(CDef).

make_cdef(CDef) :-
        debug(xp,'candidate: ~w',[CDef]),
        cdef_annotations(CDef,Genes),
        \+ more_specific_match(CDef,Genes).

more_specific_match(CDef,Genes) :-
        CDef=cdef(Genus,Diffs),
        subclass(Genus2,Genus),
        Genus2\=Genus,
        cdef_annotations(cdef(Genus2,Diffs),Genes),
        debug(xp,'  more specific genus: ~w',[Genus2]).

more_specific_match(CDef,Genes) :-
        CDef=cdef(Genus,Diffs),
        select(Rel=Target,Diffs,Diffs2),
        subclass(T2,Target),
        Target\=T2,
        cdef_annotations(cdef(Genus,[Rel=T2|Diffs2]),Genes),
        debug(xp,'  more specific target: ~w',[Rel=T2]).


% extend
more_specific_match(CDef,Genes) :-
        CDef=cdef(Genus,Diffs),
        cdef_annotations(cdef(Genus,[Rel=_T|Diffs]),Genes),
        \+ member(Rel=_,Diffs),
        debug(xp,'  more specific ext: ~w',[Rel=_T]).

write_cdefs :-
        forall((make_cdef(CDef),cdef_annotations(CDef,L)),
               (   write_cdef(CDef),
                   format(' => ~w~n',[L]))).

write_cdefs_birn :-
        forall((CDef=cdef(Genus,_),subclass(Genus,'PATO:0000001'),sub_atom(Genus,0,_,_,'PATO:'),uniq(CDef,init_cdef(CDef)),make_cdef(CDef),cdef_annotations(CDef,L)),
               (   write_cdef(CDef),
                   format(' => ~w~n',[L]))).

write_cdef(cdef(Genus,Diffs)) :-
        write(Genus),
        forall(member(R=T,Diffs),
               (   format('^~w(~w)',[R,T]))).
        

uniq(Temp,Goal) :-
        setof(Temp,Goal^Goal,Temps),
        member(Temp,Temps).
        

/*
     /\
    /\/\
    1234
  /A
 /\B
 \/C  
  \D  

  gene1 = CD*34
  
  C*3 : no members
  CD*1234 : exists a more specific class with same set
  {CD*34} : most specific class with that set
  
  */


        
