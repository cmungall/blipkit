:- module(ontol_xp_maker,
          []).

:- use_module(ontol_db).
:- use_module(bio(curation_db)).

xp_annotation(Genus,Rel,Target,Gene) :-
        subclass(C,Genus),
        curation_statement(_,Gene,_,C),
        restriction(C,Rel,Target).

xp_annotations(Genus,Rel,Target,Genes) :-
        setof(Gene,xp_annotation(Genus,Rel,Target,Gene),Genes).

make_xp(Genus,Rel,Target) :-
        xp_annotations(Genus,Rel,Target,Genes),
        \+ more_specific_match(Genus,Rel,Target,Genes).

more_specific_match(Genus,Rel,Target,Genes) :-
        subclass(Genus2,Genus),
        \+ Genus2=Genus,
        xp_annotations(Genus2,Rel,Target,Genes).

more_specific_match(Genus,Rel,Target,Genes) :-
        restriction(T2,Rel,Target),
        Target\=T2,
        xp_annotations(Genus,Rel,T2,Genes).


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


        
