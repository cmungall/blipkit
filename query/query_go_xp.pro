
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).

% eventually we want to use the reasoner to do this properly...

formation(A-AN,B-BN,Path):-
        differentium(AFormation,'OBO_REL:results_in_formation_of',A),
        differentium(BFormation,'OBO_REL:results_in_formation_of',B),
        AFormation\=BFormation,
        parentT(A,Path,B),
        \+subclassT(A,B),
        \+subclass(AFormation,BFormation),
        class(A,AN),
        class(B,BN).

formation_by_stage(A-AN,B-BN,Path):-
        differentium(AFormation,'OBO_REL:results_in_formation_of',A),
        differentium(BFormation,'OBO_REL:results_in_formation_of',B),
        AFormation\=BFormation,
        parentT(A,Path,B),
        \+subclassT(A,B),
        \+subclass(AFormation,BFormation),
        class(A,AN),
        class(B,BN).

% using stage will get a LOT of temporal links
%temporal_link(A,Rel,B):-
%        restriction(A,Rel,B).
%temporal_link(A,Rel,B):-
%        restriction(A,starts,B).


% do this one on pre-reasoned ontology
formation2(A-AN,B-BN,AFormation-AFN,BFormation-BFN,CRelInfo,PCRel,PRel):-
        differentium(AFormation,PCRel,A),
        PCRel\='OBO_REL:unfolds_in',
        restriction(A,CRel,B),
        link_info(restriction(A,CRel,B),CRel,CRelInfo),
        A\=B,
        differentium(BFormation,PCRel,B),
        AFormation\=BFormation,
        \+subclass(A,B),
        (   restriction(AFormation,PRel,BFormation)
        ->  true
        ;   PRel=none),
        class(A,AN),
        class(B,BN),
        class(AFormation,AFN),
        class(BFormation,BFN).

link_info(Link,R,Info):-
        entailed_by(Link,_),
        atom_concat(R,'*',Info),
        !.
link_info(_,R,R).




        
