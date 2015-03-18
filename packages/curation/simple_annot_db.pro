:- module(simple_annot_db,
          [
           annot/4,
           annot/5
          ]).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(curation_db)).

:- use_module(bio(dbmeta)).

:- extensional(annot/4).
annot(Gene,Term,Ref,EvType) :- 
        curation_statement(C,Gene,_,Term),
        curation_source(C,Ref),
        curation_evidence_code(C,EvType).

annot(Src,Gene,Term,Ref,EvType) :- 
        G=curation_statement(C,Gene,_,Term),
        G,
        curation_source(C,Ref),
        curation_evidence_code(C,EvType),
        fact_clausesource(curation_db:G,Src).

%src_annot(Src,Gene,Term,Ref,EvType) :-
%        load_biofile(go_assoc,Src),


% MOVED: see annot-compare
/*
compare_annotsets(Src1,Src2,Gene,Change) :-
        annot(Src1,Gene,Term,Ref,Ev),
        debug(compare,'Comparing: ~w',[annot(Src1,Gene,Term,Ref,Ev)]),
        match_annot_d(Src1,Src2,Gene,Term,Ref,Ev,Change).

match_annot_d(Src1,Src2,Gene,Term,Ref,Ev,Change) :-
        match_annot(Src1,Src2,Gene,Term,Ref,Ev,Change),
        !.
match_annot(_,Src2,Gene,Term,Ref,Ev,identical) :-
        annot(Src2,Gene,Term,Ref,Ev).
match_annot(Src1,Src2,Gene,Term,Ref,Ev,refined(Term2)) :-
        annot(Src2,Gene,Term2,Ref,Ev),
        parentT(Term2,Term),
        \+ annot(Src1,Gene,Term2,Ref,Ev).
match_annot(Src1,Src2,Gene,Term,_,_,replaced(Term2,Ref2,Ev2)) :-
        annot(Src2,Gene,Term2,Ref2,Ev2),
        parentRT(Term2,Term),
        \+ annot(Src1,Gene,Term2,Ref2,Ev2).
match_annot(_,_,_,_,_,_,gone).






        
*/
