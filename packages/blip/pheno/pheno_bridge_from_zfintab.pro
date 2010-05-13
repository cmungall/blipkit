/* -*- Mode: Prolog -*- */


:- module(pheno_bridge_from_zfintab,
          []).

:- use_module(bio(curation_db)).
%:- use_module(bio(curation_bridge_from_zfintab)). HANDLED BY OBO WRITER
:- use_module(bio(pheno_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).

% phenorow(G,GN,Start,StartN,End,EndN,E,EN,Q,QN,EDiff,EDiffN,Tag,Pub).
phenorow(G,GN,Start,StartN,End,EndN,E,EN,Q,QN,EDiff,EDiffN,Tag,Pub):-
        phenorow(G,GN,Start,StartN,End,EndN,E,EN,Q,QN,EDiff,EDiffN,Tag,Pub,_).

:- dynamic
        is_caching_complete/0,
        cached_genotype/2,
        cached_phenotype/1,
        cached_pm/5.

assertall:-
        \+ is_caching_complete,
        !,
        assert(is_caching_complete),
        debug(zfin,'caching genotypes',[]),
        solutions(G-N,phenorow(G,N,_,_,_,_,_,_,_,_,_,_,_,_,_),GNs),
        forall(member(G-N,GNs),
               assert(cached_genotype(G,N))),
        debug(zfin,'caching pms',[]),
        solutions(cached_pm(M,G,P,Pub,PStruct),g_p_pub(M,G,P,Pub,PStruct),PMs),
        forall(member(PM,PMs),
               assert(PM)),
        debug(zfin,'caching ps',[]),
        solutions(P,member(cached_pm(_,_,P,_,_),PMs),Ps),
        forall(member(P,Ps),
               assert(cached_phenotype(P))),
        debug(zfin,'done caching',[]).

assertall.

g_p_pub(M,G,P,Pub,PStruct):-
        phenorow(G,_,Start,_StartN,End,_EndN,E,EN,Q,QN,EDiff,EDiffN,Tag,Pub),
        PElts=[Start,End,E,Q,EDiff,Tag],
        concat_atom(PElts,'--',P0),
        atom_concat('OBD:ZFIN-pheno-',P0,P),
        concat_atom([P,G,Pub],'--',M),
        fixid(Start,StartID),
        fixid(End,EndID),
        fixid(E,EN,EID),
        debug(zfin,'fix ~w',[E-EN-EID]),
        fixid(Q,QN,QID),
        fixid(EDiff,EDiffN,EDiffID),
        PStruct=p(StartID,EndID,EID,QID,EDiffID,Tag).
g_p_pub(M,G,P,Pub):-
        g_p_pub(M,G,P,Pub,_).

pheno_db:genotype(G):-
        assertall,
        cached_genotype(G,_).
metadata_db:entity_label(G,N):-
        assertall,
        cached_genotype(G,N).
pheno_db:genotype_organism(G,'NCBITaxon:7955'):-
        genotype(G).

pheno_db:phenotype(P):-
        assertall,
        cached_phenotype(P).
pheno_db:phenotype_manifestation(M,P,G):-
        assertall,
        cached_pm(M,G,P,_,_).
metadata_db:entity_source(M,Pub):-
        assertall,
        cached_pm(M,_,_,Pub,_).
pheno_db:phenotype_manifestation_provider(M,zfin):-
        assertall,
        cached_pm(M,_,_,_,_).
pheno_db:phenotype_phenochar(P,P/char):-
        phenotype(P).
pheno_db:phenochar_bearer_class(P/char,BC):-
        assertall,
        unique(E-EDiff,cached_pm(_,_,P,_,p(_,_,E,_,EDiff,_))),
        (   EDiff=''
        ->  BC=E
        ;   BC=class(E,[part_of=EDiff])).
pheno_db:phenochar_quality(P/char,P/quality):-
        phenotype(P).
pheno_db:quality_class(P/quality,QC):-
        assertall,
        unique(P-QC,cached_pm(_,_,P,_,p(_,_,_,QC,_,_))).
pheno_db:quality_qualifier(P/quality,during,Start):-  % Start=End - we say during
        assertall,
        unique(P-Start,cached_pm(_,_,P,_,p(Start,Start,_,_,_,_))).
pheno_db:quality_qualifier(P/quality,start,Start):-
        assertall,
        unique(P-Start-End,cached_pm(_,_,P,_,p(Start,End,_,_,_,_))),
        End\=Start.
pheno_db:quality_qualifier(P/quality,end,End):-
        assertall,
        unique(P-Start-End,cached_pm(_,_,P,_,p(Start,End,_,_,_,_))),
        End\=Start.


fixid(ZID,OBOID):- atom_concat('ZFIN:',ZID,X),class_xref(OBOID,X),!.
fixid(X,X).
fixid(ZID,_,OBOID):- atom_concat('ZFIN:',ZID,X),class_xref(OBOID,X),!.
fixid(_,N,ID):- class(ID,N),!.
fixid(_,N,ID):- entity_synonym(ID,N),!.
fixid(X,_,X).

unique(Template,Goal):-
        solutions(Template,Goal,L),
        member(Template,L).

        


        
        

        
