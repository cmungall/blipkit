:- use_module(bio(ontol_sqlmap_go)).
:- use_module(bio(seqfeature_sqlmap_go)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(rdb_util)).

violation(F,Exceptions):-
        rdb_connect(Dbh,go),
        go2taxon(F,_,only_in_taxon,_,Tax,_), % from file
        db_violation(Dbh,F,Tax,Exceptions).

db_violation(Dbh,F,OnlyInTax,VSp):-
        rdb_solutions(Dbh,
                      Sp,
                      (   curation_statementT(_,G,_,F),
                          feature_organism(G,Sp)),
                      Sps1),
        maplist(fixtax,Sps1,Sps),
        member(VSp,Sps),
        \+ subclassRT(VSp,OnlyInTax).

%findall(Sp,
%                (   member(Sp,Sps),
%                    \+ subclassRT(Sp,OnlyInTax)),
%                Vs).

fixtax(N,T):-
        number(N),
        !,
        atom_concat('NCBITaxon:',N,T).
fixtax(T,T).

