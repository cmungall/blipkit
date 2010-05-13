
ontol_db:subclass(Omim,CDef):-
        substr(_,Fma,Omim,fma,_,FmaN,OmimN),
        %sub_atom(Fma,0,_,_,'FMA:'),
        substr(_,Pato,Omim,quality,_,PatoN,_),
        %sub_atom(Pato,0,_,_,'PATO:'),
        debug(omim,'~w = ~w ~w',[OmimN,PatoN,FmaN]),
        sformat(CDef,'~w^inheres_in(~w)',[Pato,Fma]).

ontol_db:subclass(Omim,CDef):-
        substr(_,Fma,Omim,fma,_,FmaN,OmimN),
        \+ substr(_,Pato,Omim,quality,_,_,_),
        debug(omim,'NO PATO: ~w = ~w',[OmimN,FmaN]),
        fail.

ontol_db:subclass(Omim,MP):-
        substr(_,MP,Omim,'MPheno.ontology',omim,MPN,OmimN),
        debug(omim,'~w < ~w',[OmimN,MPN]).
ontol_db:subclass(Omim,MP):-
        eq(_,MP,Omim,'MPheno.ontology',omim,MPN,OmimN),
        debug(omim,'~w < ~w',[OmimN,MPN]).



