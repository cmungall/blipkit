
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(bioprolog_util)).

% HP
gene_mp(F,P) :-
        curation_statement(_,F,_,P).

mp_phenotype(MP,(E,Q,D,W)) :-
        genus(MP,Q),
        belongs(Q,quality),
        (   differentium(MP,'OBO_REL:inheres_in',E)
        ->  true
        ;   E=(-)),
        (   differentium(MP,'OBO_REL:inheres_in_part_of',W)
        ->  true
        ;   W=(-)),
        (   differentium(MP,'OBO_REL:towards',D)
        ->  true
        ;   D=(-)).

mp_subsumed_by_phenotype(MP,P) :-
        subclassRT(MP,MP1),
        mp_phenotype(MP1,P).

mp_nr_subsumed_by_phenotype(MP,P) :-
        subclassRT(MP,MP1),
        mp_phenotype(MP1,P),
        \+ ((subclassRT(MP,MP2),
             mp_phenotype(MP2,_),
             subclassT(MP2,MP1))).

feature_phenotype(F,P) :-
        gene_mp(F,MP),
        mp_nr_subsumed_by_phenotype(MP,P).

feature_phenotype(F,P,Root) :-
        gene_mp(F,MP),
        subclassRT(MP,Root),
        mp_nr_subsumed_by_phenotype(MP,P).

feature(X) :-
        setof(X,P^gene_mp(X,P),Xs),
        member(X,Xs).

mim2genesymbol(D,Sym) :-
        p2g(D,MG),
        entity_xref(G,MG),
        entity_label(G,Sym).

split_reverse_join(In,Del,Out) :-
        concat_atom(L,Del,In),
        reverse(L,L2),
        concat_atom(L2,Del,Out).

phenoterow('Disease ID','Disease Name',
           'Gene ID','Gene Name',
           'Genotype',
           'Gene Symbol(s)',
           'Phenotype ID',           'Phenotype Name',
           'Entity ID','Entity Name',
           'Quality ID', 'Quality Name', 
           'Add\'l Entity ID','Add \'l Entity Name',
           'Mode of inheritance ID',           'Mode of inheritance Name',
           'Age of Onset ID',           'Age of Onset Name',
           'Evidence',
           'Frequency',
           'Abnormal ID','Abnormal Name',
           'Description',
           'Orthologs',
           'Pub',
           'Date Created',
           ''
          ).

phenoterow(OmimID,Name,
           OmimGene,'',
           '',
           SymbolsAtom,
           HP,'',
           E,'',
           Q,'',
           E2,'',
           Mode,'',
           Age,'',
           Ev,
           Freq,
           Abn,'',
           Desc,
           Orthologs,
           Pub,
           DateDDMMYYYY,'') :-
        phenotype_annotation(_,OmimNum,Name,_,HP,_Ref,
                             Ev,_,Freq,_,_IO,_Syns,
                             DateYYYYMMDD,_Editor),
        atom_concat('MIM:',OmimNum,OmimID),
        (   setof(G,p2g(OmimID,G),[OmimGene])
        ->  true
        ;   OmimGene=''),
        solutions(G,mim2genesymbol(OmimID,G),Symbols),
        concat_atom(Symbols,', ',SymbolsAtom),
        (   mp_eqa(HP,E,Q,E2,Abn)
        ->  true
        ;   E='',
            Q='',
            E2='',
            Abn=''),
        split_reverse_join(DateYYYYMMDD,'.',DateDDMMYYYY),
        Mode='',
        Age='',
        Desc='',
        Orthologs='',
        Pub=''.

/*

phenoterow(F,'',
           OmimGene,'',
           '',
           SymbolsAtom,
           HP,'',
           E,'',
           Q,'',
           E2,'',
           Mode,'',
           Age,'',
           Freq,
           Abn,'',
           Desc,
           Orthologs,
           Pub,
           '','') :-
        feature(F),
        %\+ \+ score(_,_,_,_,F,_),
        (   setof(G,p2g(F,G),[OmimGene])
        ->  true
        ;   OmimGene=''),
        %gene_list(F,Genes),
        %setof(G,p2g(F,G),Genes),
        setof(G,mim2genesymbol(F,G),Symbols),
        concat_atom(Symbols,', ',SymbolsAtom),
        %Genes='',
        gene_mp(F,HP),
        (   mp_eqa(HP,E,Q,E2,Abn)
        ->  true
        ;   E='',
            Q='',
            E2='',
            Abn=''),
        Mode='',
        Age='',
        Desc='',
        Orthologs='',
        Pub=''.
  
phenoterow(F,'',
           '','',
           '',
           G,
           '','',
           '','',
           '','',
           '','',
           '','',
           '','',
           '',
           '','',
           '',
           Orthologs,
           '',
           '','') :-
        feature(F),
        p2g(F,G),
        Orthologs=''.
*/

mp_eqa(HP,E,Q,E2,Abn) :-
        genus(HP,Q),
        !,
        (   differentium(HP,'OBO_REL:inheres_in',E)
        ->  true
        ;   (   differentium(HP,'inheres_in_part_of',EP)
            ->  sformat(E,'FMA:62955^part_of(~w)',[EP])
            ;   E='')),
        (   differentium(HP,towards,E2)
        ->  true
        ;   E2=''),
        (   differentium(HP,qualifier,Abn)
        ->  true
        ;   Abn='').
        
