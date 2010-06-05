:- module(pkb_writer_phenotetab,
          []).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(curation_db)).


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
           'Frequency',
           'Abnormal ID','Abnormal Name',
           'Description',
           'Orthologs',
           'Pub',
           'Date Created',
           ''
           ).



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
        organism(F),
        (   setof(G,organism_gene(F,G),[OmimGene])
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

/*
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
        
