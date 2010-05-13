/* -*- Mode: Prolog -*- */



:- module(omim_xmlmap_omimxml,[]).

:- use_module(bio(xml_transform)).


io:xml_to_preds(omimxml,XML,PL):-
        apply_xmlpred(omim_xmlmap_omimxml,XML,PL).
io:preds_to_xml(omimxml,PL,XML):-
        reverse_xmlpred(omim_xmlmap_omimxml,XML,PL).

tagmap('OMIM-tx').
tagmap('OMIM-rf').
tagmap('OMIM-cs').
tagmap('OMIM-syn').
tagmap('OMIM-cn','OMIM-cn-value').
tagmap('OMIM-cd','OMIM-cd-value').
tagmap('OMIM-ed','OMIM-ed-value').

xmlpred(omim_disease_set,_,[],
        [translate('OMIM-disease')]).
xmlpred('OMIM-disease',_,omim(ID,N),
        [
         let(ID='OMIM-no'),
         let(N='OMIM-ti'),
         prolog(M=in(ID)),
         translate('OMIM-tx',M),
         translate('OMIM-syn',M),
         translate('OMIM-rf',M),
         translate('OMIM-cn',M),
         translate('OMIM-cd',M),
         translate('OMIM-ed',M),
         translate('OMIM-phenotype',M),
         translate('OMIM-mutation',M)
        ]).
% generic mapping for simple tags
xmlpred(El,in(ID),omim_prop(ID,El,X),let(X='.')):- tagmap(El).
xmlpred(El,in(ID),omim_prop(ID,El,X),let(X=ValEl)):- tagmap(El,ValEl).

xmlpred('OMIM-phenotype',in(ID),omim_phenotype(ID,Class,Desc),
        [let(Class=phenotype_class),
         let(Desc=phenotype_desc)]).
xmlpred('OMIM-mutation',in(ID),omim_mutation(MutID,ID,Desc),
        [let(MutIDLocal='OMIM-mutation-no'),
         let(Desc='OMIM-mutation-desc'),
         prolog(concat_atom([ID,MutIDLocal],'-',MutID)),
         prolog(M=in(MutID)),
         translate('OMIM-mutation-text',M),
         translate('OMIM-mutation-fxn',M)]).

xmlpred('OMIM-mutation-fxn',in(MutID),omim_mutation_fxn(MutID,Gene,Codon,WildAA,MutAA),
        [let(Gene=mut_gene),
         let(Codon=codon_no),
         let(WildAA=aa_wild),
         let(MutAA=aa_mutant)]).

xmlpred('OMIM-mutation-text',in(MutID),omim_mutation_text(MutID,Text),
        [let(Text='.')]).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test)=
       load_biofile(omimxml,'test2-omim.xml')/[]).
        
unittest(test(basic,
            [_=load(test)],
            (   ensure_loaded(bio(omim_db)),
                setof(N,ID^omim(ID,N),Ns),
                writeln(names=Ns)),
            true)).

unittest(test(mutfxn,
            [_=load(test)],
            (   ensure_loaded(bio(omim_db)),
                forall(omim_mutation(Mut,Omim,Desc),
                       (   writeln(Omim-Desc),
                           forall(omim_mutation_fxn(Mut,Gene,Codon,AAW,AAM),
                                  writeln(mut(Mut,Gene,Codon,AAW,AAM)))))),
            true)).

unittest(test(phen,
            [_=load(test)],
            (   ensure_loaded(bio(omim_db)),
                forall(omim_phenotype(Omim,Class,Desc),
                       (   writeln(phe(Omim,Class,Desc))))),
            true)).

unittest(test(syn,
            [_=load(test)],
            (   ensure_loaded(bio(omim_db)),
                forall(omim_prop(Omim,'OMIM-syn',Text),
                       (   writeln(syn(Omim,Text))))),
            true)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/10/24 01:42:24 $
  @license LGPL

  ---+ Name
%  omim_xmlmap_omimxml

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(omim_db)).
  :- use_module(bio(omim_xmlmap_omimxml)).

  demo:-
    load_biofile(omimxml,'test-omim.xml'),
    setof(ID-Name,gene(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  
  */