/* -*- Mode: Prolog -*- */



:- module(gene_xmlmap_entrezgene,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(entrezgene,XML,PL):-
        apply_xmlpred(gene_xmlmap_entrezgene,XML,PL).
io:preds_to_xml(entrezgene,PL,XML):-
        reverse_xmlpred(gene_xmlmap_entrezgene,XML,PL).

xmlpred('Entrezgene-Set',_,[],
        [let(ID=['Entrezgene','Entrezgene_track-info','Gene-track','Gene-track_geneid']),
         prolog(atom_concat('entrezgene:',ID,GeneID)),
         translate(['Entrezgene','Entrezgene_gene'],in(GeneID))]).

xmlpred('Entrezgene_gene',in(ID),[gene_symbol(ID,N),gene_description(ID,Desc)],
        [let(N=['Gene-ref','Gene-ref_locus']),
         let(Desc=['Gene-ref','Gene-ref_desc']),
         translate(['Gene-ref','Gene-ref_syn','Gene-ref_syn_E'],in(ID))]).
xmlpred('Gene-ref_syn_E',in(ID),[gene_synonym(ID,Syn)],
        let(Syn='.')).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(tp53,
            [],
            (   ensure_loaded(bio(gene_db)),
                load_biofile(entrezgene,'tp53-entrez.xml'),
                setof(N,ID^gene_symbol(ID,N),Ns)),
            (   Ns=['Rab1']))).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/10/24 01:42:24 $
  @license LGPL

  ---+ Name
%  gene_xmlmap_entrezgene

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(gene_db)).

  demo:-
    load_biofile(entrezgene,'tp53-entrez.xml'),
    setof(ID-Name,class(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  treats genes as types (universals), not instances
  
  */