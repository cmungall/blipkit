/* -*- Mode: Prolog -*- */



:- module(ontol_xmlmap_entrezgene,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(entrezgene,XML,PL):-
        apply_xmlpred(ontol_xmlmap_entrezgene,XML,PL).
io:preds_to_xml(entrezgene,PL,XML):-
        reverse_xmlpred(ontol_xmlmap_entrezgene,XML,PL).

xmlpred('Entrezgene-Set',_,[],
        [let(ID=['Entrezgene','Entrezgene_track-info','Gene-track','Gene-track_geneid']),
         prolog(atom_concat('entrezgene:',ID,GeneID)),
         translate(['Entrezgene','Entrezgene_gene'],in(GeneID))]).

xmlpred('Entrezgene_gene',in(ID),[class(ID,N),belongs(ID,'NCBIGene'),class_comment(ID,Comment)],
        [let(N=['Gene-ref','Gene-ref_locus']),
         let(Comment=['Gene-ref','Gene-ref_desc']),
         translate(['Gene-ref','Gene-ref_syn','Gene-ref_syn_E'],in(ID))]).
xmlpred('Gene-ref_syn_E',in(ID),[synonym(ID,'',Syn)],
        let(Syn='.')).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(tp53_class,
            [],
            (   ensure_loaded(bio(ontol_db)),
                load_biofile(entrezgene,'tp53-entrez.xml'),
                setof(N,ID^class(ID,N),Ns)),
            (   Ns=['TP53']))).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/10/24 01:42:24 $
  @license LGPL

  ---+ Name
%  ontol_xmlmap_entrezgene

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_xmlmap_entrezgene)).

  demo:-
    load_biofile(entrezgene,'tp53-entrez.xml'),
    setof(ID-Name,class(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  treats genes as types (universals), not instances
  
  */