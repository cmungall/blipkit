/* -*- Mode: Prolog -*- */

:- module(homol_xmlmap_homologene,[]).

:- use_module(bio(xml_transform)).
:- use_module(bio(homol_db)).

io:xml_to_preds(homologene,XML,PL):-
        apply_xmlpred(homol_xmlmap_homologene,XML,PL).
io:preds_to_xml(homologene,PL,XML):-
        reverse_xmlpred(homol_xmlmap_homologene,XML,PL).

expand_id(ID,Prefix,Full):- concat_atom(Prefix,ID,Full).

xmlpred('HG-Entry',_,[homologset(ID),homologset_type(ID,gene),homologset_taxid(ID,Tax)],
        [let(ID1='HG-Entry_hg-id'),
         prolog(atom_concat('homologene:',ID1,ID)),
         let(Tax1='HG-Entry_taxid'),
         prolog(atom_concat('NCBITaxon:',Tax1,Tax)),
         translate(['HG-Entry_genes'],in(ID)),
         translate(['HG-Entry_commentaries','HG-CommentarySet'],in(ID))
         ]).
xmlpred('HG-Entry_genes',in(ID),[],translate(['HG-Gene'],in(ID))).

xmlpred('HG-Gene',in(ID),[homologset_member(ID,Member,MemberTax)],
        [let(Member1=['HG-Gene_geneid']),
         prolog(atom_concat('entrezgene:',Member1,Member)),
         let(MemberTax1=['HG-Gene_taxid']),
         prolog(atom_concat('NCBITaxon:',MemberTax1,MemberTax))
        ]).

xmlpred('HG-CommentarySet',in(ID),[],
        [let(Title='HG-CommentarySet_title'),
         translate(['HG-CommentarySet_commentaries',
                    'HG-Commentary'],in(ID,Title))]).
xmlpred('HG-Commentary',in(ID,Title),[],
        [let(Tax1='HG-Commentary_taxid'),
         prolog(atom_concat('NCBITaxon:',Tax1,Tax)),
         translate(['HG-Commentary_link',
                    'HG-Link',
                    'HG-Link_hypertext'],
                   in(ID,Title,Tax))
        ]).
xmlpred('HG-Link_hypertext',in(ID,Title,Tax),[homologset_taxid_xref(ID,Tax,Xref,Title)],
        let(Xref='.')).

/** <module>  xml transformations for homol_db model
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/10/24 01:42:24 $
  @license LGPL

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(homol_db)).
  :- use_module(bio(homol_xmlmap_homologene)).

  demo:-
    load_biofile(homologene,'tp53-homolo.xml'),
    setof(ID-Name,class(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  treats genes as types (universals), not instances
  
  */