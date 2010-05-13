/* -*- Mode: Prolog -*- */



:- module(gene_db,
          [
           gene_info/14,
           gene_info/13,        % intensional
           
           gene_taxon/2,
           gene_dbxref/2,
           gene_synonym/2,
           gene_chromosome/2,
           gene_symbol/2,
           gene_typename/2,
           gene_class/2,
           gene_description/2,
           gene_official_symbol/2,
           gene_fullname/2,

           gene_rif/5,
           gene_rifdesc_pub/3,
           gene_rifdesc/2
           ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(macros_optargs)).
:- datapreds(gene_db,
             [
              gene_info(
                        atom('tax_id' -
                            'the unique identifier provided by NCBI Taxonomy
                            for the species or strain/isolate'),
                       
                       pk('GeneID' -
                         'the unique identifier for a gene'),

                       atom('Symbol' -
                           'the default symbol for the gene
                           ASN1:  gene->locus'),
                       
                       atom('LocusTag' -
                           'the LocusTag value
                           ASN1:  gene->locus-tag'),

                       atom('Synonyms' -
                           'bar-delimited set of unofficial symbols for the gene'),

                       atom('dbXrefs' -
                           'bar-delimited set of identifiers in other databases
                           for this gene.  The unit of the set is database:value.'),

                       atom('chromosome' -
                           'the chromosome on which this gene is placed.
                           for mitochondrial genomes, the value "MT" is used.'),

                       atom('map location' -
                           'the map location for this gene'),

                       atom('description' -
                           'a descriptive name for this gene'),

                       atom('type' -
                           'the type assigned to the gene according to the list of options
                           provided in http://www.ncbi.nlm.nih.gov/IEB/ToolBox/CPP_DOC/lxr/source/src/objects/entrezgene/entrezgene.asn'),

                       atom('Symbol from nomenclature authority' -
                           'when not "-", indicates that this symbol is from a
                           a nomenclature authority'),

                       atom('Full name from nomenclature authority' -
                           'when not "-", indicates that this full name is from a
                           a nomenclature authority'),

                       atom('Nomenclature status' -
                           'when not "-", indicates the status of the name from the 
                           nomenclature authority (O for official, I for interim)'),

                       atom('x' -
                           '')),
                     
             gene_rif(
                      atom('tax_id' -
                          'the unique identifier provided by NCBI Taxonomy
                          for the species or strain/isolate'),
                                          pk('GeneID' -
                       'the unique identifier for a gene'),
                     
                     atom('PMID' -
                         'pubmed ID'),
                     atom('LastUpdate' -
                         ''),
                     atom('RIF'))
             ]).
:- optargs gene_info/14.

% todo: use macros to automatically write these

gene_dbxrefs(Gene,DbXrefs):-
        gene_info(_,Gene,_,_,_,DbXrefs).
gene_dbxref(Gene,DbXref):-
        gene_dbxrefs(Gene,DbXrefsAtom),
        concat_atom(DbXrefsList,'|',DbXrefsAtom),
        member(DbXref,DbXrefsList),
        DbXref\='-'.

gene_synonyms(Gene,Synonyms):-
        gene_info(_,Gene,_,_,Synonyms).
gene_synonym(Gene,Synonym):-
        gene_synonyms(Gene,SynonymsAtom),
        concat_atom(SynonymsList,'|',SynonymsAtom),
        member(Synonym,SynonymsList),
        Synonym\='-'.

gene_taxon(Gene,Tax):-
        gene_info(Tax,Gene,_,_,_).
gene_symbol(Gene,Sym):-
        gene_info(_,Gene,Sym,_,_),
        Sym\='-'.
gene_chromosome(Gene,C):-
        gene_info(_,Gene,_,_,_,_,C),
        C\='-'.
gene_description(Gene,D):-
        gene_info(_,Gene,_,_,_,_,_,_,D),
        D\='-'.
gene_typename(Gene,T):-
        gene_info(_,Gene,_,_,_,_,_,_,_,T,_,_,_),
        T\='-'.
gene_class(Gene,T):- gene_typename(Gene,TN),class(T,TN). % requires SO
gene_official_symbol(Gene,S):-
        gene_info(_,Gene,_,_,_,_,_,_,_,_,S,_,_),
        S\='-'.
gene_fullname(Gene,N):-
        gene_info(_,Gene,_,_,_,_,_,_,_,_,_,N,_),
        N\='-'.

gene_rifdesc_pub(G,D,P) :- gene_rif(_,G,P,_,D).
gene_rifdesc(G,D)       :- gene_rifdesc_pub(G,D,_).

% SO mappings
type_id(pseudo,'SO:0000336'). % pseudogene
type_id(miscRNA,'SO:0000356').  %  RNA
type_id('protein-coding','SO:0000104'). %  polypeptide
type_id(rRNA,'SO:0000252'). % tRNA
type_id(tRNA,'SO:0000253'). % tRNA
type_id(other,'SO:0000110').    % located_sequence_feature 0'

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(gene,
             [],
             (   ensure_loaded(bio(gene_db)),
                 load_biofile('BRCA-related.gene_info'),
                 forall((gene_symbol(G,S),gene_taxon(G,T)),
                        format('Gene: ~w "~w" Tax: ~w~n',[G,S,T]))),
            true)).

unittest(test(gene2,
             [],
             (   ensure_loaded(bio(gene_db)),
                 load_biofile('BRCA-related.gene_info'),
                 gene_symbol(G,'BRE'),
                 gene_taxon(G,9606),
                 gene_synonym(G,'BRCC4')),
            true)).
/** <module> represents genes; based on ncbi/entrez gene

  ---+ Synopsis

  ==
  :- use_module(bio(gene_db)).

  ==

  ---+ Description

  ftp://ftp.ncbi.nih.gov/gene

  ---++ Examples

  join of gene_rif and obo version of genes
==
blip-findall -u gene_db -r generif -r gene/9606 "gene_rif(_,G,_,_,Rif),entity_label(G,N)" -select "x(G,N,Rif)"
==  

  
**/
