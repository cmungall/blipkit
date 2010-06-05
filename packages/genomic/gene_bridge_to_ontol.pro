:- module(gene_bridge_to_ontol,[]).

:- use_module(bio(gene_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).

% genes are instances

localid_globalid(Local,Global):- atom_concat('entrezgene:',Local,Global).
chromosome_globalid(Local,Global,Tax):- concat_atom(['TempChromosomeDB',Tax,Local],':',Global). 

ontol_db:inst(Gene):- gene_symbol(GeneLoc,_),localid_globalid(GeneLoc,Gene).
metadata_db:entity_label(Gene,Symbol):- gene_symbol(GeneLoc,Symbol),localid_globalid(GeneLoc,Gene).
metadata_db:entity_synonym(Gene,Syn):- gene_synonym(GeneLoc,Syn),localid_globalid(GeneLoc,Gene).
ontol_db:inst_of(Gene,'SO:0000704'):- gene_symbol(GeneLoc,_),localid_globalid(GeneLoc,Gene).
metadata_db:entity_resource(Gene,'NCBIGene'):- gene_symbol(GeneLoc,_),localid_globalid(GeneLoc,Gene).
metadata_db:entity_xref(Gene,Xref):- gene_dbxref(GeneLoc,Xref),localid_globalid(GeneLoc,Gene).
metadata_db:entity_comment(Gene,X):- gene_description(GeneLoc,X),localid_globalid(GeneLoc,Gene).
ontol_db:inst_rel(Gene,'OBO_REL:located_in',Chrom):- gene_chromosome(GeneLoc,ChromLoc),localid_globalid(GeneLoc,Gene),gene_taxon(GeneLoc,TaxId),chromosome_globalid(ChromLoc,Chrom,TaxId).
ontol_db:inst_rel(Gene,part_of_organism,Sp):- gene_taxon(GeneLoc,TaxId),atom_concat('NCBITaxon:',TaxId,Sp),localid_globalid(GeneLoc,Gene).

unique_pmid(PMID):-
        solutions(P,gene_rifdesc_pub(_,_,P),Ps),
        member(PMID,Ps).

pmid_full(PMID,PMID_With_Prefix):-
        atom_concat('PMID:',PMID,PMID_With_Prefix).
% RIFs
ontol_db:inst_of(P,'obd:Publication'):- unique_pmid(PMID),pmid_full(PMID,P).
ontol_db:inst_of(DescInst,'obd:Description'):- gene_rifdesc_pub(G,D,P),term_gensym(rif(G,D,P),'entrezgene:',DescInst).
ontol_db:inst(DescInst):- gene_rifdesc_pub(G,D,P),term_gensym(rif(G,D,P),'entrezgene:',DescInst).
metadata_db:entity_label(DescInst,D):- gene_rifdesc_pub(G,D,P),term_gensym(rif(G,D,P),'entrezgene:',DescInst).
ontol_db:inst_rel(DescInst,has_source,P2):- gene_rifdesc_pub(G,D,P),term_gensym(rif(G,D,P),'entrezgene:',DescInst),pmid_full(P,P2).
ontol_db:inst_rel(DescInst,describes,G):- gene_rifdesc_pub(G,D,P),term_gensym(rif(G,D,P),'entrezgene:',DescInst). % todo: anon inst?

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(gene_to_obo,
             [],
             (   ensure_loaded(bio(gene_db)),
                 ensure_loaded(bio(gene_bridge_to_class)),
                 load_biofile('BRCA-related.gene_info'),
                 write_biofile(obo,_)),
            true)).
