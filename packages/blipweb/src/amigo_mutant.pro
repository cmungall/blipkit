/* -*- Mode: Prolog -*- */
/** @copyright
  
  Copyright (C) 2005 by Chris Mungall (cjm AT fruitfly DOT org)
  
  @/copyright

  amigo component for querying/displaying data of class 'mutant'

  two dataclasses: phenotype and mutant
  
  */

% ==================== CONFIG ====================

:- use_module(bio(pheno_db)).
:- use_module(bio(ortho_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(tabling)).

:- table_pred(ontol_db:subclassT/2).


amigo_component(mutant).

%data_class(mutant).
%data_class(orthomutant).
data_class(phenotype).
data_class_by_ont(_Ont,phenotype).
data_class_window_size(mutant,20).
data_class_window_size(phenotype,50).
data_class_window_size(orthomutant,50).

http_param_label(mutant,'mutant data').
http_param_label(phenotype,'phenotype data').
http_param_label(orthomutant,'orthologous mutants').

% ==================== MODEL ====================

% delegate
amigo_query(phenotype,Q,IDL):-
        lphenotype_query(Q,IDL).
amigo_query(mutant,Q,IDL):-
        lmutant_query(Q,IDL).
amigo_query(orthomutant,Q,IDL):-
        lorthomutant_query(Q,IDL).

lphenotype_query(Q,IDL):-
        xsetof(ID,phenotype_query(Q,ID),IDL).
phenotype_query(class(EPC),Q):-
        %parentRT(EC,EPC),
        phenochar_bearer_class(PCh,EC),  % iterate through insts first
        subclassRT(EC,EPC),     % search UP
        phenochar_quality(PCh,Q).
phenotype_query(class(APC),Q):-
        %parentRT(AC,APC),
        quality_class(Q,AC),
        subclassRT(AC,APC).  % phenotypes do not automatically inherit over part_of
phenotype_query(search(''),Q):-
        phenochar_quality(_,Q).
phenotype_query(search(Text),Q):-
        Text \= '',
        class_query(search(Text),EC),
        phenochar_bearer_class(E,EC),
        phenochar_quality(E,Q).

lmutant_query(Q,IDL):-
        xsetof(ID,mutant_query(Q,ID),IDL).
mutant_query(search(Text),ID):-
        genotype(ID,N),
        sub_atom(N,_,_,_,Text).
mutant_query(search(Text),ID):-
        genotype_feature(ID,N),
        sub_atom(N,_,_,_,Text).
mutant_query(search(Text),G):-
        Text \= '',
        class_query(search(Text),EC),
        phenochar_bearer_class(P,EC),
        genotype_phenochar(G,P).
mutant_query(class(EPC),G):-
        phenochar_bearer_class(P,EC),
        genotype_phenochar(G,P),
        parentRT(EC,EPC).

lorthomutant_query(Q,IDL):-
        xsetof(ID,orthomutant_query(Q,ID),IDL).
orthomutant_query(Q,ID):-
        mutant_query(Q,G),
        genotype_feature(G,F),
        (   ortholog(ID,F,F2)
        ;   ortholog(ID,F2,F)),
        feature(F2,_,_).

genotype_organism_name(G,ON):-
        genotype_organism(G,ON). % TODO

% filters - partitions data into sets that can be used by tabs
id_filter_set(mutant,ID,Set,Set,organism):-
        genotype_organism_name(ID,Set).
id_filter_set(orthomutant,ID,Set,Set,organism):-
        ortholog_feature(ID,F),
        genotype_feature(G,F),
        genotype_organism_name(G,Set).
id_filter_set(phenotype,Q,Set,Set,kind):-
        phenochar_quality(PCh,Q),
        phenochar_bearer_class(PCh,BC),
        belongs(BC,Set).

% ==================== VIEW ====================

% genotypes

href_id_as_label(mutant,ID) =>
 doc:'show mutant ID as label',
 if(genotype(ID,N),
    then: href_data_item(mutant,ID,noesc(N),0),
    else: html:i(href_data_item(mutant,ID,ID))).

search_result_headers(mutant,summary,
                      tr(th([]),
                         th('Mutant'),
                         th('Entity'),
                         th('Entity-qualifier'),
                         th('Phenotypic-attribute'))).
      
search_result_id_view(mutant,summary,ID) =>
 doc:'Summary View of a single ID',
 td(href_data_item(mutant,ID,noesc(N),0) where genotype(ID,N)),
                                %        td(noesc(N)),
 td(ul(li([F,FName where feature(F,FName,_)]) forall genotype_feature(ID,F))),
 td(OC) where genotype_organism(ID,OC),
 td(genotype_phenotype_table(ID)).

search_result_id_view(mutant,detail,ID) =>
 doc:'Detail View of a single ID',
 td(div(class=info,
        h2(['Genotype: ',noesc(N)]) where genotype(ID,N),
        ul(tagval('ID',ID),
           findall(tagval('Allele/Gene',F),
                   genotype_feature(ID,F)),
%           ufindall(tagval('Gene',Gene),
%                    genotype_feature(ID,F),feature_relationship(F,Gene)),
           tagval('Organism',[OC,' (',OC,')']) where genotype_organism(ID,OC))),
    div(class=info,
        genotype_phenotype_table(ID))).

% mutant pairs

search_result_headers(orthomutant,summary,
                      tr(th([]),
                         th('Gene1'),
                         th('Org1'),
                         th('Gene2'),
                         th('Org2'),
                         th('Mutants1'),
                         th('Mutants2'))).

search_result_id_view(orthomutant,summary,ID) =>
 doc:'Summary View of a single Orthology ID',
 call((ortholog(ID,F1,F2),
       feature(F1,N1,_),
       feature(F2,N2,_))),
 td(href_data_item(gene,F1,N1,0)),
 td(''),
 td(href_data_item(gene,F2,N2,0)),
 td(''),
 td(gene_phenotype_summary(F1)),
 td(gene_phenotype_summary(F2)).

search_result_id_view(orthomutant,detail,ID) =>
 doc:'Detail view of a pair of mutants',
 search_result_id_view(orthomutant,summary,ID).

% GENES

gene_phenotype_summary(F) =>
 doc:'ALL phenos for a gene',
 table(ufindall(tr(td([href_id_as_label(term,EC),
                       html:font(size='-2',
                                 href_id_as_label(term,AC))])),
                (   genotype_feature(G,F),
                    genotype_phenotype(G,P),
                    phenotype_annot(P,G,EC,AC,_)))).
                           
% PHENOTYPES

search_result_headers(phenotype,summary,
                      tr(th([]),th('ID'),th('Name'),th('Gene'),th('Organism'),th('Phenotype Parts'),th('Stages'))).

search_result_id_view(phenotype,summary,Q) =>
 call((   phenochar_quality(PCh,Q),
          genotype_phenochar(Gt,PCh),
          phenotype_phenochar(P,PCh),
          phenotype_manifestation(PM,P,_))),
 td(href_id_as_label(term,BC) where phenochar_bearer_class(PCh,BC)),
 td(href_id_as_label(term,QC) where quality_class(Q,QC)),
 td(href_id_as_label(mutant,Gt)),
 td([FN where feature(F,FN,_)] forall genotype_feature(Gt,F)),
 %td(href_id_as_label(xref,F-FN) where feature(F,FN,_) forall genotype_feature(Gt,F)),
 td(findall(href_id_as_label(term,QualC),quality_qualifier(Q,_,QualC))),
 td(font(size= -3, Source) where phenotype_manifestation_provider(PM,Source)),
 td(href_id_as_label(xref,Pub) where phenotype_manifestation_provenance(PM,Pub)).


search_result_id_view(phenotype,summary,X) => log(wibble),h1(test),h2(X).

search_result_id_view(phenotype,detail,Q) =>
 search_result_id_view(phenotype,summary,Q).
% TODO: detail

% -- END OF GENOTYPE RESULTS

/*
  phenotypes


  phenoset(relates_to(gut),
           qualifier([stage=day4],
                     size=[small],
                     struct=[degen],)
           qualifier([stage=day5],
                     struct=[degen,dysp]))
*/

genotype_phenotype_entity(G,P,E):-
        genotype_phenotype(G,P),
        phenotype_entity(P,E).

genotype_phenotype_table(ID) =>
 doc:'all phenotypes for an genotype',
 doc:comment('shows all phenotypes for a mutant in a table',
             'listed by entity class first (eg gut)',
             ' entity class may be qualified by entity attvals',
             ' (this defines the actual entity)',
             '  under that the phenotype attvals are listed'),
 html:table(findall(phenochar_rows(PC),
                    genotype_phenochar(ID,PC))).

phenochar_rows(PCh) =>
 doc:'phenos by genotype,ent-genus',
 tr(td(colspan=3,
       href_id_as_label(term,BC))) where phenochar_bearer_class(PCh,BC),
 findall(quality_rows(Q),
         phenochar_quality(PCh,Q)),
 doc:comment('for a given mutant and entity genus class combo',
             ' (eg dpp/gut), show all phenos').
 
quality_rows(Q) =>
 doc:'phenos by genotype,ent',
 call(quality_class(Q,QC)),
 findall(tr(td(['-']),
            td(colspan=2,qualifier_text(R,QualC))),
         quality_qualifier(Q,R,QualC)),
 findall(tr(td(['-']),
            td(['-']),
            td(href_id_as_label(term,QC))),
         (quality_class(Q,QC))),
 doc:comment('given a mutant entity combo',
             ' (eg dpp/this-gut-at-stage-4), show all phenos').

qualifier_text(R,QualC) =>
 doc:'tag-value qualifier',
 href_id_as_label(property,R),
 ':',
 href_id_as_label(term,QualC).
 
detail_page(mutant) =>
 doc:'details on one (or more?) mutant genotypes',
 getparam(id,ID),
 outer(['OBD: Genotype report for ',ID,' : ',N where genotype(ID,N)],
       div(class=main,
           basic_query_form,
           report_by_data_class(mutant))).


% ==================== CONTROLLER ====================

