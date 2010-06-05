/* -*- Mode: Prolog -*- */



:- module(pheno_writer_general,[
                                ]).

:- use_module(bio(io)).
:- use_module(bio(pheno_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(xml_writer)).
:- use_module(bio(serval)).
:- use_module(bio(dbmeta)).
:- use_module(library(sgml)).

io:redirect_stdout(phenoxml).
io:write_all(phenoxml,_,_):-
        write_phenoxml.

write_phenoxml:-
        X=xml([]),
        xmldoc_start(X),
        write_sterm([],X,phenoset).

xxx:-
        xmldoc_start(X),
        xmlnode_start(X,X2,phenoset,[xmlns='http://www.bioontologies.org/obd/schema/pheno']),
        write_preds(X2,genotype(_,_)),
        write_preds(X2,genetic_feature(_,_,_)),
        write_preds(X2,phenotype_manifestation(_,_,_)),
        xmlnode_end(X2,X).

phenoset=>
  call(use_module(bio(pheno_db))),
  xml:phenoset(xmlns='http://www.bioontologies.org/obd/schema/pheno',
               genotype(G,N) forall genotype(G,N),
               genetic_feature(A,B,C) forall genetic_feature(A,B,C),
               phenotype_manifestation(M,P,G) forall phenotype_manifestation(M,P,G)).

genotype(G,N)=>
 call((genotype_organism(G,OC)->true ; OC='')),
 xml:genotype(id=G,
               organism=OC,
               xml:name(N),
               xml:genotype_feature_link(feature=F) forall genotype_feature(G,F)).

genetic_feature(F,N,T)=>
  xml:genetic_feature(id=F,
                      xml:name(N),
                      xtyperef(T)).

phenotype_manifestation(M,P,G)=>
  xml:phenotype_manifestation(  % id=M,
                              xml:description(Desc) forall phenotype_manifestation_desc(M,Desc),
                              xml:manifest_in(genotype=G, xtyperef(T) where phenotype_manifestation_type(M,T)),
                                %xml:annotation_provider(about=Source) forall dbmeta:fact_source(pheno_db:phenotype_manifestation(M,P,G),Source), % TODO: check this
                              xml:annotation_provider(about=Source) forall phenotype_manifestation_provider(M,Source), % TODO: check this
                              xml:provenance(about=Pub) forall phenotype_manifestation_provenance(M,Pub),
                              phenotype(P)
                               ).

phenotype(P)=>
  xml:phenotype( % id=P,
                phenotype_character(PC) forall phenotype_phenochar(P,PC)).

phenotype_character(PC)=>
  xml:phenotype_character( % id=PC,
                           desc(Desc) forall phenochar_desc(PC,Desc),
                           xml:bearer(xtyperef(BC) where phenochar_bearer_class(PC,BC)),
                           quality(Q) forall phenochar_quality(PC,Q)).

quality(Q)=>
  xml:quality(xtyperef(QC) where quality_class(Q,QC),
              xml:related_entity(xtyperef(E2)) forall quality_related_entity(Q,E2),
              quality_qualifier(Rel,To) forall quality_qualifier(Q,Rel,To)).

quality_qualifier(Rel,To)=>
  if(Rel=modifier,
     then:
    xml:modifier(xtyperef(To)),
     else:
    xml:temporal_qualifier(relation=Rel,
                           xml:time_range(xtyperef(To)))).

desc(D) => xml:description(D).

xtyperef(T) =>
  if(T=class(G,DL),
     then:
    xml:typeref(about=G,
                typeref_qualifier(Diff) forall member(Diff,DL)),
     else:
    if((ontol_db:is_anonymous(T),ontol_db:genus(T,G)),
       then:
      [
       xml:typeref(about=G),
       typeref_qualifier(R=To) forall ontol_db:differentium(T,R,To)
      ],
       else:
      xml:typeref(about=T))).

typeref_qualifier(R=To) =>
  xml:qualifier(relation=R,
                xml:holds_in_relation_to(xtyperef(To))).
        
io:write_all(phenotbl,F):-
        fh(F,H),
        forall(phenotype_annot(P,G,EC,AV,SC),
               write_termrow(H,phenotype_annot(P,G,EC,AV,SC))),
        close(H).


fh(F,user_output):-   var(F),!.
fh(F,H):- open(F,write,H).

        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 03:35:06 $
  @license LGPL


  ==
  :- use_module(bio(pheno_writer_general)).
  ==

  you should not need this module directly - handled by module io
  
  */
