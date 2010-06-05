/* -*- Mode: Prolog -*- */


:- module(pheno_bridge_to_ontol,
          []).

:- use_module(bio(curation_db)).
%:- use_module(bio(curation_bridge_to_ontol)). HANDLED BY OBO WRITER
:- use_module(bio(pheno_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

% can we constrain this mapping more?
% annotation? more specific relation?
% note that inference will produce spurious 'data'
% - eg return wild type part_of relations
% - may be better with explicit genotype+environment -> phenotype assocs

% G = gene classance (genotype/allele - simplification)
curation_db:curation(M):- phenotype_manifestation(M,_,_).
curation_db:curation_statement(A,G,'OBOL:influences',CCID):-
        phenotype_manifestation(A,P,G),
        phenotype_phenochar(P,PC),
        phenochar_quality(PC,Q),
        quality_composed_type(Q,CDef),
        cdef_id(CDef,CCID).
curation_db:curation(M):- phenotype_manifestation(M,_,_).

%ontol_db:class(G):-
%        genotype(G).
ontol_db:inst(G):- genotype(G).

%metadata_db:entity_label(G,N):- infinite loop!!!
%        genotype(G,N).
%ontol_db:subclass(G,'SO:0001027'):-  % use special leaf_of relation??
%        genotype(G,_).
ontol_db:inst_of(G,'SO:0001027'):-  % use special leaf_of relation??
        genotype(G,_).
ontol_db:inst_rel(G,'OBO_REL:has_part',F):-
        genotype_feature(G,F).
metadata_db:entity_resource(G,NS):-      % TODO?? use ID prefix?
        genotype(G,_),
        (   concat_atom([NS,_],':',G)
        ->  true
        ;   NS=genotype).

ontol_db:inst_rel(A,'oban:has_data_source',Pub):-
        phenotype_manifestation(A,_,_),
        phenotype_manifestation_provenance(A,Pub).
        
% composed class anonymous ID (move to other module??)
cdef_id(CDef,CCID):-
        term_gensym(CDef,'_:anon',CCID).
cdef_id_for_quality(CCID):-
        unique_quality_composed_type(CDef),
        cdef_id(CDef,CCID).
unique_quality_composed_type(CDef):-
        solutions(CDef1,quality_composed_type(_,CDef1),CDefs),
        member(CDef,CDefs).

% we make an anon class for a post-composed quality class
ontol_db:is_anonymous(CCID):-
        cdef_id_for_quality(CCID).
metadata_db:entity_resource(CCID,phenotype):-
        cdef_id_for_quality(CCID).
ontol_db:class(CCID):-
        cdef_id_for_quality(CCID).
%metadata_db:entity_label(CCID,N):- % do this somewhere else...?
%        unique_quality_composed_type(CDef),
%        cdef_id(CDef,CCID),
%        name_composed_type(CDef,N).
ontol_db:genus(CCID,Genus):-
        unique_quality_composed_type(CDef),
        CDef=Genus^_,
        cdef_id(CDef,CCID).
ontol_db:differentium(CCID,Rel,To):-
        unique_quality_composed_type(CDef),
        CDef=_^Diffs,
        member(Rel=To,Diffs),
        cdef_id(CDef,CCID).


/** <module>
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/01/04 11:49:05 $
  @license LGPL

  ---+ Name
  ---++ pheno_bridge_to_class
- view pheno_db as generic ontol classances

  ---+ Synopsis

  ==
  :- use_module(bio(pheno_bridge_to_class)).

  ==

  ---+ Description

  each phenotype is a particular organism, with various parts

  Q: genoypes - rel to allele..
  
**/
