/* -*- Mode: Prolog -*- */


:- module(parser_phenosyn,
          [
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(pheno_db)).
:- use_module(bio(dbmeta)). % for genotype lookup
:- use_module(bio(pheno_xmlmap_phenoxml)).
:- use_module(bio(xml_transform),[xml_compact_to_full/2]).
:- use_module(library(sgml_write)).
:- [bio(parser_general)].

:- dynamic default_namespace/1.

% TODO - handle obsoletes

io:parse_stream(phenosyn,IO):-
        read_blocks(IO).
io:parse_stream_with_cache(phenosyn,IO,File):-
        read_blocks_with_cache(IO,File).

read_blocks_with_cache(IO,File):-
        tell(File),
        read_blocks(IO,on),
        told.

read_blocks(IO):-
        read_stream_to_codes(IO,Codes),
        dcg_phenotypes(Ps,Codes,[]),
        stream_property(IO,file_name(Source)),
        maplist(assert_phenotree(Source),Ps).

assert_phenotree(Source,PTerm):-
        xml_compact_to_full(phenoset([PTerm]),XmlElement),
        io:xml_to_preds(phenoxml,XmlElement,Facts),
        forall(member(Fact,Facts),
               insert_fact_with_source(pheno_db:Fact,Source)).

remove_spaces(A,B):-
        concat_atom(L,' ',A),
        concat_atom(L,B).

% -- grammar --
dcg_phenotypes([P|Ps]) --> dcg_phenotype(P),!,invis,dcg_phenotypes(Ps).
dcg_phenotypes([]) --> [].


dcg_phenotype(P) --> hook_dcg_phenotype(P).
dcg_phenotype(phenotype([T|PCs])) --> "Type=",!,invis,dcg_typeref(T),invis,dcg_phenotype_characters(PCs).
dcg_phenotype(phenotype(PCs)) --> dcg_phenotype_characters(PCs).

%:- multifile hook_dcg_phenotype/3.
%hook_dcg_phenotype(_) --> {fail}.

hook_dcg_phenotype(phenotype_manifestation([provenance([about=Pub]),manifest_in(ML),P])) -->
        "PUB=",!,invis,dcg_obo_id(Pub),invis,
        dcg_manifest_in_list(ML),invis,
        dcg_phenotype(P).

dcg_manifest_in_list([M|ML]) --> dcg_manifest_in(M),!,dcg_manifest_in_list(ML).
dcg_manifest_in_list([]) --> [].

% --GENOTYPES--
%   refered to by name in phenosyntax; we must lookup the ID in a persistent DB
%   if it's mot there, create a new one and preserve it
%dcg_manifest_in(genotype=G) --> "GT=",!,invis,dcg_genotype(GN),invis,{lookup_or_insert('OBD:genotype',obd:identifier_label(G,GN),G)}.
dcg_manifest_in(genotype=G) --> "GT=",!,invis,dcg_genotype(GN),invis,{lookup_or_insert('OBD:genotype',pheno_db:genotype(G,GN),G)}.
dcg_manifest_in(T) --> "GC=",!,invis,dcg_typeref(T),invis.

dcg_genotype(G) --> "genotype",invis,"(",!,invis,token(G^")"),")".
dcg_genotype(G) --> quoted_val(G).
dcg_genotype(G) --> not_ws_atom(G).
%dcg_genotype_rest('') --> ")",!.
%dcg_genotype_rest( G)--> "(",!,dcg_genotype(G)


dcg_phenotype_characters([PC|PCs]) --> dcg_phenotype_character(PC),invis,dcg_phenotype_characters(PCs).
%dcg_phenotype_characters([]) --> [].
dcg_phenotype_characters([PC]) --> dcg_phenotype_character(PC),!.

%dcg_phenotype_character(phenotype_character([Desc,E|Qs])) --> dcg_desc(Desc),!,invis,dcg_bearer(E),invis,dcg_qualities(Qs).
dcg_phenotype_character(phenotype_character([E|Qs])) --> dcg_bearer(E),!,invis,dcg_qualities(Qs).
%dcg_phenotype_character(phenotype_character([Desc])) --> dcg_desc(Desc),!.

dcg_bearer(bearer([T])) --> "E=",!,invis,dcg_typeref(T).

dcg_qualities([Q|Qs]) --> dcg_quality(Q),!,invis,dcg_qualities(Qs).
dcg_qualities([]) --> [].
dcg_quality(quality([Q|Tags])) --> "Q=",!,invis,dcg_typeref(Q),invis,dcg_quality_tags(Tags).
dcg_quality(Desc) --> dcg_desc(Desc),!.

dcg_quality_tags([QT|QTs]) --> dcg_quality_tag(QT),!,invis,dcg_quality_tags(QTs).
dcg_quality_tags([]) --> [].
dcg_quality_tag(count=C) --> "C=",!,invis,number(C).
dcg_quality_tag(related_entity([T])) --> "E2=",!,invis,dcg_typeref(T).
dcg_quality_tag(temporal_qualifier([relation=Rel,time_range([T])]))--> "T=",!,invis,word(Rel),"(",!,dcg_typeref(T),")".
dcg_quality_tag(modifier([T])) --> "Tag=",!,invis,dcg_typeref(T).

dcg_desc_opt('') --> [].
dcg_desc_opt(D) --> dcg_desc(D).

dcg_desc(description(D)) --> "Desc=",!,invis,quoted_val(D).

dcg_typeref(typeref([about=ID|Quals])) --> dcg_obo_id(ID),dcg_conjoined_qualifiers(Quals).

dcg_conjoined_qualifiers([QT|QTs]) --> "^",!,invis,dcg_qualifier(QT),invis,dcg_conjoined_qualifiers(QTs).
dcg_conjoined_qualifiers([]) --> [].

dcg_qualifier(qualifier([relation=Rel,holds_in_relation_to([T])])) --> word(Rel),"(",!,dcg_typeref(T),")".

dcg_obo_id(ID) --> word(Prefix),":",!,word(Local),{concat_atom([Prefix,Local],':',ID)}.
dcg_obo_id(ID) --> not_ws_atom(ID).  % liberal rules - any non-WS goes

quoted_val('') --> """","""",!.
quoted_val(V) --> """",!,token(V^""""),"""".

% comments
invis --> ws_plus,!,invis.
invis --> "/*",!,comment_block_internal,invis.
invis --> [].

comment_block_internal --> "*/",!.
comment_block_internal --> [_],!,comment_block_internal.


skip_until_newline([],[]).
skip_until_newline([C|CL],[C|CL]):-
        newline(C),
        !.
skip_until_newline([_|CL],CL2):-
        skip_until_newline(CL,CL2).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/08/24 23:51:06 $
  @license LGPL

  ---+ Name
  ---++ parser_phenosyn
- parses phenosyn 1.0 and 1.2 ontology formats

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).

  demo:-
    load_biofile(phenosyn,'test.phenosyn'),
    write_biofile(owl,'test.owl'),
    writeln('Showing first 100 lines of exported OWL file:'),
    shell('head -100 test.owl').
  ==

  ---+ Description

  parses phenosyn format files - stores data in the ontol_db data module,
as predicates class/2, subclass/2, restriction/3, etc
  
  ---++ See also
  
  <http://www.geneontology.org> GO

  */

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test_phenosyn)=
      ((true,load_biofile(phenosyn,'phenotest.txt')))/[]).
        
unittest(test(load_phenosyn_file,
             [_=load(test_phenosyn)],
            (   ensure_loaded(bio(pheno_db))),
            true)).
