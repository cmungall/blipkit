/* -*- Mode: Prolog -*- */



:- module(ontol_owlmap_from_ncithesaurus,
          []).

:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(mode)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns(ncithesaurus,'http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#').

xref_db(ncithesaurus:'Swiss_Prot').
xref_db(ncithesaurus:'Locus_ID').
xref_db(ncithesaurus:'OMIM_Number').
xref_db(ncithesaurus:'UMLS_CUI').
xref_db(ncithesaurus:'CAS_Registry').
xref_db(ncithesaurus:'FDA_UNII_Code').
xref_db(ncithesaurus:code,'NCI_code').
xref_db(NS:P,P):- xref_db(NS:P).

:- multifile ontol_bridge_from_owl:reserved_property/1.
ontol_bridge_from_owl:reserved_property(ncithesauris:'Synonym').
ontol_bridge_from_owl:reserved_property(ncithesauris:'FULL_SYN').
ontol_bridge_from_owl:reserved_property(ncithesauris:'DEFINITION').
ontol_bridge_from_owl:reserved_property(NSExp:P):- xref_db(NS:P),rdf_global_id(NS,NSExp).

metadata_db:entity_xref(C,X):-
        rdfid_oboid(Res,C),
        xref_db(Prop,Xref_DB),
        rdf_global_id(Prop,PropExpanded),
        rdf_has(Res,PropExpanded,Code),
        literal_to_native(Code,Atom),
        concat_atom([Xref_DB,Atom],':',X).

metadata_db:entity_synonym(C,Syn):-
        rdfid_oboid(Res,C),
        rdf_has(Res,ncithesaurus:'Synonym',Lit),
        literal_to_native(Lit,Syn).
metadata_db:entity_synonym(C,Syn):-
        rdfid_oboid(Res,C),
        rdf_has(Res,ncithesaurus:'FULL_SYN',Lit),
        literal_to_native(Lit,Syn).

ontol_db:def(C,Def):-
        rdfid_oboid(Res,C),
        rdf_has(Res,ncithesaurus:'DEFINITION',Lit),
        literal_to_native(Lit,Def).

/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ ontol_owlmap_from_ncithesaurus
- 

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_owlmap_from_ncithesaurus)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_bridge_from_owl)).
  
  % 
  demo:-
    load_biofile(owl,'ncit_subset.owl'),
    class(ID,rna),
    format('In biopax, rna is a subclass of the following:~n',[]),
    forall(subclass(ID,PID),
           showclass(PID)).

  showclass(ID):-
    class(ID,N),
    format('Class ID:~w~nClass name:~w~n',[ID,N]).
  ==


  ---+ Description

  This configures the owl -> ontol_db mapping so that NCIT specific annotationproperties are treated correctly
  
**/
