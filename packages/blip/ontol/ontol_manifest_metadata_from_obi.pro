/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_obi,
          []).

:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(mode)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns(bfo,'http://www.ifomis.org/bfo/1.1#').
:- rdf_register_ns(snap,'http://www.ifomis.org/bfo/1.1/snap#').
:- rdf_register_ns(span,'http://www.ifomis.org/bfo/1.1/span#').

:- rdf_register_ns('OBO_REL','http://www.obofoundry.org/ro/ro.owl#').

:- rdf_register_ns('OBI','http://purl.obofoundry.org/obo/OBI_').
:- rdf_register_ns('IAO','http://purl.obofoundry.org/obo/IAO_').
%:- rdf_register_ns('OBI','http://purl.obofoundry.org/obo/').

:- multifile ontol_bridge_from_owl:suppress_resource/1.
ontol_bridge_from_owl:suppress_resource('http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').

%:- multifile ontol_bridge_from_owl:reserved_property/1.
%ontol_bridge_from_owl:reserved_property(obi:definition).
%ontol_bridge_from_owl:reserved_property('obi:definition').
%ontol_bridge_from_owl:reserved_property('obi:branch').

%metadata_db:entity_resource(C,Ont):-
%        rdfid_oboid(C_RDF,C),
%        rdf_has(C_RDF,obofoundry:'todo',Lit),
%        literal_to_native(Lit,Ont1),
%        atom_concat(Toks,' ',Ont1),
%        atom_concat(Toks,'_',Ont).

metadata_db:entity_synonym(C,Syn):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF,'IAO':'0000111',Lit),
        literal_to_native(Lit,Syn).

metadata_db:entity_name(C,Syn):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF,'IAO':'0000111',Lit),
        \+ rdf_has(C_RDF,'rdfs:label',_),
        literal_to_native(Lit,Syn).

metadata_db:entity_synonym_scope(C,Syn,exact):-
        entity_synonym(C,Syn).

ontol_db:def(C,Def):-
        rdfid_oboid(Res,C),
        rdf_has(Res,'IAO':'0000115',Lit),
        literal_to_native(Lit,Def).

metadata_db:entity_obsolete(C,class):-
        rdfid_oboid(Res,C),
        rdf_has(Res,rdfs:subClassOf,oboInOwl:'ObsoleteClass').


% sometimes/always literal
%ontol_db:def_xref(C,X):-
%        rdfid_oboid(Res,C),
%        rdf_has(Res,obi:'OBI_0000279',X1),
%        rdfid_oboid(X1,X).

