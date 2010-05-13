/* -*- Mode: Prolog -*- */



:- module(curation_writer_obo,
          [
           write_curation/2
          ]).

:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(serval)).
:- use_module(bio(ontol_writer_obo)).

io:redirect_stdout(curation_obo).
        
io:write_all(curation_obo,_,_):-
        write_header(obo),
        dsetof(O,ID^entity_resource(ID,O),Os),
        forall((member(O,Os),io:in_include_list(O,ontology)),
               write_ontology(obo,O)),
        % write properties that dont belong to an ontology
        forall_distinct((property(ID),\+entity_resource(ID,_)),
                        write_entity(obo,ID)),
        % write instances that dont belong to an ontology
        forall_distinct((inst(ID),\+entity_resource(ID,_)),
                        write_inst(obo,ID)),
        % write all curations
        forall_distinct(curation(ID),
                        write_curation(curation_obo,ID)).


:- mode write_curation(+,+) is det.
write_curation(curation_obo,ID):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        user:ensure_loaded(bio(curation_db)),   % serval runs in user space... TODO; inherit this?
        forall(curation_statement(ID,S,R,O),
               write_sterm([],xml([]),curation_statement(ID,S,R,O,fail))),
        forall(negative_curation_statement(ID,S,R,O),
               write_sterm([],xml([]),curation_statement(ID,S,R,O,true))).

curation_statement(ID,S,R,O,IsNegated) =>
   openstanza('Annotation'),
   %%%%tvpairnl(id,ID) where (\+ is_anonymous(ID)), % only write REAL ids
   %%%%%tagnodenl(true,instance_of,'oban:Annotation'),
   tagnodenl(true,publisher,X) forall_unique curation_publisher(ID,X),
   tagnodenl(true,namespace,X) forall_unique entity_resource(ID,X),
   tagnodenl(true,source,X) forall_unique curation_source(ID,X),
   tagnodenl(true,evidence,X) forall_unique curation_evidence(ID,X),
   tagnodenl(true,subject,S),
   tagnodenl(true,relation,R),
   tagnodenl(true,object,O),
   tvpairnl(is_negated,'true') where IsNegated,
   newline,
   evidence(X) forall_unique curation_evidence(ID,X).

% should really be handled in curation->ontolog bridge BUT we don't want all the other instances...
evidence(ID) =>
  openstanza('Instance'),
  tvpairnl(id,ID),
  tvpairnl(instance_of,X) where evidence_type(ID,X),
  tagnodenl(true,property_value,with,X) where evidence_with(ID,X),
  newline.

% use advanced obo1.3 option
:- multifile obo_format_option/1.
ontol_writer_obo:obo_format_option(pheno_syntax):- !.


/** <module>
  @author Chris Mungall
  @version  $Revision: 1.14 $
  @date  $Date: 2006/03/27 17:57:51 $
  @license LGPL

  ---+ Name
  ---++ ontol_writer_obo
- generates obo format from ontol_db

  ---+ Synopsis

  ==
  

  ==

  ---+ Description

**/
