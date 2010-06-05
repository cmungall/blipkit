/* -*- Mode: Prolog -*- */



:- module(annotation_db,
          []).

:- use_module(bio(dbmeta)).
:- datapreds(annotation_db,
             [
              annotation(pk('Annotation'),
                         fk('Type',class))
             - 'An information artefact that is created by some agent sourced from another information artefact'

             ]).

/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ annotation_db
- 

  ---+ Synopsis

  ==
  :- use_module(bio(annotation_db)).

  ==

  ---+ Description

  An annotation is an information artefact created by some agent (human or machine)

  annotations have a source, an optional description and zero or more
assertions, plus zero or more about-links to universals or instances

  sources are information artefacts: documents, database records, images, slices, maps

  sources can be nested, via links from assertions;
  - eg

  source is the provenance. The source can refer to 
  
**/