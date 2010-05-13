/* -*- Mode: Prolog -*- */


:- module(parser_phenote,
          [
          ]).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/08/24 23:51:06 $
  @license LGPL

  ---+ Name
  ---++ parser_phenote
- parses phenote phenosyn extension

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).

  demo:-
    load_biofile(phenote,'test.phenote'),
    write_biofile(owl,'test.owl'),
    writeln('Showing first 100 lines of exported OWL file:'),
    shell('head -100 test.owl').
  ==

  ---+ Description

  ---++ See also
  
  <http://www.geneontology.org> GO

  */