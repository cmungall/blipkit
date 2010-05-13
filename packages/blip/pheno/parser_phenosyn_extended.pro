/* -*- Mode: Prolog -*- */


:- module(parser_phenosyn_extended,
          [
          ]).

:- use_module(bio(parser_phenosyn)).

:- multifile parser_phenosyn:hook_dcg_phenotype/3.

parser_phenosyn:hook_dcg_phenotype(phenotype_manifestation(provenance([id=Pub]),manifest_in([about=G],P))) -->
{trace},
                                   "PUB=",!,invis,dcg_obo_id(Pub),invis,
                                   "GT=",!,invis,not_ws_atom(G),
                                   parser_phenosyn:dcg_phenotype(P).
        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/08/24 23:51:06 $
  @license LGPL

  ---+ Name
  ---++ parser_phenosyn_extended
- parses phenosyn_extended phenosyn extension

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).

  demo:-
    load_biofile(phenosyn_extended,'test.phenosyn_extended'),
    write_biofile(owl,'test.owl'),
    writeln('Showing first 100 lines of exported OWL file:'),
    shell('head -100 test.owl').
  ==

  ---+ Description

  ---++ See also
  
  <http://www.geneontology.org> GO

  */