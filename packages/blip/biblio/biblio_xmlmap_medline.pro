/* -*- Mode: Prolog -*- */

:- module(biblio_xmlmap_medline,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(medline,XML,PL):-
        apply_xmlpred(biblio_xmlmap_medline,XML,PL).

xmlpred('MedlineCitationSet',_,[],
        [translate('MedlineCitation')]).
xmlpred('MedlineCitation',_,citation(ID,PMID),
     [
      let(ID='MedlineID'),
      prolog(M=in(ID)),
      translate('PubmedID',M),
      translate('Article',M)]).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/25 02:07:05 $
  @license LGPL


  ==
  :- use_module(bio(biblio_xmlmap_medline)).
  ==

  bridging layer from medline-xml to native biblio model

  you should not need this module directly - handled by module io
  
  */
