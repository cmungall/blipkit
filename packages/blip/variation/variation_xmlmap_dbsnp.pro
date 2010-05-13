/* -*- Mode: Prolog -*- */

:- module(variation_xmlmap_dbsnp,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(dbsnp,XML,PL):-
        apply_xmlpred(variation_xmlmap_dbsnp,XML,PL).

xmlpred('NSE-ExchangeSet',_,[],translate('NSE-ExchangeSet_rs-set'),[]).
xmlpred('NSE-ExchangeSet_rs-set',_,[],translate('NSE-rs'),[]).
xmlpred('NSE-rs',_,variant(ID,Class,Type,Het),
     [
      let(ID='NSE-rs_refsnp-id'),
      let(Class='NSE-rs_snp-class'),
      let(Type='NSE-rs_snp-type'),
      let(Het='NSE-rs_het'),
      prolog(M=in(ID)),
      translate('NSE-rs_seq-5',M),
      translate('NSE-rs_seq-3',M),
      translate('NSE-rs_ss-list',M)]).

xmlpred(Match,in(ID),variant_seq(ID,Type,Seq),
        [
         prolog((Match='NSE-rs_seq-5'->Type=5
                ;Match='NSE-rs_seq-3'->Type=3)),
         let(Seq='.')]).
xmlpred('NSE-rs_ss-list',in(ID),[],[]).

/** <module>   bridging layer from dbsnp-xml to native variation model
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/06/22 02:16:16 $
  @license LGPL


  ==
  ==

  

  you should not need this module directly - handled by module io
  
  */