/* -*- Mode: Prolog -*- */



:- module(seqfeature_xmlmap_chaos,[]).

:- use_module(bio(xml_transform)).


io:xml_to_preds(chaos,XML,PL):-
        apply_xmlpred(seqfeature_xmlmap_chaos,XML,PL).
io:preds_to_xml(chaos,PL,XML):-
        reverse_xmlpred(seqfeature_xmlmap_chaos,XML,PL).

xmlpred(chaos,_,[],
     [translate(feature),
      translate(feature_relationship)]).
xmlpred(feature,_,[seqfeature_db:feature(ID),
                   seqfeature_db:feature_type(ID,Type),
                   metadata_db:entity_label(ID,N)],
        [
         let(ID=feature_id),
         let(N=name),
         let(Type=type),
         prolog(M=in_feature(ID)),
         translate(seqlen,M),
         translate(residues,M),
         translate(dbxref,M),
         translate(organism,M),
         translate(feature_cvterm,M),
         translate(featureloc,M),
         translate(featureprop,M)
        ]).
xmlpred(seqlen,in_feature(ID),feature_seqlen(ID,X),let(X='.')).
xmlpred(residues,in_feature(ID),feature_residues(ID,X),let(X='.')).
xmlpred(dbxref,in_feature(ID),feature_dbxref(ID,X),let(X='.')).
xmlpred(organism,in_feature(ID),feature_organism(ID,X),let(X='.')).
xmlpred(featureloc,
        in_feature(ID),
        featureloc(ID,Src,Beg,End,Strand,Rank,Group,[]),
        [
         let(Src=srcfeature_id),
         let(Beg=number(nbeg)),
         let(End=number(nend)),
         let(Strand=number(strand)),
         let(Rank=number(rank)),
         let(Group=number(locgroup))
        ]).
xmlpred(featureprop,
     in_feature(ID),
     featureprop(ID,Type,Value),
     [
      let(Type=type),
      let(Value=value)
      ]).
xmlpred(feature_cvterm,
     in_feature(ID),
     feature_cvterm(ID,Type,ToClass),
     [
      let(Type=type),
      let(ToClass=cvterm)
      ]).
xmlpred(feature_relationship,_,feature_relationship(Su,Ob,Type,Rank),
     [
      let(Su=subject_id),
      let(Ob=object_id),
      let(Type=type),
      let(Rank=rank)
      ]).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest([
        load(rab1)=
       load_biofile(chaos,'Rab1.chaos-xml')/[],
        
        test(load_file,
             [_=load(rab1)],
             (   ensure_loaded(bio(seqfeature_db)),
                 setof(N,ID^gene(ID,N),Ns)),
             (Ns=['Rab1'])),

        test(reverse,
             [_=load(rab1)],
             (   ensure_loaded(bio(seqfeature_db)),
                 ensure_loaded(bio(dbmeta)),
                 db_facts(seqfeature_db,PL),
                 io:preds_to_xml(seqfeature_db,PL,XML),
                 writeln(XML)),
             true)
       ]).

        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/10/24 01:42:24 $
  @license LGPL

  ---+ Name
%  seqfeature_xmlmap_chaos

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(seqfeature_db)).
  :- use_module(bio(seqfeature_xmlmap_chaos)).

  demo:-
    load_biofile(chaos,'Rab1.chaos-xml'),
    setof(ID-Name,gene(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  bridging layer from chaos-xml to native seqfeature model

  you should not need this module directly - handled by module io
  
  */