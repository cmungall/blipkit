/* -*- Mode: Prolog -*- */



:- module(seqfeature_xmlmap_chado,[]).

:- use_module(bio(xml_transform)).


io:xml_to_preds(chado,XML,PL):-
        apply_xmlpred(seqfeature_xmlmap_chado,XML,PL).

nodes_index([],[]).
nodes_index([N|Ns],[Id-N|Ix]):-
        node_id(N,Id),
        !,
        nodes_index(Ns,Ix).
nodes_index([_|Ns],Ix):-
        nodex_index(Ns,Ix).
        
node_id(N,Id):-
        N=element(_,Atts,_),
        member(id=Id,Atts).

get_type_name(element(type_id,[],[Id]),Ix,Type):-
        member(Id-N,Ix),        % lookup
        get_type_name(N,Ix,Type).
get_type_name(N,Type):-
        pxpath(N,[cvterm,name],Type).

xmlpred(chado,_,[],
        [
         translate(feature,index([]))
        ]).
xmlpred(feature,index(Idx),feature(ID,N,Type),
        [
         let(ID=uniquename),
         let(N=name),
         translate(type_id,get_type_name(Type)),
         prolog(M=index(Idx)-in_feature(ID)),
         translate(featureloc,M)
        ]).
xmlpred(type_id,index(Ix)-Type,[],
        [
         prolog(Node,get_type_name(Node,Ix,Type))
        ]).
xmlpred(seqlen,in_feature(ID),feature_seqlen(ID,X),let(X='.')).
xmlpred(residues,in_feature(ID),feature_residues(ID,X),let(X='.')).
xmlpred(dbxref,in_feature(ID),feature_dbxref(ID,X),let(X='.')).
xmlpred(featureloc,
     in_feature(ID),
     featureloc(ID,Src,Beg,End,Strand,Rank,Group,[]),
     [
      let(Src=srcfeature_id),
      let(Beg=nbeg),
      let(End=nend),
      let(Strand=strand),
      let(Rank=rank),
      let(Group=group)
      ]).
xmlpred(featureprop,
     in_feature(ID),
     featureprop(ID,Type,Value),
     [
      let(Type=type),
      let(Value=value)
      ]).
xmlpred(feature_relationship,_,feature_relationship(Su,Ob,Type),
     [
      let(Su=subject_id),
      let(Ob=object_id),
      let(Type=type)
      ]).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest([
        load(rab1)=
       load_biofile(chado,'Rab1.chado-xml')/[],
        
        test(load_file,
             [_=load(rab1)],
             (   ensure_loaded(bio(seqfeature_db)),
                 setof(N,ID^gene(ID,N),Ns)),
             (Ns=['Rab1']))
       ]).

        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
%  seqfeature_xmlmap_chado

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(seqfeature_db)).
  :- use_module(bio(seqfeature_xmlmap_chado)).

  demo:-
    load_biofile(chado,'Rab1.chado-xml'),
    setof(ID-Name,gene(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  bridging layer from chado-xml to native seqfeature model

  you should not need this module directly - handled by module io
  
  */