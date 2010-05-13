/* -*- Mode: Prolog -*- */



:- module(seqfeature_xmlmap_game,[]).
:- use_module(bio(xml_transform)).
:- use_module(library(porter_stem),[tokenize_atom/2]).

io:xml_to_preds(game,XML,PL):-
        apply_xmlpred(seqfeature_xmlmap_game,XML,PL).

get_id('',ID,Type):- concat_atom(['auto:',Type],Base),gensym(Base,ID),!.
get_id(ID,ID,_).

xmlpred(game,_,[],
        [translate(seq,top),
         translate(map_position),
         translate(annotation),
         translate(computational_analysis)
        ]).
xmlpred(seq,top,feature(ID,N,Type), % focus seq
     [
      let(ID=att(id)),
      let(N=name),
      prolog(Type=contig),
      prolog(M=in_feature(ID,Type)),
      translate(residues,M),
      translate(organism,M)
      ]).
xmlpred(map_position,_,featureloc(ID,Src,Beg,End,Strand,0,0,[]),
     [
      let(ID=att(seq)),
      let(Src=chromosome),
      translate(span,range(Beg,End,Strand))
      ]).
xmlpred(seq_relationship,in_feature(ID,_),
        featureloc(ID,Src,Beg,End,Strand,Rank,0,[]),
        [
         let(Src=att(seq)),
         let(Type=att(type)),
         prolog(   Type=query
               ->  Rank=0
               ;   Rank=1),
         translate(span,range(Beg,End,Strand))
        ]).
xmlpred(span,range(Beg,End,Strand),[],
     [
      let(GameBegAtom=start),
      let(GameEndAtom=end),
      prolog(atom_number(GameBegAtom,GameBeg)),
      prolog(atom_number(GameEndAtom,GameEnd)),
      prolog((   GameBeg < GameEnd
             ->  Strand = 1,
                 Beg is GameBeg-1,
                 End = GameEnd
             ;   Strand = -1,
                 Beg = GameBeg,
                 End is GameEnd-1))
      ]).
xmlpred(annotation,_,feature(ID,N,gene),
     [
      let(ID=att(id)),
      let(N=name),
      let(Type=type),
      prolog(M=in_feature(ID,Type)),
      translate(property,M),
      translate(synonym,M),
      translate(residues,M),
      translate(dbxref,M),
      translate(comment,M),
      translate(feature_set,M)
      ]).
xmlpred(feature_set,
        in_feature(GID,AnnotType),
        [feature(ID,N,Type),
         feature_relationship(ID,GID,part_of)],
        [
         let(ID=att(id)),
         let(N=name),
         %let(Type=type), in game this is always transcript
         prolog(M=in_feature(ID,AnnotType)), % eg 'transcript'
         prolog((   AnnotType=gene
                ->  Type=mRNA
                ;   Type=AnnotType)),
         translate(property,M),
         translate(synonym,M),
         translate(residues,M),
         translate(dbxref,M),
         translate(comment,M),
         translate(feature_span,M),
         translate(seq_relationship,M)
        ]).
xmlpred(feature_span,
        in_feature(TID,_ParentType),
        [feature(ID,N,Type),
         feature_relationship(ID,TID,part_of)],
        [
         let(AttID=att(id)),
         let(N=name),
         let(SpanType=type),
         prolog((   SpanType=''
                ->  Type=exon
                ;   Type=SpanType)),
         prolog(get_id(AttID,ID,Type)),
         prolog(M=in_feature(ID,Type)),
         translate(att(produces_seq),M),
         translate(property,M),
         translate(synonym,M),
         translate(residues,M),
         translate(dbxref,M),
         translate(comment,M),
         translate(seq_relationship,M)
        ]).
xmlpred(computational_analysis,_,analysis(ID,Prog,DB),
     [
      let(Prog=program),
      let(DB=database),
      prolog(concat_atom([Prog,':',DB],ID)),
      prolog(M=in_analysis(ID)),
      translate(result_set,M)
      ]).
xmlpred(result_set,
        in_analysis(AID),
        [feature(ID,N,Type),
         feature_analysis(ID,AID)],
        [
         let(AttID=att(id)),
         let(N=name),
         let(SetType=type),
         prolog((   SetType=''
                ->  Type=match
                ;   Type=SetType)),
         prolog(get_id(AttID,ID,Type)),
         prolog(M=in_feature(ID,Type)), % eg 'transcript'
         translate(property,M),
         translate(output,M),
         translate(result_span,M),
         translate(seq_relationship,M)
        ]).
xmlpred(result_span,
        in_feature(RID,_ParentType),
        [feature(ID,N,Type),
         feature_relationship(ID,RID,part_of)],
        [
         let(AttID=att(id)),
         let(N=name),
         let(SpanType=type),
         prolog((   SpanType=''
                ->  Type=match_span
                ;   Type=SpanType)),
         prolog(get_id(AttID,ID,Type)),
         prolog(M=in_feature(ID,Type)),
         translate(property,M),
         translate(output,M),
         translate(result_span,M),
         translate(seq_relationship,M)
        ]).
xmlpred(att(produces_seq),
        in_feature(ID,_),
        feature_relationship(PID,ID,coded_by),
        [
         let(PID='.')
        ]).
xmlpred(att(length),in_feature(ID,_),feature_seqlen(ID,X),let(X='.')).
xmlpred(synonym,in_feature(ID,_),feature_synonym(ID,X),let(X='.')).
xmlpred(organism,in_feature(ID,_),feature_organism(ID,X),let(X='.')).
xmlpred(comment,in_feature(ID,_),featureprop(ID,comment,X),let(X=text)).
xmlpred(residues,
        in_feature(ID,_),
        feature_residues(ID,Seq),
        [let(GameSeq='.'),
         prolog(seqfeature_xmlmap_game:strip_ws(GameSeq,Seq))]).
xmlpred(dbxref,in_feature(ID,_),feature_dbxref(ID,X),
        [
         let(DB=xref_db),
         let(Acc=db_xref_id),
         prolog(concat_atom([DB,':',Acc],X))
        ]).
xmlpred(property,
     in_feature(ID,_),
     featureprop(ID,Type,Value),
     [
      let(Type=type),
      let(Value=value)
      ]).
xmlpred(output,
     in_feature(ID,_),
     featureprop(ID,Type,Value),
     [
      let(Type=type),
      let(Value=value)
      ]).

strip_ws(A1,A2):-
        debug(game,stripping,[]),
        tokenize_atom(A1,Toks),
        concat_atom(Toks,A2).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest([
        load(X)=
       load_biofile(game,X)/[],
        
        test(load_file,
             [_=load('tiny.game.xml')],
             (   ensure_loaded(bio(seqfeature_db)),
                 setof(Type,ID^N^feature(ID,N,Type),Types),
                 writeln(types=Types)),
             (member(match,Types))),
        
        test(load_file2,
             [_=load('example.game.xml')],
             (   ensure_loaded(bio(seqfeature_db)),
                 setof(Type,ID^N^feature(ID,N,Type),Types),
                 writeln(types=Types)),
             (member(tRNA,Types)))
       ]).

        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/12/07 00:06:41 $
  @license LGPL

  ---+ Name
%  seqfeature_xmlmap_game

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(seqfeature_db)).
  :- use_module(bio(seqfeature_xmlmap_game)).

  demo:-
    load_biofile(game,'Rab1.game-xml'),
    setof(ID-Name,gene(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  bridging layer from game-xml to native seqfeature model

  you should not need this module directly - handled by module io
  
  */