
/* -*- Mode: Prolog -*- */

:- module(seqfeature_db,
          [
           % extensional predicates
           feature/1,
           feature_relationship/4,
           featureloc/8,
           feature_seqlen/2,
           feature_residues/2,
           featureprop/3,
           feature_dbxref/2,
           feature_dbxref/3,
           feature_cvterm/3,
           feature_organism/2,
           
           % intensional predicates
           junction/1,
           enumerated_junction/1,
           feature/3,
           feature/2,
           feature_type/2,
           feature_label/2,
           implied_feature_type/3,
           implied_feature_type/2,
           featureloc/5,
           featureloc/6,
           featureloc/7,
           feature_start_junction/2,
           feature_end_junction/2,
           feature_strand/2,
           feature_cvterm/2,
           feature_relationship/2,
           feature_relationship/3,
           feature_range/2,
           feature_rangeset/2,
           feature_intersects/2,
           featureset_intersects/2,

           feature_upstream_of/2,
           
           feature_by_type_id/3,
           feature_by_typename/3,
           concat_subfeature_residues/3,
           pp_from_start_codon/3,
           range_on_processed_transcript/4,
           coord_on_processed_transcript/4,

           map_coordsystem/2
          ]).

:- use_module(bio(range)).
:- use_module(bio(bioseq)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(dbmeta)).

%%  feature(?Feature) is nondet
%    TODO: fix Type to use identifier, not label/name
:- extensional(feature/1).

%%  feature_type(?Feature,?Type) is nondet
%    TODO: fix Type to use identifier, not label/name
:- extensional(feature_type/2).

%%  feature_label(?Feature,?Label) is nondet
feature_label(F,N):- feature(F),entity_label(F,N).


%%  implied_feature_type(?Feature,?ImpliedType) is nondet
implied_feature_type(F,T):- implied_feature_type(F,T,_).

%%  implied_feature_type(?Feature,?ImpliedType,?AssertedType) is nondet
implied_feature_type(F,T,T):- feature_type(F,T).
implied_feature_type(F,TP,TC):-
        feature_type(F,TC), % todo - clause ordering
        subclassT(TC,TP).
implied_feature_type(F,TPN,TCN):-
        feature_type(F,TCN),     % todo - clause ordering; consider memoization
        class(TC,TCN),
        subclassT(TC,TP),
        class(TP,TPN).

%%  feature(?Feature,?Name,?Type) is nondet
feature(F,N,T):- entity_label(F,N),feature_type(F,T).  % TODO leftjoin??

%%  feature_relationship(?Subj,?Obj,?Type,?Rank) is nondet
:- extensional(feature_relationship/4).

%% feature_relationship(?ID,?PID)
%   as feature_relationship/3, untyped
%  
feature_relationship(ID,PID):-
        feature_relationship(ID,PID,_,_).
feature_relationship(ID,PID,T):-
        feature_relationship(ID,PID,T,_).

%%  featureloc(?Feature,?SrcFeature,?Beg,?End,?Strand) is nondet
:- extensional(featureloc/5).


featureloc(ID,Src,Beg,End,Strand,Rank,Group):-
        featureloc_rank(ID,Src,Beg,End,Strand,Rank),
        featureloc_group(ID,Src,Beg,End,Strand,Group).

%%  featureloc(?Feature,?SrcFeature,?Beg,?End,?Strand,?Rank,?Group,?Opts) is nondet
featureloc(ID,Src,Beg,End,Strand,Rank,Group,Opts):-
        featureloc(ID,Src,Beg,End,Strand,Rank,Group),
        featureloc_annots(ID,Src,Beg,End,Strand,Opts).

featureloc(ID,Src,Beg,End,Strand,Rank):-
        featureloc_rank(ID,Src,Beg,End,Strand,Rank).
featureloc(ID,Src,Beg,End):-
        featureloc(ID,Src,Beg,End,_Strand).

:- extensional(featureloc_rank/6).
:- extensional(featureloc_group/6).
:- extensional(featureloc_annots/6).


junction(J) :-        feature_start_junction(_,J).
junction(J) :-        feature_end_junction(_,J).

% table this:
% j(Seq,Pos,Strand)
enumerated_junction(j(Seq,P,Str)) :-
        % succeeds once for every Seq-Str
        call_unique(seq_str_upstream_outer(Seq,Str,PMin)),
        call_unique(seq_str_downstream_outer(Seq,Str,PMax)),
        (   between(PMin,PMax,P)
        ;   between(PMax,PMin,P)).

seq_str_upstream_outer(Seq,Str,P) :-
        upstream_outermost_feature(FU),
        feature_start_junction(FU,j(Seq,P,Str)).
seq_str_downstream_outer(Seq,Str,P) :-
        downstream_outermost_feature(FD),
        feature_end_junction(FD,j(Seq,P,Str)).


upstream_outermost_feature(F) :-
        feature(F),
        \+ ((feature(F2),
             feature_starts_before_start_of(F2,F))).
downstream_outermost_feature(F) :-
        feature(F),
        \+ ((feature(F2),
             feature_ends_after_end_of(F2,F))).


% seq_strand(Seq,Str):-

%% feature_start_junction(?F,?J)
feature_start_junction(ID,j(Src,Pos,Strand)):-
        featureloc(ID,Src,Pos,_,Strand).

%% feature_end_junction(?F,?J)
feature_end_junction(ID,j(Src,Pos,Strand)):-
        featureloc(ID,Src,_,Pos,Strand).

feature_strand(ID,Strand):-
        featureloc(ID,_,_,_,Strand,0,0,_).

%%  feature_seqlen(?Feature,?Length) is nondet
:- extensional(feature_seqlen/2).

%%  feature_residues(?Feature,?Residues) is nondet
:- extensional(feature_residues/2).

%%  featureprop(?Feature,?Type,?Value) is nondet
:- extensional(featureprop/3).

%%  feature_dbxref(?Feature,?DB,?Accession) is nondet
% DEPRECATED
:- extensional(feature_dbxref/3).

%%  feature_dbxref(?Feature,?Xref) is nondet
:- extensional(feature_dbxref/2).

%%  feature_cvterm(?Feature,?Type,?ToClass) is nondet
:- extensional(feature_cvterm/3).

:- extensional(feature_cvterm/2).

%%  feature_organism(?Feature,?Organism) is nondet
:- extensional(feature_organism/2).



% --

% (+,?) nd
% unifies R with a range of a subfeature of ID
subfeature_rangeset(ID,R):-
        feature_relationship(IDSub,ID,_T),
        feature_rangeset(IDSub,R).

subfeature_rangeset(ID,R,SubT):-
        feature_relationship(IDSub,ID,_T),
        feature(IDSub,_,SubT),
        feature_rangeset(IDSub,R).

%% concat_subfeature_residues(+ID,+SubfeatureTypeName,?Seq)
%
%  unifies Seq with an atom containing residues of all subfeatures of a
%certain type concatenated together
%
%  used to get the transcript sequence from exon sequences
%
%  ==
%  concat_subfeature_residues(T,exon,TSeq)
%  ==
concat_subfeature_residues(ID,Type,Seq):-
        findall(Rank-SubID,
                (   feature_relationship(SubID,ID,part_of,Rank),
                    feature(SubID,_,Type)),
                RankedSubIDs),
        RankedSubIDs=[_|_],
        sort(RankedSubIDs,SortedRankedSubIDs),
        findall(SubSeq,
                (   member(_-SubID,SortedRankedSubIDs),
                    extract_feature_residues(SubID,SubSeq)),
                SubSeqs),
        concat_atom(SubSeqs,Seq).

extract_feature_residues(ID,Seq):-
        featureloc(ID,SrcID,Beg,End),
        feature_residues(SrcID,SrcSeq),
        sub_sequence(SrcSeq,range(Beg,End),Seq).

%% pp_from_start_codon(+Codon,?PSeq,?PRange)
%   infers polypeptide seq and range (outermost extent) given a stop codon feature ID
%  
pp_from_start_codon(C,PSeq,range(S,CStart,CStop,CDir)):-
        debug(seqfeature,'pp_from ~w',[C]),
        feature_relationship(C,T,part_of,Rank),
        debug(seqfeature,'c_on1 ~w ~w ~w',[C,T,Rank]),
        feature_residues(T,TSeq),
        featureloc(C,S,CStart,_,CDir),
        coord_on_processed_transcript(CStart,T,CStartM,E), % (+,+,?,?)
        CStartM >= 0,
        debug(seqfeature,'~w has range ~w on ~w[~w] => ~w  ex:~w',[C,CStart,T,Rank,CStartM,E]),
        sub_atom(TSeq,CStartM,_,0,CDSSeq1),
        translate_dna(CDSSeq1,PSeq),
        debug(seqfeature,'~w',[PSeq]),
        atom_length(PSeq,PSeqLen),
        CStopM is CStartM+PSeqLen*3,
        %debug(game,'c ~w rg "~w" ~w ~w',[T,CStartM,PSeq,CStopM]),
        coord_on_processed_transcript(CStop,T,CStopM,ETerm), % (+,+,?,?)        
        debug(seqfeature,'cds ~w ~w from ~w on ~w',[CStart,CStop,CStopM,ETerm]).
        

%% range_on_processed_transcript(+Range,+Transcript,?RangeMapped,?Exon)
%   maps a location from genome coords to processed transcript coords
%  mode: nondet (via different exons TODO fix this?)
%  
range_on_processed_transcript(R,T,RM,E):-
        nonvar(R),
        feature_relationship(E,T,part_of),
        feature(E,_,exon),
        throw(not_implemented_yet),
        feature_rangeset(E,ER),
        intersects(ER,R),
        relative_range(R,ER,RM),
        debug(seqfeature,'relative range ~w ~w ~w',[R,ER,RM]).
        
%% coord_on_processed_transcript(+Point,+Transcript,?MappedPoint,?Exon)
%  maps integer coords between genome and processed transcript
%  mode: nondet (via different exons??? TODO fix this?)
coord_on_processed_transcript(P,T,PM,E):-
        nonvar(P),
        feature_relationship(E,T,part_of,Rank),
        feature(E,_,exon),
        feature_rangeset(E,ER),
        point_overlaps(P,ER),
        project_point(P,ER,PM1),
        debug(seqfeature,'overlaps ~w ~w ~w on ~w[~w]',[P,ER,PM1,T,Rank]),
        exon_beg_in_processed_transcript(E,T,EBegT),
        PM is PM1+EBegT.

%% coord_on_processed_transcript(?Point,+Transcript,+MappedPoint,?Exon)
coord_on_processed_transcript(P,T,PM,E):-
        var(P),
        nonvar(PM),
        exon_beg_in_processed_transcript(E,T,EBegT),
        P1 is PM - EBegT,
        P1 >= 0,
        feature_rangelength(E,ELen),
        P1 =< ELen,
        featureloc(E,_,EBeg,_,Dir),
        P is EBeg + P1*Dir.

% (?,+,?)
% gets position of exon relative to beginning of processed transcript
exon_beg_in_processed_transcript(E,T,Beg):-
        feature_relationship(E,T,part_of,Rank),
        feature(E,_,exon),
        solutions(E1,(feature_relationship(E1,T,part_of,Rank1),
                      Rank1<Rank,
                      feature(E1,_,exon)),
                  Es),
        sumof(ELen,(member(E1,Es),feature_rangelength(E1,ELen)),Beg).

%% feature_rangeset(+ID,?Range)
%
%   finds the range of a feature; the range term can be used with
%the range module. See the range module for details about the Range term
%
%  TODO: check srcfeature
%  
%  interbase
%  
% non-terminal
feature_rangeset(ID,rangeset(RL)):-
        feature(ID,_,_),
        setof(Rsub,subfeature_rangeset(ID,Rsub),RL).
% terminal
feature_rangeset(ID,Range):-
        not(subfeature_rangeset(ID,_)),
        feature_range(ID,Range).

feature_rangesetlength(ID,Len):-
        featureloc(ID,_,B,E,_),
        Len is abs(B-E).

%% feature_range(+ID,?Range)
%
%   finds the range of a feature - always a single
%   interval (i.e. not a rangeset)
feature_range(ID,range(Src,Beg,End,Strand)):-
        featureloc(ID,Src,Beg,End,Strand).

%% featureset_intersects(+ID1,+ID2)
%  
%  tests if two features intersect
%
%  a feature intersects only if the leaf features intersects; ie two
%genes intersect if their transcripts intersect; their transcripts
%intersect if their exons intersect
%
%  todo: more sophisticated rules for genes
%  
%  
featureset_intersects(IDx,IDy):-
        feature_rangeset(IDx,Rx),
        feature_rangeset(IDy,Ry),
        intersects(Rx,Ry).

%% featureset_intersects(+ID1,+ID2)
%  
%  tests if two features intersect.
%  the feature is treated as a single interval
feature_intersects(X,Y):-
        feature_range(X,XR),
        feature_range(Y,YR),
        intersects(XR,YR).

feature_upstream_of(X,Y):-
        featureloc(X,S,_,EX,Str),
        featureloc(Y,S,BY,_,Str),
        ((EX - BY) * Str) < 0.
feature_starts_before_start_of(X,Y) :-
        featureloc(X,S,BX,_,Str),
        featureloc(Y,S,BY,_,Str),
        ((BX - BY) * Str) < 0.
feature_ends_after_end_of(X,Y) :-
        featureloc(X,S,_,EX,Str),
        featureloc(Y,S,_,EY,Str),
        ((EX - EY) * Str) > 0.
        

%% feature_cvterm(ID,Class)
%   as feature_cvterm/3, untyped
feature_cvterm(ID,Class):-
        feature_cvterm(ID,_,Class).

%% feature_equivalent(?F1,?F2)
%  
feature_equivalent(F1,F2):-
        % this has some duplicate calculations..
        feature_equivalent_asym(F1,F2),
        feature_equivalent_asym(F2,F1).
feature_equivalent_asym(F1,F2):-
        feature(F1),
        feature(F2),
        \+ \+ featureloc(F1,_,_,_,_), % at least one loc
        forall(featureloc(F1,S,B,E,D,R,G),
               featureloc(F2,S,B,E,D,R,G)).

infer_feature_relationship_order(Su,Ob,Type,Rank):-
        setof(Su1,feature_relationship(Su1,Ob,Type),Sus),
        order_features(Sus,OrderedSus),
        pos_in_list(Su,OrderedSus,0,Rank).

%% order_features(+Fs,?OFs)
%   as order_features/2, using default (zero) rank and group
%  
order_features(Fs,OFs):-
        order_features(Fs,OFs,0,0).

%% order_features(+Fs,?OFs,?Rank,?Group)
%  @param Fs
%  list of Features
%  @param OFs
%  ordered list of Features
%  @param Rank
%  featureloc rank (zero or one, typically zero)
%  @param Rank
%  featureloc group (typically zero for direct localization)
%   must be on the same strand, and located relative to the same srcfeature
%  
order_features(Fs,OFs,Rank,Group):-
        % must all be on same strand and same srcfeature
        setof(S-Dir,F^Beg^End^(member(F,Fs),
                               featureloc(F,S,Beg,End,Dir,Rank,Group)),[_]),
        solutions(Ord-F,
                  (   member(F,Fs),
                      featureloc(F,S,Beg,_,Dir),
                      Ord is Beg*Dir),
                  Ps),
        sort(Ps,OPs),
        findall(F,member(_-F,OPs),OFs).

pos_in_list(X,[X|_],N,N):- !.
pos_in_list(X,[_|Xs],N1,N):-
        N2 is N1+1,
        pos_in_list(X,Xs,N2,N).


%feature(F):- feature(F,_,_).
feature(F,N):- feature(F,N,_).



%% feature_by_type_id(+QueryTypeID,?ID,?TypeID)
%  nd
%
%  finds features belonging to a certain type (possibly by transitivity)
%
%  requires an ontology of types to be loaded into the in-memory db
%
%  
feature_by_type_id(QTypeID,ID,TypeID):-
        subclassRT(TypeID,QTypeID),
        class(TypeID,TypeName),
        feature(ID,_,TypeName).

%% feature_by_typename(+QueryTypeName,?ID,?TypeID)
%  nd
%
%  finds features belonging to a certain type (possibly by transitivity)
%
%  for example, =|feature_by_typename(transcript,ID,TypeID)|= will unify with feature IDs of type mRNA
%
%  requires an ontology of types to be loaded into the in-memory db
%  
%  
feature_by_typename(QTypeName,ID,TypeID):-
        class(QTypeID,QTypeName),
        feature_by_type_id(QTypeID,ID,TypeID).


gffstrand_number('.',0).
gffstrand_number('+',1).
gffstrand_number('-',-1).

%% map_coordsystem(+RangeIn,?RangeOut)
% Range = gff(S,E,D) | interbase(Min,Max,Dir) | directional_interbase(B,E,Dir)
map_coordsystem(gff(S1,E,Str),interbase(S0,E,Dir)):-
        gffstrand_number(Str,Dir),
        S0 is S1-1.
map_coordsystem(gff(S,E,Str),directional_interbase(S2,E2,Dir)):-
        gffstrand_number(Str,Dir),
        (   Dir < 0
        ->  E2 is S-1,
            S2 = E
        ;   S2 is S-1,
            E2=E).


% todo: move somehwere
user:show_db_summary(feature):-
        (setof(T,ID^N^feature(ID,N,T),Ts)
        ->  true
        ;   Ts=[]),
        writeln(types=Ts),
        forall(member(T,Ts),
               show_count(T,ID,N^feature(ID,N,T))),
        show_count(features,ID,N^T^feature(ID,N,T)),
        show_count(featureprops,ID/T/Val,featureprop(ID,T,Val)),
        show_count(feature_residues,ID,Res^feature_residues(ID,Res)),
        show_count(feature_seqlen,ID,SL^feature_seqlen(ID,SL)),
        show_count(feature_relationships,Sbj/Obj,T^feature_relationship(Sbj,Obj,T)).

show_count(Item,Var,Call):-
        (setof(Var,Call,Vars)
        ->  true
        ;   Vars=[]),
        length(Vars,Count),
        writeln(count(Item,Count)).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(sofa)=
      load_biofile(obo,'sofa.obo')/[]).
unittest(load(rab)=
      load_biofile(chaos,'Rab1.chaos-xml')/[]).

unittest(test(revcomp,
            [_=load(sofa),
             _=load(rab)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(bioseq)),
                forall(feature_by_typename(transcript,ID,_),
                       (   concat_subfeature_residues(ID,exon,DnaSeq),
                           revcomp(DnaSeq,RevSeq),
                           feature(ID,Name,_),
                           format('transcript: ~w~n  seq: ~w~n  rev:~w~n~n',[Name,DnaSeq,RevSeq])))),
            true)).
/** <module> genomic sequence features
  
  ---+ Synopsis
  
  ==
  :- use_module(bio(seqfeature_db)).
  :- use_module(bio(io)).

  % find all exons
  demo:-
    load_biofile(chaos,'Rab1.chaos-xml'),
    forall((   feature(ID,_,exon),
               featureloc(ID,Src,Beg,End,Strand)),
            format('Exon on ~w:~w..~w[~w]~n',
                   [Src,Beg,End,Strand])).
  
  ==
  
  ---+ Description

  This is a data module that defines extensional and intensional
predicates representing sequence features. The datamodel is chado-like
(but simpler)

  ---++ Related modules

  predicates such as revcomp/2 in bioseq and intersects/2 in
range may be useful
  
  ---++ Sequence Feature Types

  seqfeatures can be typed using an ontology, ie the Sequence Ontology
(SO). Use the ontol module to query SO
  
  ==
  % demonstrate use of seqfeature module in conjunction with
  % an ontology of sequence feature classes
  :- use_module(bio(seqfeature_db)).
  :- use_module(bio(io)).
  :- use_module(bio(ontol_db),[class/2,subclassRT/2]).
  :- use_module(bio(bioseq),[revcomp/2]).

  % reverse complement a transcript sequence
  % if ID is not-ground, then will unify with some transcript ID
  % in the database
  revcomp_transcript(ID,Name,RevSeq):-
    feature_by_typename(transcript,ID,_),
    feature(ID,Name,_),
    feature_residues(ID,DnaSeq),
    revcomp(DnaSeq,RevSeq).

  demo:-
    load_biofile(chaos,'Rab1.chaos-xml'),
    load_bioresource(so),         % load sequence ontology
    forall(revcomp_transcript(ID,Name,Seq),
           format('transcript ~w ~w revcomp(dnaseq) = ~w~n',[ID,Name,Seq])).
  ==

  ---++ Importing and exporting data

  This is a data module. Facts can be imported and exported from both
prolog fact databases and other formats
  
  ---+++ Import

  The following file formats can be read in using load_biofile/2
and load_bioresource/1

  
  * chaos
  

  The following database schemas have nascent adapters:

  
  * chadosql
  * enscoresql
  
  
  ---+++ Export

  The following file formats can be written using write_biofile/2

  
  * OWL - (ensure_loaded seqfeature_bridge_to_inst)
  

  */
