:- module(seqfeature_types,
          [
           gene/2,
           transcript/2,
           exon/2,
           translation/2

          ]).

:- use_module(bio(seqfeature_db)).

%% TODO: rank,group; do as opts?
%%featurepair_ranges(ID,range

%% gene(ID,N)
%   a feature of type 'gene'
%  
gene(ID,N):-
        feature(ID,N,gene).
%% transcript(ID,N)
%   a feature of type 'transcript', or any superclass of 'transcript'
%  
transcript(ID,N):-
        feature(ID,N,Type),
        class(TypeID,Type),
        class(SuperTypeID,transcript),
        subclassRT(TypeID,SuperTypeID).
%% exon(ID,N)
%   a feature of type 'exon'
%  
exon(ID,N):-
        feature(ID,N,exon).
%% translation(ID,N)
%   a feature of type 'protein' or 'polypeptide'
%  
translation(ID,N):-
        feature(ID,N,protein)
        ; feature(ID,N,polypeptide).
%% variant(ID,N)
%   a feature of type 'sequence_variant'
%  
variant(ID,N):-
        feature(ID,N,sequence_variant).

% convenience linking tables
% TODO: optimise based on mode
%% gene_transcript(GID,TID)
%   convenience linking predicate: calls feature_relationship/2
%  
gene_transcript(GID,TID):-
        gene(GID,_),
        feature_relationship(GID,TID),
        transcript(TID,_).
%% gene_exon(GID,XID)
%   convenience linking predicate: calls feature_relationship/2 twice ignoring intermediate transcript
%  
gene_exon(GID,XID):-
        gene(GID,_),
        feature_relationship(GID,TID),
        feature_relationship(TID,XID),
        exon(XID,_).
%% gene_translation(GID,XID)
%   convenience linking predicate: calls feature_relationship/2
%  
gene_translation(GID,XID):-
        gene(GID,_),
        feature_relationship(GID,TID),
        feature_relationship(TID,XID),
        translation(XID,_).
%% transcript_exon(TID,XID)
%   convenience linking predicate: calls feature_relationship/2
%  
transcript_exon(TID,XID):-
        transcript(TID,_),
        feature_relationship(TID,XID),
        exon(XID,_).
%% transcript_translation(TID,XID)
%   convenience linking predicate: calls feature_relationship/2
%  
transcript_translation(TID,XID):-
        transcript(TID,_),
        feature_relationship(TID,XID),
        translation(XID,_).
