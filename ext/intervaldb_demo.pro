
:- use_module(intervaldb).

% http://genome.biowiki.org/genomes/dmel/
% 0..137,500

%% seq_ncl(?Seq,-NCL) is semidet
% maps a biological sequence (e.g. chromosome) to a NCL
% we store both strands in the same NCL. We can post-filter later
:- dynamic seq_ncl/2.

%% load_demo_data
% loads test set - predicates are
% * feature_type/2 -- maps a feature ID to its type (e.g. gene, exon)
% * featureloc/5 -- maps a feature to its start, end and and strand on a sequence
% * subfeature_of/2 -- maps a feature to its container; eg exon to transcript
load_demo_data:-
        [dmeltest].


%% index_features
% populate seq_ncl/2
index_features :-
        % we make one index for every sequence on which features are located
        %
        % in the demo set there is only one seq (2L) but it is good to keep this generalized
        free_all_ncls,
        setof(Seq,F^S^E^Str^featureloc(F,Seq,S,E,Str),Seqs),
        forall(member(Seq,Seqs),
               index_features_on_seq(Seq)).

free_all_ncls :-
        forall(seq_ncl(Seq,NCL),
               (   intervaldb_free(NCL),
                   retract(seq_ncl(Seq,NCL)))).

%% index_features_on_seq(+Seq) is det
% give a Sequence (e.g. dmel chromosome 2L), find all features with a featureloc on that sequence,
% and index the corresponding intervals.
% the resulting index is stored in seq_ncl/2
index_features_on_seq(Seq) :-
        nonvar(Seq), % force deterministic
        setof(S-E,F^Str^featureloc(F,Seq,S,E,Str),Intervals),
        spandb_make(SDB),
        % NOTE: these low level details should be abstracted away
        set_x(0),
        forall(member(S-E,Intervals),
               (   inc_x, get_x(X), format('~w ~w ~w ',[X,S,E]),
                   (   (   integer(S), integer(E)) ; throw('Non Integer')),
                   spandb_addone(SDB,S,E,0,0,0))),
        spandb_to_intervaldb(SDB,IDB),
        nclist_build(IDB,NCL),
        assert(seq_ncl(Seq,NCL)).

%% range_query(+Seq,+S,+E,?F) is nondet
% true if F is a feature with a featureloc on Seq, between S..E
range_query(Seq,S,E,F) :-
        range_type_query(Seq,S,E,_,F).

%% range_type_query(+Seq,+S,+E,?Type,?F) is nondet
% true if F is a feature with a featureloc on Seq, between S..E,
% and F is of type Type.
% if Type is not ground, then this is the same as range_query/4
range_type_query(Seq,S,E,Type,F) :-
        seq_ncl(Seq,NCL),
        nclist_intersection(NCL,S,E,MDB),
        matchdb_feature(Seq,MDB,F),
        feature_type(F,Type).

matchdb_feature(Seq,MDB,F) :-
        intervaldb_match(MDB,S,E),
        featureloc(F,Seq,S,E,_).

%% t
%
% simulate real-world deployment.
% first create an index from some test data (in this case all happen to be featureloc-d to the same
% sequence, 2L, but could be any number of seqs).
% then issue a single query to retrieve all features in a given range
t :-
        load_demo_data,
        index_features,
        forall(range_query('2L',70000,80000,F),
               writeln(F)).
        
       



