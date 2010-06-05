/* -*- Mode: Prolog -*- */


:- module(io_gff,
          [gffrow/2,
           load_gff/1,
           query_feature/2
          ]).
:- use_module(bio(range)).
:- use_module(bio(bioprolog_util),[load_factfile/1]).


test(ID1/ID2):-
        query_feature([type='CDS',seq_id=S,range=R2],ID2),
        writeln(id2=ID2),
        query_feature([type='5_UTR',seq_id=S,range=R1],ID1),
        writeln(id1=ID1),
        intersects(R1,R2).

%% query_feature(+Constraints,?ID) nd
%   fetches ID of a GFF feature based on query constraints
%
%  The constraint can either be a constraint term or list of constraint
%  terms; a constraint term typically uses the =/2 functor
%
%  constraints can be variables; this allows the constraints to act as
%  a query
%  
%  Constraint terms:
%
%  type=TypeString 
%  seq_id=Seq
%  range=Range
%
%  ranges are interbase range terms (see range module)
query_feature(CL,ID):-
        gffrow(ID,Gff),
        test_match(CL,Gff).

% +C,+GffRow sd
% checks a constraint or constraint list vs a gff term
test_match([],_).
test_match([C|CL],D):-
        test_match(C,D),
        test_match(CL,D).
test_match(type=T,gffrow(_,_,T,_,_,_,_,_,_)).
test_match(seq_id=S,gffrow(S,_,_,_,_,_,_,_,_)).
test_match(range=R,gffrow(_,_,_,Min,Max,_,Str,_,_)):-
        range_convert(gffrange(Min,Max,Str),R).

%% gffrow(?ID,?GffRow) nd
%   fetches a gff line from the in-memory db
%
%  GffRow = gffrow(S,Src,T,Min,Max,F,Str,Sc,group(ID,PL,Gr))
%
%  one ID may have multiple rows (split locations)
%  
gffrow(ID,gffrow(S,Src,T,Min,Max,F,Str,Sc,group(ID,PL,Gr))):-
        gffrow(S,Src,T,Min,Max,F,Str,Sc,group(ID,PL,Gr)).


%% gff_feature(?ID,?Feature) nd
%% gff_feature(+ID,?Feature) sd
%   fetches a gff line from the in-memory db
%
%  Feature = gfffeat(S,Src,Type,group(ID,PL,Gr),LocL)
%
%  this predicate joins GFF splitlocations into a single feature;
%  thus every valid ID should unify with one feature
%
%  SLOW: TODO make efficient
gff_feature(ID,gfffeat(S,Src,T,group(ID,PL,Gr),LocL):-
        setof(Loc,gff_locpart(ID,Loc),LocL),
        gffrow(S,Src,T,_,_,_,_,_,group(ID,PL,Gr)).

% this is the feature-varying part of a gff line
gff_locpart(ID,gffloc(Min,Max,F,Str,Sc)):-
        gffrow(_,_,_,Min,Max,F,Str,Sc,group(ID,_,_)).

                      
%% range_convert(+GffRange,?Range) sd
%   converts gffrange(Min,Max,Strand) to interbase range(S,E)
%  as used by the range module
%
%  GffRange can be a list of ranges (in which case it is converted to
%  a rangeset)
%  
range_convert(LocL,rangeset(RL)):-
        list(LocL),
        setof(R,(member(Loc,LocL),range_convert(Loc,R)),RL).
range_convert(gffrange(Min1,Max,Str),range(X,Y)):-
        Min is Min1-1,
        (Str='-' ->
            X=Max,
            Y=Min
        ;
            X=Min,
            Y=Max
        ).

%% load_gff(+GffFile)
%   loads a gff file into the in-memory database
%
%  a little hacky right now - calls a perlscript gff2p to convert
%  gff file to a prolog fact format, the loads that
%  
load_biofile(gff,GffFile) :-
	file_name_extension(Base, _Ext, GffFile),
	file_name_extension(Base, pro, PrologFile),
	(   exists_file(PrologFile),
	    time_file(PrologFile, PrologTime),
	    time_file(GffFile, GffTime),
	    PrologTime >= GffTime
	->  load_factfile(PrologFile)
	;   access_file(PrologFile, write)
	->  convert_and_load(gff,GffFile,PrologFile)
	;   throw(cannot_write_to(PrologFile))
        ).

% todo: allow custom directory
convert_and_load(gff,GffFile,PrologFile):-
        (   access_file(PrologFile, write)
        ->  true
        ;   throw(cannot_write_to(PrologFile))),
        concat_atom([gff2p,GffFile,'>',PrologFile],' ',Cmd),
        (   shell(Cmd,0)
        ->  true
        ;   throw(problem_executing(Cmd))),
        load_factfile(PrologFile).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/05/14 19:04:05 $
  @license LGPL


  ==
  :- use_module(bio(io_gff)).
  :- load_gff('chr1.gff').
  intersecting_exon_pair(F1,F2):-
        query_feature([type=exon,seq_id=S,range=R2],ID2),
        query_feature([type=exon,seq_id=S,range=R1],ID1),
        ID1 \= ID2,
        intersects(R1,R2).
  ==
  
  ---+

  input/output module for GFF files

  relies on gff2p perl script in bin

  other than that, this module is relatively simple and
  self-contained. the range module is required for range-based queries

  TODO: deal with GFF split ranges
  
  
  */