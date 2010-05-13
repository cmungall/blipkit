/* -*- Mode: Prolog -*- */


:- module(range,
          [
           point_overlaps/2,
           intersects/2,
           range_orientation/2,
           relative_range/3,
           project_point/3,
           reverse_project_point/3,
           range_min/2,
           range_max/2,
           range_beg/2,
           range_end/2
          ]).

:- use_module(bio(bioprolog_util)).

point_between(P,X,Y):-
        (P > X, P < Y),!
        ;
        (P > Y, P < X).

%% point_overlaps(+Position:int,+Beg,+End) is semidet
% @param Position (interbase number) 
% @param Beg upstream coord
% @param End downstream coord
point_overlaps(P,X,Y):- P >= X, P =< Y, !.
point_overlaps(P,X,Y):- P >= Y, P =< X, !.

%% point_overlaps(+Position:int,+Range) is semidet
% @param Position (interbase number)
% @param Range
point_overlaps(P,R):-
        range_beg(R,X),
        range_end(R,Y),
        point_overlaps(P,X,Y).        

%% intersects(+Range1,+Range2) is semidet
%
%  succeeds if Range1 and Range2 intersect
%  [ignores strand]
%  if Ranges are relative to a sequence, checks to make sure sequence is the same
%
% @param Range1 range(From,To)
% @param Range2 range(From,To)
intersects(range(X1,Y1),range(X2,Y2)):-
        point_between(X1,X2,Y2),!
        ;
        point_between(Y1,X2,Y2),!
        ;
        point_between(X2,X1,Y1).
intersects(range(X1,Y1),rangeset([H|L])):-
        intersects(range(X1,Y1),H),!
        ;
        intersects(range(X1,Y1),rangeset(L)).
intersects(rangeset([H|L]),rangeset(L2)):-
        intersects(H,rangeset(L2)),!
        ;
        intersects(rangeset(L),rangeset(L2)).
intersects(range(S,X1,Y1,_),range(S,X2,Y2,_)):-
        intersects(range(X1,Y1),range(X2,Y2)).
        
%intersects(interval(Lo1,Hi1),interval(Lo1,Hi1))


%% range_orientation(+Range,?Ori:int)
%  @param Range range(From,To) | rangeset([Range,...])
%   mixed orientation ranges are unified with 0
%  @param Ori -1,1, or 0 for reverse, forward or undetermined
range_orientation(range(X,Y),Z):-
        sign(Y-X,Z).
range_orientation(range(_,_,_,D),D).
range_orientation(rangeset(Ranges),Z):-
        setof(Z1,(member(Range,Ranges),range_orientation(Range,Z1)),Zs),
        collapse_orientation(Zs,Z).

%% range_min(+Range,?MinCoord:int)
% @param Range see above
% @param MinCoord leftmost point in range or rangeset, when orientation is not considered
range_min(range(X,Y),M):-
        M is min(X,Y).
range_min(range(_,X,Y,_),M):-
        M is min(X,Y).
range_min(rangeset(Ranges),M):-
        setof(M1,(member(Range,Ranges),range_min(Range,M1)),Ms),
        list_min(Ms,M).

%% range_max(+Range,?MaxCoord:int)
% @param Range see above
% @param MaxCoord rightmost point in range or rangeset, when orientation is not considered
range_max(range(X,Y),M):-
        M is max(X,Y).
range_max(range(_,X,Y,_),M):-
        M is max(X,Y).
range_max(rangeset(Ranges),M):-
        setof(M1,(member(Range,Ranges),range_max(Range,M1)),Ms),
        list_max(Ms,M).

%% range_beg(+Range,?BegCoord:int)
% @param Range see above
% @param BegCoord upstream-most point in range or rangeset (orientation considered)
range_beg(range(X,_),X).
range_beg(range(_,X,_,_),X).

%% range_end(+Range,?EndCoord:int)
% @param Range see above
% @param EndCoord downstream-most point in range or rangeset (orientation considered)
range_end(range(_,Y),Y).
range_end(range(_,_,Y,_),Y).

%% range_boundary(+Range,?RangeBoundary)
%
% unifies RangeBoundary with the minimum interval which covers Range
%
%  @param Range see above
%  @param RangeBoundary range(Min,Max)
range_boundary(R,range(X,Y)):-
        range_min(R,X),
        range_max(R,Y).

% will flip a minmax range if negatively oriented
orient_range(range(X,Y),-1,range(Y,X)):- !.
orient_range(range(S,X,Y,D),-1,range(S,Y,X,D2)):-
        !,
        D2 is -D.
orient_range(R,_,R).

%% range_flatten(+Range,?RangeFlat)
%  @param RangeBoundary
%  range(Beg,End)
%  
%  collapses a range or set of ranges into one single range
%  
range_flatten(R,R2):-
        range_min(R,X),
        range_max(R,Y),
        range_orientation(R,Z),
        orient_range(range(X,Y),Z,R2).

% 
collapse_orientation(Zs,1):-
        member(1,Zs),
        \+member(-1,Zs),
        !.
collapse_orientation(Zs,-1):-
        member(-1,Zs),
        \+member(1,Zs),
        !.
collapse_orientation(_,0). % mixed also collapses to 0

%% relative_range(+RangeIn,+RangeRelativeTo,?RelativeRange)
%   coordinate transformation
%  
relative_range(range(S,Xi,Yi,Di),range(S,Xt,_Yt,-1),range(S,Xr,Yr,Dr)):-
        Xr is Xt-Xi,            % dist from nbeg, reverse
        Yr is Xt-Yi,            % dist from nbeg, reverse
        Dr is -Di.
relative_range(range(S,Xi,Yi,Di),range(S,Xt,_Yt,1),range(S,Xr,Yr,Dr)):-
        Xr is Xi-Xt,            % dist from nbeg, fwd
        Yr is Yi-Xt,            % dist from nbeg, fwd
        Dr=Di.

%% project_point(+Point:int,+Range,?TransformedPoint:int) is det
%   coordinate transformation
%  
project_point(P,range(_,Xt,_Yt,-1),Pr):-
        Pr is Xt-P.            % dist from nbeg, reverse
project_point(P,range(_,Xt,_Yt,1),Pr):-
        Pr is P-Xt.             % dist from nbeg, fwd

%% reverse_project_point(+ProjectedPoint:int,+Range,+Point:int) is det
% reverse of project_point/3
% TODO: can we use clp to make this more elegant?
reverse_project_point(Pr,range(_,Xt,_Yt,-1),P):-
        P is Xt-Pr.            % dist from nbeg, reverse
reverse_project_point(Pr,range(_,Xt,_Yt,1),P):-
        P is Pr+Xt.             % dist from nbeg, fwd

/** <module>   operations on linear ranges (intervals)

  ---+ Synopsis

  ==
  use_module(bio(range)).

  intersects(range(10,20),range(15,25)).
  ==

  ---+ Description

  linear range operations

  ---++ Datatypes

  The core term used is a Range structure:
  
  ==
  Range = RangeLeaf | rangeset(Ranges)
  RangeLeaf = range(Beg,End) | range(Src,Beg,End,Strand)
  ==

  range is directional (Beg>End indicates negative orientation) and
interbase (counts from zero; if range is on some unit such as DNA base
pairs, the number indicates the index of the space between the units)


  ---+ TODO

  Check orientation? Other range operations.

lo-hi vs beg-end?

R-trees
  
  */
