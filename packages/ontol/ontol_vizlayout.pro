:- module(ontol_vizlayout,
          [
           graph_to_semantic_svg_term/2,
           graph_to_semantic_svg_xml/2,
           load_viz_constraints/1
          ]).

/**

  ---+ Name
  ---++ ontol_vizlayout
- constraint-based SVG layout for ontology graphs

  ---+ Synopsis

  Pass in constraints using a file with contents such as:
  ==
:- op(1100,xfy,::).
:- op(800,xfy,in).

develops_from :: lower(subject) > lower(object) in x. % horizontal, R to L

preceded_by :: lower(subject) = lower(object) in y.      % same height
preceded_by :: upper(subject) = upper(object) in y.      % same height
preceded_by :: lower(subject) > upper(object) in x.  % earlier event left-of right, not overlapping

% we can't force same height due to mixed levels in precedes..
% but how about making sure predecessor always contained by
'OBO_REL:precedes' :: lower(subject) >= lower(object) in y.      % 
'OBO_REL:precedes' :: upper(subject) =< upper(object) in y.      % same height
'OBO_REL:precedes' :: lower(subject) > upper(object) in x.  % earlier event left-of right, not overlapping

part_of :: lower(subject) < lower(object) in y. % arrange vertically, sub-parts lower
part_of :: lower(subject) >= lower(object) in x.
part_of :: upper(subject) =< upper(object) in x. % sub-part horizontally contained by super-part

start :: lower(subject) = lower(object) in x.  % vertically aligned

end :: upper(subject) = upper(object) in x.  % vertically aligned
  ==

  
  
  */

:- use_module(library('clp/bounds')).
:- use_module(library(sgml_write)).
:- use_module(bio(svg)).
:- use_module(bio(graph)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(xml_transform)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- op(1100,xfy,::).
:- op(800,xfy,in).
:- multifile (::)/2.

:- multifile allow_overlap/0.
:- multifile allow_overlap/1.

/*

  Data structures:

  Loc = loc(Node,XL-XU,YL-YU)
  
  The following are constraint vars:
  XL = X position, lower bound
  XU = X position, upper bound
  YL = Y position, lower bound
  YU = Y position, upper bound

  Input graph:
  
  Edge = edge(Relation,SubjectNode,TargetNode)

  Constraints:

  Facts of the form:

  Rel :: Role1 Op Role2 in XorY

  e.g.:
  
  develops_from :: lower(subject) > lower(object) in x. % horizontal, R to L
  

  
*/

load_viz_constraints(S):-
        debug(viz,'loading_constraints: ~w',[S]),
        consult(S).

% TEMPORARY HACK: TODO!!!
normalize_edge(type_parent(R),R):- !.
normalize_edge(relation_link(R),R):- !.
normalize_edge(R,R):- !.

%% constrain_by_edges(+Locs,+Edges)
% give a list of Loc constraints and a list of input edges, constrain locs further
constrain_by_edges(_,[]).
constrain_by_edges(Locs,[Edge|Edges]):-
        constrain_by_edge(Locs,Edge),
        constrain_by_edges(Locs,Edges).

%% constrain_by_edge(+Locs,+Edge)
constrain_by_edge(Locs,Edge):-
        debug(viz,'edge: ~w',[Edge]),
        Edge=..[edge,Subj,Obj,RX],
        normalize_edge(RX,R),
        constrain_by_edge(R,Subj,Obj,Locs).

%% constrain_by_edge(+R,+Subj,+Obj,+Locs)
% constraints are facts of the form: Rel :: Role1 Op Role2 in XorY
constrain_by_edge(R,Subj,Obj,Locs):-
        findall(Constraint, R :: Constraint, Constraints),
        debug(viz,'constraints for ~w : ~w',[R,Constraints]),
        add_constraints(Constraints,R,Subj,Obj,Locs).

%% add_constraints(+Constraints,+R,+Subj,+Obj,+Locs)
% adds a constraint for each C in Constraints
add_constraints([],_,_,_,_).
add_constraints([C|Cs],R,Subj,Obj,Locs):-
        debug(viz,'adding constraint: ~w',[C]),
        call_det(add_constraint(C,R,Subj,Obj,Locs)),
        add_constraints(Cs,R,Subj,Obj,Locs).

%% add_constraint(+Constraint,+Relation,+Subj,+Obj,+Locs)
% constraints are of the form: Role1 Op Role2 in XorY
add_constraint(C in XorY,_,Subj,Obj,Locs):-
        debug(viz,'adding for ~w s=~w o=~w',[C in XorY,Subj,Obj]),
        C=..[Op,SubjTerm,ObjTerm], % e.g. lower(subject) < lower(object)
        % translate to clp form
        mapop(Op,ClpOp), 
        relatum_id_to_var(XorY,subject,SubjTerm,Subj,Locs,SubjVar),
        relatum_id_to_var(XorY,object,ObjTerm,Obj,Locs,ObjVar),
        Clp=..[ClpOp,SubjVar,ObjVar], % make CLP constraint. E.g. SX #< SY
        debug(viz,'Clp constraint: ~w',[Clp]),
        Clp, % state constraint
        debug(viz,'added constraint',[]).

mapop(<,#<).
mapop(=<,#=<).
mapop(>,#>).
mapop(>=,#>=).
mapop(=,#=).

%% relatum_id_to_var(+XorY,+SubjectOrObject,+Role,+Node,+Locs,+Var)
% convert from high level constraint language variable to CLP.
% e.g. if Role=lower(subject) and Node=nodeA, then Locs will contain
% a member loc(nodeA,AXL-AXU,AYL-AYU)
% we select x we get the var AXL
relatum_id_to_var(x,Relatum,lower(Relatum),ID,Locs,P):- member(loc(ID,P-_,_),Locs).
relatum_id_to_var(x,Relatum,upper(Relatum),ID,Locs,P):- member(loc(ID,_-P,_),Locs).
relatum_id_to_var(y,Relatum,lower(Relatum),ID,Locs,P):- member(loc(ID,_,P-_),Locs).
relatum_id_to_var(y,Relatum,upper(Relatum),ID,Locs,P):- member(loc(ID,_,_-P),Locs).

(   parent_over_nr(R,_) :: Constraint ):- (R :: Constraint).
(   parent_over_oneof(R-_) :: Constraint ):- (R :: Constraint).

% ========================================
% LAYOUT
% ========================================

% TODO: make configurable
gridsize(64).
%scale(32).
scale(16).
margin(2).
screen_width(1200).
screen_height(1200).
%boxstyle('fill:yellow;stroke:black;stroke-width:3').
boxstyle('fill:yellow;fill-opacity:0;stroke:black;stroke-width:3').
%boxstyle('stroke:black;stroke-width:3').

svgns('http://www.w3.org/2000/svg').
xlinkns('http://www.w3.org/1999/xlink').
locs2svg(Locs,svg(xmlns=NS,
                  'xmlns:xlink'=XlinkNS,
                  width=Width,
                  height=Height,
                  g([rect(x=0,y=0,width=10,height=10,style='fill:black;stroke:black;stroke-width:1'),rect(x=0,y=0,width=Width,height=Height,style='fill:white;stroke:black;stroke-width:1')|
                    SVGTerms]))):-
        svgns(NS),
        xlinkns(XlinkNS),
        screen_width(Width),
        screen_height(Height),
        maplist(loc2svg,Locs,SVGTerms).

loc2svg(loc(C,XL-XU,YL-YU),a([Text,Rect])):-
        scale(Scale),
        margin(Margin),
        SXL is XL * Scale + Margin,
        SXU is XU * Scale - Margin,
        SYL is YL * Scale + Margin,
        SYU is YU * Scale - Margin,
        entity_label(C,Label),         % todo: generic
        Text=text(x=SXL,y=SYL,Label),
        debug(viz,'Loc: ~w',[loc(C,XL-XU,YL-YU,Text)]),
        Width is SXU-SXL,
        Height is SYU-SYL,
        boxstyle(Style),
        Rect=rect(x=SXL,y=SYL,width=Width,height=Height,style=Style).

% TODO: move so that it can be re-used
term2xml([],[]):-
        !.
term2xml([H|T],[H2|T2]):-
        !,
        term2xml(H,H2),
        maplist(term2xml,T,T2).
term2xml(Term,Term):-
        atom(Term),
        !.
term2xml(Term,element(Name,Atts,Elts)):-
        Term=..[Name,Args],
        is_list(Args),
        !,
        list_atts_terms(Args,Atts,Terms),
        term2xml(Terms,Elts).
term2xml(Term,element(Name,Atts,Elts)):-
        !,
        Term=..[Name|Args],
        list_atts_terms(Args,Atts,Terms),
        term2xml(Terms,Elts).
list_atts_terms([],[],[]).
list_atts_terms([A=V|Args],[A=V|Atts2],Terms):-
        !,
        list_atts_terms(Args,Atts2,Terms).
list_atts_terms([Arg|Args],Atts,[Arg|Terms]):-
        list_atts_terms(Args,Atts,Terms).

% --

%% label_locs(+Locs)
% labelling step - once vars in Locs have been constrained, we label them - i.e.
% find a possible solution
label_locs([]).
label_locs([H|T]):-
        label_loc(H),
        label_locs(T).
label_loc(loc(_,XL-XU,YL-YU)):-
        label([XL,XU,YL,YU]).



%% setrandom(+Locs)
% take the first location and place it in the middle.
% useful as a layout heuristic - less likely to run out of room by placing things on the edge
setrandom(Locs):-
        member(loc(_,XL-_,YL-_),Locs),
        !,
        gridsize(GridSize),
        MidPoint is GridSize / 2,
        XL #= MidPoint,
        YL #= MidPoint.

place_in_middle(loc(_,XL-_,YL-_)):-
        gridsize(GridSize),
        MidPoint is GridSize / 2,
        XL #= MidPoint,
        YL #= MidPoint.

%% graph_to_semantic_svg_term(+Edges,?SVGTerm) nondet
% given Edges, return an SVG term 
graph_to_semantic_svg_term(Edges,SVGTerm):-
        gridsize(GridSize),
        % get all unique nodes from list of edges. These will also be nodes in the display graph.
        solutions(Node,(member(edge(_,Node,_),Edges);member(edge(Node,_,_),Edges)),Nodes),

        % create an array Locs = [loc(N,XMin-XMax,YMin-YMax),..]
        % this is initially constrained minimally such that everything fits in grid / window
        findall(loc(Node,XL-XU,YL-YU),(member(Node,Nodes),XL #>0, XU #< GridSize, XL #< XU, YL #>0, YU #< GridSize, YL #< YU),Locs),
        
        %findall(loc(Node,XL-XU,YL-YU),(member(Node,Nodes), XL #< XU, YL #< YU),Locs),
        %findall(loc(Node,XL-XU,YL-YU),(member(Node,Nodes),XL #>0, XL #< XU, YL #>0, YL #< YU),Locs),
        %edgelist_to_trees(Edges,[Tree|_]),
        %Tree=node(_,Root,_),
        %member(RootLoc,Locs),RootLoc=loc(Root,_,_),
        %debug(viz,'placing ~w in middle',[Root-RootLoc]),
        %place_in_middle(RootLoc),
        setrandom(Locs),
        debug(viz,'locs=~w',[Locs]),
        constrain_by_edges(Locs,Edges),
        findall(C1-C2,(member(loc(C1,_,_),Locs),
                       member(loc(C2,_,_),Locs)),CPairs),
        no_overlap_all(CPairs,Locs), % has no effect is allow_overlap/0 is asserted. TODO: less weird config
        label_locs(Locs),
        locs2svg(Locs,SVGTerm).

%% graph_to_semantic_svg_xml(+Edges,?XMLNode)
% 
graph_to_semantic_svg_xml(Edges,XMLNode):-
        graph_to_semantic_svg_term(Edges,SVGTerm),
        term2xml(SVGTerm,XMLNode).


%% no_overlap_all(+NodePairs,+Locs)
% ensure that the results contain no overlapping boxes,
% unless allow_overlap/0 is asserted
no_overlap_all([],_).
no_overlap_all([C1-C2|T],Locs):-
        L1=loc(C1,_,_),
        L2=loc(C2,_,_),
        member(L1,Locs),
        member(L2,Locs),
        no_overlap(L1,L2),
        no_overlap_all(T,Locs).

no_overlap(_,_):- allow_overlap,!.
% config: if allow_overlap(Rel) is set and X Rel Y then we allow X and Y to overlap
% if they stand in a transitive relation Rel to each other.
% this is the only dep on the ontol module: make generic? TODO
no_overlap(loc(C1,_,_),loc(C2,_,_)):- allow_overlap(R),parent_overT(R,C1,C2),!.
no_overlap(loc(C1,_,_),loc(C2,_,_)):- allow_overlap(R),parent_overT(R,C2,C1),!.

% identical nodes are allowed to overlap!
no_overlap(loc(C1,_,_),loc(C2,_,_)):- C1 = C2,!.
%no_overlap(loc(C1,_,_),loc(C2,_,_)):- C1 @=< C2,!.

% if we reach this point without failing, we should assert the no overlap constraint..
no_overlap(loc(_,XL1-XU1,YL1-YU1),loc(_,XL2-XU2,YL2-YU2)):-
        (   XU1 #> XL2 #/\
            YU1 #> YL2 #/\
            XL1 #< XU2 #/\
            YL1 #< YU2 #=>
        1#=0).

% TEST

svg:-
        test(Locs),
        !,
        locs2svg(Locs,SVGTerm),
        writeq(SVGTerm),nl,
        %svgterm_xmlnode(SVGTerm,XMLNode),
        term2xml(SVGTerm,XMLNode),
        debug(viz,'~w',XMLNode),
        open('foo.svg',write,OutStream,[]),
        sgml_write(OutStream,XMLNode,[]),
        close(OutStream).

test(Locs):-
        soln(Locs),
        debug(viz,'~w',s=Locs),
        label_locs(Locs).

soln(Locs):-
        findall(loc(C,XL-XU,YL-YU),(c(C),XL #>0, XU #< 16, XL #< XU, YL #>0, YU #<16, YL #< YU),Locs),
        findall(edge(R,A,B),r(R,A,B),Edges),
        debug(viz,'~w',Locs),
        debug(viz,'~w',Locs),
        %maplist(constrain_by_edge(Locs),Edges),
        constrain_by_edges(Locs,Edges),
        debug(viz,'~w',locs2=Locs),
        findall(C1-C2,(member(loc(C1,_,_),Locs),
                       member(loc(C2,_,_),Locs)),CPairs),
        debug(viz,'~w',locs3=Locs),
        no_overlap_all(CPairs,Locs).

