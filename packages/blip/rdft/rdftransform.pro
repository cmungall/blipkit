/* -*- Mode: Prolog -*- */



:- module(rdftransform,
          []).

:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(library(sgml)).


%**********************************************************
%  Main
%***********************************************************
rdftransform(TransformDoc,RdfDoc):-
        Opts = [dialect(xmlns),space(remove)],
        load_structure(TransformDoc,Tr,Opts),
        rdf_load(RdfDoc,[namespaces(NSList)]),
        forall((member(NS=Full,NSList),NS\=[]),
               catch(rdf_register_ns(NS,Full),_E,true)),
        apply_templates1(Tr,'/'). % 'root' ID


%**********************************************************
%  Processing
%***********************************************************
apply_templates(Tr,Objs):-
        maplist(apply_templates1(Tr),Objs).
apply_templates1(Tr,Obj):-
        Tr=elt(_,_,Nodes),
        rdft_ns(NS),
        member(elt(NS:template,Atts,SubNodes),Nodes),
        template_match_by_atts(Obj,Atts), % todo - nomatch
        reduce_elements(Tr,Nodes,Obj).

%% reduce_element(+Transform,+CurrentNode,+Resource) is det
% processes CurrentNode
% CurrentNode may be a command, or an element
% Resource may be a list of URI or a list or URIs
reduce_element(Tr,Node,Objs):-
        list(Objs),
        !,
        maplist(reduce_element(Tr,Node),Objs). % correct semantics? yes for for-each
                                % constraint: Obj is non-list from here on
reduce_element(Tr,elt(NS:'apply-templates',Atts,Nodes),Obj):-
        rdft_ns(NS),
        !,
        spo_select(Atts,Obj,Results,_Atts2),
        apply_templates(Tr,Results),
        reduce_elements(Tr,Nodes,Results). % Nodes is usually []
reduce_element(Tr,elt(NS:'for-each',Atts,Nodes),Obj):-
        rdft_ns(NS),
        !,
        spo_select(Atts,Obj,Results,_Atts2),
        reduce_elements(Tr,Nodes,Results). % one Result at a time
reduce_element(Tr,elt(NS:choose,Atts,Nodes),Obj):-
        rdft_ns(NS),
        !,
        process_chooses(Tr,Nodes,Obj).
reduce_element(Tr,elt(NS:text,_Atts,Nodes),Obj):-
        rdft_ns(NS),
        !,
        export_text(Nodes).
reduce_element(Tr,Node,Obj):-
        rdft_ns(NS),
        Node=elt(NS:Name,_,_),
        !,
        throw(error(rdft_command_unknown(Name,in(Node)))).
reduce_element(Tr,Node,Obj):-
        Node=elt(Name,Atts,Nodes),
        export_open(Name,Atts),
        reduce_elements(Tr,Nodes,Obj),
        export_close(Name).

% list form
reduce_elements(_,[],_).
reduce_elements(Tr,[Node|Nodes],Obj):-
        reduce_element(Tr,Node,Obj),
        reduce_elements(Tr,Nodes,Obj).

% in <rs:choose>
process_choose(Tr,elt(NS:otherwise,_Atts,Nodes),Obj):-
        rdft_ns(NS),
        !,
        process_resource(Tr,Nodes,Obj).        
process_choose(Tr,elt(NS:when,_Atts,Nodes),Obj):-
        rdft_ns(NS),
        apply_test(Atts,Obj),
        !,
        process_resource(Tr,Nodes,Obj).

% find select="Q" in attributes and
% hand to query processor
spo_select(Atts,Obj,Results,Atts2):-        
        select(select=Sel,Atts,Atts2),
        spo_query(Sel,Obj,Results).

%**********************************************************
%  I/O
%***********************************************************
export_text(Data):-
        list(Data),
        !,
        maplist(export_text,Data).
export_text(Data):-
        write(Data).
export_open(Name,[]):-
        !,
        format('<~w>',[Name]).
export_open(Name,Atts):-
        !,
        format('<~w ',[Name]),
        maplist(export_att,Atts),
        format('>').
export_att(Att=Val):-
        % TODO - varsubstr
        xml_quote_cdata(Val,ValEsc),
        format(' ~w="~w"',[Att,ValEsc]).
export_close(Name):-
        format('</~w>~n',[Name]).

%**********************************************************
%  Queries
%***********************************************************
/*

  * wildcard
  . this object; or object(), $object
  subject()
  
  sel="{*} * {*}" - all triples

  sel="rdf:type"     - implicit {.}
  sel="rdf:type{*}"
  sel="{.}rdf:type{*}"
  
  sel="*" - equiv to
  sel="* {*}"

  sel="{*} rdf:type {.}"
  sel="{*} * {.}"

  sel="SELECT {X} FROM ..."

  */


spo_query(Q,Res,Objs):-
        findall(Obj,spo_query1(Q,Res,Obj),Objs).
spo_query1(Q,Res,Obj):-
        spo_parse(Q,Parts),
        spo_queryx(Parts,Res,Obj).

% queries:
% eg 'rdf:type'
spo_queryx([Rel],Subj,Obj):-
        rdf_reachable(Subj,Rel,Obj).
% eg './rdf:type'
spo_queryx(['.',Rel],Subj,Obj):-
        rdf_reachable(Subj,Rel,Obj).
% eg './rdf:type/*'
spo_queryx(['.',Rel,'*'],Subj,Obj):-
        rdf_reachable(Subj,Rel,Obj).
% eg '*/ro:part_of/.'
spo_queryx(['*',Rel,'.'],Obj,Subj):-
        rdf_reachable(Subj,Rel,Obj).
% 
spo_queryx(['*'],Subj,Rel-Obj):-
        rdf_reachable(Subj,Rel,Obj).

spo_parse(Q,Parts):-
        atom_codes(Q,Codes),
        spo(Codes,Parts,[]).

spo(Parts) --> spo_subj(S),spo_pre(P),spo_obj(O).
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ rdftransform
- 

  ---+ Synopsis

  ==
  :- use_module(bio(rdftransform)).

  ==

  ---+ Description

  http://www.semanticplanet.com/2003/08/rdft/spec
  
**/