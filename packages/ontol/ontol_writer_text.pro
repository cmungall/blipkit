/* -*- Mode: Prolog -*- */



:- module(ontol_writer_text,
          [
           writetab/1,
%           write_class/2,
%           write_class/3,
           rcode/3,
           rcode_info/2,
           class_label_by_xp/2,
	   show_dagtree/4
           ]).

:- multifile ontol_writer:write_class/2.

:- use_module(bio(mode)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).

ontol_writer:write_class(textnl,ID,Opts):-
        ontol_writer:write_class(text,ID,Opts),
        nl.

class_label_by_xp(ID,XPLabel):-
        genus(ID,Genus),
        (   class(Genus,GenusName)->true ; GenusName='?'),
        sformat(GenusAtom,'\'~w\'[~w]',[GenusName,Genus]),
        findall(Diff,
                (   differentium(ID,R,To),
                    (   entity_label(To,ToName) -> true ; ToName='?'),
                    (   property(R,RName) -> true ; RName=R),
                    sformat(Diff,'~w ( \'~w\'[~w] )',[RName,ToName,To])),
                Diffs),
        concat_atom(Diffs,' & ',DiffsAtom),
        sformat(XPLabel,'~w that ~w',[GenusAtom,DiffsAtom]).

class_label_by_xp_ref(Ref,XPLabel):-
        differentium(ID,_,Ref),
        genus(ID,Genus),
        (   class(Genus,GenusName)->true ; GenusName='?'),
        sformat(GenusAtom,'"~w"[~w]',[GenusName,Genus]),
        findall(Diff,
                (   differentium(ID,R,To),
                    (   entity_label(To,ToName) -> true ; ToName='?'),
                    (   property(R,RName) -> true ; RName=R),
                    sformat(Diff,'~w ( "~w"[~w] )',[RName,ToName,To])),
                Diffs),
        concat_atom(Diffs,' & ',DiffsAtom),
        sformat(XPLabel,'~w that ~w',[GenusAtom,DiffsAtom]).



% todo: change to use DCG
ontol_writer:write_class(text,ID,Opts):-
        (   member(prefix(T),Opts)
        ->  true
        ;   T=''),
        (   entity_label(ID,N)
        ->  true
        ;   N='?'),
        rcode(T,Code,Via),
        (   member(hideid(1),Opts)
        ->  format('~w ~w',[Code,N])
        ;   format('~w ~w ! ~w',[Code,ID,N])),
        (   entity_obsolete(ID,_),!, format(' **OBSOLETE** ') ; true),
        (   member(focus(FocusIDs),Opts),
            member(ID,FocusIDs)->  write(' *** ')
        ;   true),
        (   member(with_synonyms(1),Opts)
        ->  forall(entity_synonym_scope(ID,Syn,SynType),
                   format(' [SYNONYM: "~w" (~w)]',[Syn,SynType]))
        ;   true),
        (   member(showsubsets(1),Opts)
        ->  forall(entity_partition(ID,Subset),
                   format(' [Subset: ~w]',[Subset]))
        ;   true),
        (   member(ontolmap(OntolMap),Opts)
        ->  forall((entity_resource(ID,Ont),member(Ont-OntCode,OntolMap),!),
                   format(' [~w]',[OntCode]))
        ;   true),
        (   member(subsets(Subsets),Opts)
        ->  forall((member(Subset,Subsets),entity_partition(ID,Subset)),
                   format(' [Subset: ~w]',[Subset]))
        ;   true),
        (   member(showxrefs(1),Opts)
        ->  forall(entity_xref(ID,X),
                   (   class(X,XN)
                   ->  format(' [xref: ~w "~w"]',[X,XN])
                   ;   format(' [xref: ~w]',[X])))
        ;   true),
        (   member(showinvxrefs(1),Opts)
        ->  forall(entity_xref(X,ID),
                   (   class(X,XN)
                   ->  format(' [invxref: ~w "~w"]',[X,XN])
                   ;   format(' [invxref: ~w]',[X])))
        ;   true),
        (   member(showisa(1),Opts),
            forall(   (subclass(ID,PID),class(PID,PN)),
                      format(' [is_a: "~w"]',PN))
        ;   true),

        (   member(showrels(ShowRels),Opts)
        ->  (   member(inverses,ShowRels)
            ->  forall_distinct(   (parent(PID,R,ID),class(PID,PN)),
                                   format(' [inv(~w): "~w"]',[R,PN]))
            ;   true),
            (   member(all,ShowRels)
            ->  forall_distinct(   (parent(ID,R,PID),(class(PID,PN);PN=PID)),
                               format(' [~w: "~w"]',[R,PN]))
            ;   true),
            forall_distinct(   (parent(ID,R,PID),member(R,ShowRels),class(PID,PN)),
                                   format(' [~w: "~w"]',[R,PN]))
        ;   true),

        % annotation counts (all)
        (   member(showannots(count),Opts)
	->  ensure_loaded(bio(curation_db)),
	    class_annotated_entity_count(ID,Num),
	    format(' [AEC: ~w]',[Num])
        ;   true),
        % list annotations (focus)
        (   member(showannots(focus),Opts),
	    member(focus(FocusIDs),Opts),
	    member(ID,FocusIDs)
	;   member(showannots(all),Opts)
	->  ensure_loaded(bio(curation_db)),
                %solutions(G,curation_statementT(_,G,has_role,ID),Gs),  % required for mapping to sql?
	    solutions(G,curation_statementTI(_,G,_,ID),Gs),
	    forall(member(G,Gs),
		   (   (   entity_label(G,GN)
		       ->  true
		       ;   GN=''),
		       format(' [ann: ~w "~w"]',[G,GN])))
        ;   true),
	(   member(counts(NCPairs),Opts),
            member(Num-ID,NCPairs)
        ->  format(' [~w]',[Num])
        ;   true),
	(   member(showxp(1),Opts),
            class_label_by_xp(ID,XPLabel)
        ->  format(' [XP: ~w]',[XPLabel])
        ;   true),
        (   member(showxprev(1),Opts) % reverse
        ->  forall(class_label_by_xp_ref(ID,XPLabel),
                   format(' [XP: ~w]',[XPLabel]))
        ;   true),
        (   member(showdefs(1),Opts),
            def(ID,Def)
        ->  format(' [DEF: "~w"]',Def)
        ;   true),
        (   member(showcomments(1),Opts),
            class_comment(ID,Cmt)
        ->  format(' [Cmt: "~w"]',Cmt)
        ;   true),
        (   member(showinstances(1),Opts)
        ->  forall(inst_of(Inst,ID),
                   show_instance(Inst,Opts))
        ;   true),
        rcode_info(Via,Info),
        write(Info).

show_class(C):-
        class(C,N),
        !,
        format('~w "~w"',[C,N]).
show_class(C):-
        write(C).

show_curation_statement(A,Opts):-
	ensure_loaded(bio(seqfeature_db)), % move this..
	ensure_loaded(bio(curation_db)), % move this..
	curation_statement(A,G,R,C),
	format('~w ~w ',[G,R]),
	(   ontol_writer:write_class(text,C,Opts)
	->  true
	;   write('??')),
	nl.


show_instance(Inst,_):-
        format('~w ',[Inst]).

writetab(N):-
        writetab(N,' ').

writetab(N,S):-
        (N =< 0
        ->  true
        ;   write(S),
            N2 is N-1,
            writetab(N2,S)).

rcode(X,Y,Info):-
        rcode1(X,Y,Info),!.
rcode1(relation_link(T),C,''):- rcode(T,C).
rcode1(type_parent(T),C,''):- rcode(T,C). % deprecated
rcode1(parent_over(T,Via),C,Via):- rcode(T,C).
rcode1(parent_over_nr(T,Via),C,Via):- rcode(T,C).
rcode1(parent_over_oneof(T-_),C,''):- rcode(T,C).
rcode1(T,C,''):- rcode(T,C).
rcode(root,'/').
rcode(subclass,'is_a').
rcode(part_of,'po').
rcode(develops_from,'df').
rcode(ID,N):- entity_label(ID,N).
rcode(X,X).

rcode_info(subclassRT(ID),S):-
        !,
        class(ID,N),
        sformat(S,' {via is_a ~w}',[N]).
rcode_info(parent(R,ID),S):-
        !,
        class(ID,N),
        sformat(S,' {via ~w ~w}',[R,N]).
rcode_info(_,'').

show_dagtree(PID,node(R,ID,L),D,Opts):-
        D2 is D+1,
        (   member(tabchar(Char),Opts)
        ->  writetab(D2,Char)
        ;   writetab(D2)),
        write_class(text,ID,[prefix(R)|Opts]),
        forall((member(showrels(RL),Opts),member(R2,RL),parent(ID,R2,P2),\+P2=PID),
               (write(' ['),write_class(text,P2,[prefix(R2)|Opts]),write(']'))),
        forall((member(showrels(RL),Opts),member(R2,RL),inst_rel(ID,R2,P2),\+P2=PID),
               (write(' ['),write_class(text,P2,[prefix(R2)|Opts]),write(']'))),
        nl,
        forall(member(Node,L),
               show_dagtree(ID,Node,D2,Opts)).

/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ ontol_writer_text
- 

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_writer_text)).

  ==

  ---+ Description

  writes ontology classes as formatted text
  
**/
