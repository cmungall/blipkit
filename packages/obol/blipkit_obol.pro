/* -*- Mode: Prolog -*- */
/**   
  @author Chris Mungall
  @version @cvskw $Revision: 1.48 $
  @date @cvskw $Date: 2007/07/20 00:11:08 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| blipkit - simple interface to blip module functionality

  @s1 Description

*/

:- module(blipkit_obol,[]).

:- use_module(library(oset)).
:- use_module(library(porter_stem),[]).
:- use_module(token_match).
:- use_module(classdef_parser).
:- use_module(quickterm).
:- use_module(bio(mode)).
:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_lookup)).
:- use_module(bio(qobol_mp)).
:- use_module(bio(av_db)).
:- use_module(bio(tokenizer)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_obo)).
:- use_module(bio(ontol_writer_text)).
:- use_module(bio(bioprolog_util)).

blipkit:example('obol av -w translation',
                'looks up a word in the obol AV').
blipkit:example('obol av -u av_bridge_from_wn -w translation',
                'looks up a word in the obol AV and a wordnet db with an av view layer').

blipkit:opt_description(word,'single token').
blipkit:opt_description(stem,'perform stemming - NOT YET IMPLEMENTED - only works on basicnlp').

:- blip('av',
        'queries atomic vocab (AV)',
        [atom([word,w],W)],
        _Files,
        (   load_bioresource(obol_av),
            forall(word_type(W,T,On),
                   format('Word: ~w [~w] in ~w~n',[W,T,On])))).



% todo: move this
:- multifile
        user:bench_call/2,
        user:amigo_component/1.

:- multifile
        user:use_bioresource/1,
        user:use_bioresource/3,
        user:amigo_port/1.

user:amigo_port(p):- fail.
user:bioresource(r):- fail.
user:use_bioresource(r):- fail.
user:use_bioresource(r,fmt,module):- fail.

blipkit:example('obol -r go -table_pred process5/3 -table_pred process/3 obol-server -amigo conf_obol',
                'starts a serval server').
:- blip('obol-server',
        'starts a serval/http web server',
        [
         atom(preprocess_method,PreProcessMethod,pre),
         atom(query,_QAtom1,''),
         atom(synonyms,_SynonymPolicy,show),
         number(port,Port,8200),
         bool([background,bg],Bg),
         atoms(amigo,ConfL)
        ],
        _,
        (   debug(obol,'conf: ~w',[ConfL]),
            forall(member(Conf,ConfL),
                   user:consult(amigo_conf(Conf))),
            forall(amigo_component(Src),
                   user:consult(amigo_src(Src))),
            (   var(Port)
            ->  (   amigo_port(Port)
                ->  true
                ;   Port=8111)
            ;   true),
            ensure_loaded(bio(classdef_parser)),
            ensure_loaded(bio(serval)),
            preprocess_tokens(PreProcessMethod),
            start_server(Port),
            (   Bg=1
            ->  background
            ;   true),
            prolog)).
background:-
        repeat,
        fail.

:- blip('obol-generate-synonyms',
        'given an xp, generate synonyms using grammar',
        [
         atom(preprocess_method,PreProcessMethod,pre),
         atom(query,QueryAtom,true),
         bool(create_names,CreateNames)
        ],
        _QAtoms,
        (   load_bioresource(obol_av),
            ensure_loaded(bio(classdef_parser)),
            preprocess_tokens(PreProcessMethod),
            debug(obol,'Generating synonyms..',[]),
            atom_to_term(QueryAtom,Query,Bindings),
            member('ID'=ID,Bindings),
            forall((class_cdef(ID,CDef),Query,
                    (   class(ID,N)->true;N='?'),debug(obol,'Class: ~w ~w',[ID,CDef]),generate_synonyms(ID,CDef,[],Syns)),
                   (   (   Syns=[]
                       ->  format('! No syns generated for ~w ~w~n',[ID,N])
                       ;   format('[Term]~nid: ~w ! ~w~n',[ID,N]),
                           (   CreateNames=1
                           ->  Syns=[NewName|Syns2],
                               format('name: ~w~n',[NewName])
                           ;   Syns2=Syns),
                           write_synonyms(ID,Syns2),
                           nl))))).

:- blip('obol-generate-textdefs',
        'given an xp, generate textdefs using grammar',
        [
         atom(preprocess_method,PreProcessMethod,pre),
         atoms(idspace,IDSpaces)
        ],
        _QAtoms,
        (   load_bioresource(obol_av),
            ensure_loaded(bio(classdef_parser)),
            preprocess_tokens(PreProcessMethod),
            debug(obol,'Generating textdefs..',[]),
            forall((class_cdef(ID,CDef),
                    \+ def(ID,_),
                    id_idspace(ID,IDSpace),
                    member(IDSpace,IDSpaces)),
                   generate_and_write_textdef(ID,CDef)))).


generate_and_write_textdef(ID,CDef) :-
        (   class(ID,N)
        ->  true
        ;   N='?'),
        debug(obol,'Class: ~w ~w ~w',[ID,N,CDef]),
        cdef_to_textdef_via_cfg(CDef,TextDef),
       
        !,
        format('[Term]~nid: ~w ! ~w~n',[ID,N]),
        format('def: "~w" [OBOL:automatic]~n',[TextDef]),
        nl,
        nl.
generate_and_write_textdef(ID,_) :-
        (   class(ID,N)
        ->  true
        ;   N='?'),
        format('! No textdef for ~w "~w"~n',[ID,N]).

:- blip('obol-generate-names',
        'given an xp, generate names using grammar',
        [
         bool(show_classdef,ShowClassDef),
         atom(query,QueryAtom,true),
         atom(preprocess_method,PreProcessMethod,pre)
        ],
        _QAtoms,
        (   load_bioresource(obol_av),
            ensure_loaded(bio(classdef_parser)),
            preprocess_tokens(PreProcessMethod),
            debug(obol,'Generating names..',[]),
            atom_to_term(QueryAtom,Query,Bindings),
            member('ID'=ID,Bindings),
            forall((class_cdef(ID,CDef),Query),
                   (   debug(obol,'Class: ~w ~w',[ID,CDef]),
                       (   class(ID,N)
                       ->  format('! already have name ~w "~w"~n',[ID,N])
                       ;   cdef_to_name_via_cfg(CDef,N))
                   ->  format('[Term]~nid: ~w~n',[ID]),
                       format('name: ~w~n',[N]),
                       (   ShowClassDef=1
                       ->  show_classdef_body(CDef)
                       ;   true),
                       nl
                   ;   debug(obol_info,'No parse for ~w ~w',[ID,CDef]))))).

% declare this externally
user:generate_cdef/1.

:- blip('obol-generate-xps',
        'generate cross-products. Relies on generate_cdef/1 being declared.',
        [
         boolean(optimize,IsOptimize),
         atom(idspace,IDSpace,'test:'),
         atom(parse_rule,ParseRule,term_label),
         atom(xp_policy,XPPolicy,show),
         atom(synonym_policy,SynonymPolicy,name)
        ],
        _,
        (   %load_bioresource(obol_av),
            ensure_loaded(bio(classdef_parser)),
            preprocess_tokens(pre),
            (   IsOptimize=1
            ->  user:optimize_generate_cdef
            ;   true),
            Opts=[parse_rule(ParseRule),
                  xp_policy(XPPolicy),
                  def_policy(show),
                  synonym_policy(SynonymPolicy)],
            solutions(CDef,(class(X),class_cdef(X,CDef)),ExistingCDefs),
            solutions(X-CDef,(class(X),class_cdef(X,CDef)),ClassCDefPairs),
            debug(obol,'generating...',[]),
            solutions(CDef,generate_cdef(CDef),CDefs1),
            debug(obol,'normalizing...',[]),
            maplist(normalize_cdef,CDefs1,CDefs),
            debug(obol,'writing...',[]),
            forall((member(CDef,CDefs),member(ID-CDef,ClassCDefPairs)),
                   write_cdef(obo,ID,CDef)),
            format('! new classes~n'),
            forall((member(CDef,CDefs),\+member(CDef,ExistingCDefs)),
                   (   gensym(IDSpace,ID),
                       debug(obol,'cdef=~w',[CDef]),
                       show_classdef(ID,CDef,Opts))))).

:- blip('obol-generate-xp-LCAs',
        'generate cross-product common ancestors. SLOW',
        [
         atom(ont,Ont),
         atoms(root,Roots),
         atom(idspace,IDSpace,'test:'),
         atom(parse_rule,ParseRule,term_label),
         atom(xp_policy,XPPolicy,show),
         atom(synonym_policy,SynonymPolicy,hide)
        ],
        _,
        (   Opts=[parse_rule(ParseRule),
                  xp_policy(XPPolicy),
                  def_policy(hide),
                  synonym_policy(SynonymPolicy)],
            solutions(CDef,(class(X),class_cdef(X,CDef)),ExistingCDefs),
            assert(done(foo)),
            repeat,
            class(X),
            belongs(X,Ont),
            class(Y),
            belongs(Y,Ont),
            X @< Y,
            xp_lca(X,Y,CDef,Roots),
            \+ member(CDef,ExistingCDefs),
            \+ done(CDef),
            assert(done(CDef)),
            gensym(IDSpace,ID),
            show_classdef(ID,CDef,Opts),
            fail)).
            
%            solutions(CDef,xp_lca_ont(CDef,Ont),CDefs),
%            forall((member(CDef,CDefs),\+member(CDef,ExistingCDefs)),
%                   (   gensym(IDSpace,ID),
%                       show_classdef(ID,CDef,Opts))))).

xp_lca_ont(CDef,Ont):-
        class(X),
        class(Y),
        belongs(X,Ont),
        belongs(Y,Ont),
        X @< Y,
        xp_lca(X,Y,CDef).

xp_lca(X,Y,cdef(GA,DiffsA),Roots) :-
        genus(X,GX),
        genus(Y,GY),
        subclass_lca(GX,GY,GA),
        \+ ((member(Root,Roots),
             subclassRT(Root,GA))),
        !,
        solutions(R=F,differentium(X,R,F),DiffsX),
        solutions(R=F,differentium(Y,R,F),DiffsY),
        diffs_lca(DiffsX,DiffsY,DiffsA),
        DiffsA\=[].

diffs_lca([R=FX|DiffsXT],DiffsY,[R=FA|DiffsA]) :-
        select(R=FY,DiffsY,DiffsYT),
        !,
        subclass_lca(FX,FY,FA),
        diffs_lca(DiffsXT,DiffsYT,DiffsA).
diffs_lca(DiffsX,[R=FY|DiffsYT],[R=FA|DiffsA]) :-
        select(R=FX,DiffsX,DiffsXT),
        !,
        subclass_lca(FX,FY,FA),
        diffs_lca(DiffsXT,DiffsYT,DiffsA).
diffs_lca([_|DiffsXT],DiffsYT,DiffsA) :-  % ignore
        !,
        diffs_lca(DiffsXT,DiffsYT,DiffsA).
diffs_lca(DiffsXT,[_|DiffsYT],DiffsA) :-  % ignore
        !,
        diffs_lca(DiffsXT,DiffsYT,DiffsA).
diffs_lca([],[],[]).

subclass_lca(X,Y,A) :-
        subclass_lca([X,Y],A),
        belongs(X,Ont),
        belongs(Y,Ont).

:- blip('obol-parse',
        'parses terms into classdefs (NEW). recommend: use with -table_pred on process5/3 and process/3',
        [
         atom(preprocess_method,PreProcessMethod,pre),
         number(minscore,MinScore,-999),
         atom(query,QAtom1),
         atom(lookup,LookupAtom),
         atom(parse_rule,ParseRule,term_label),
         atom(xp_policy,XPPolicy,show),
         atoms(xp_ontology,XPOntologyList),
         atom(synonym_policy,SynonymPolicy,hide)
        ],
        QAtoms,
        (   load_bioresource(obol_av),
            preprocess_tokens(PreProcessMethod),
            Opts=[parse_rule(ParseRule),
                  xp_policy(XPPolicy),
                  minscore(MinScore),
                  synonym_policy(SynonymPolicy)],
            solutions(ID,
                      (   member(QAtom,[QAtom1|QAtoms]),
                          nonvar(QAtom),
                          atom_to_term(QAtom,Q,Bindings),
                          member('ID'=ID,Bindings),
                          debug(obol,'Q=~w',[Q]),
                          Q),
                      IDs1),
            solutions(ID,
                      (   nonvar(LookupAtom),
                          atom_to_term(LookupAtom,Lookup,[]),
                          lookup_class(Lookup,ID)),
                      IDs2),
            append(IDs1,IDs2,IDs),
            length(IDs,NumIDs),
            debug(obol,'ids to parse [~w]: ~w',[NumIDs,IDs]),
            findall(ID-CDef,
                    (   member(ID,IDs),
                        statistics(cputime,CpuTime),
                        format('! parsing: ~w  :: CPUTIME: ~w~n',[ID,CpuTime]),
                        time_goal(show_classdef_parse(ID,CDef,Opts),
                                  TimeDelta),
                        (   XPOntologyList\=[]
                        ->  debug(obol,'checking for membership: ~w',[XPOntologyList]),
                            test_cdef_has_xp_in_ontology_list(CDef,XPOntologyList)
                        ;   true),
                        show_classdef(ID,CDef,Opts),
                        format('! seconds spent parsing ~w: ~w~n',[ID,TimeDelta])),
                    IDCDefPairs),
            show_supplementary_info(IDCDefPairs,[]))).

% (+,+) semidet
test_cdef_has_xp_in_ontology_list(C,Onts):-
        member(Ont,Onts),
        test_cdef_has_xp_in_ontology(C,Ont),
        !.

% (+,+) nondet
% diff
test_cdef_has_xp_in_ontology(cdef(_,DL),Ont):-
        member(_=X,DL),
        belongs(X,Ont).
% genus
test_cdef_has_xp_in_ontology(cdef(G,_),Ont):-
        belongs(G,Ont).
% recursive
test_cdef_has_xp_in_ontology(cdef(_,DL),Ont):-
        member(_=X,DL),
        test_cdef_has_xp_in_ontology(X,Ont).

% OLD
test_cdef_xp(cdef(_,DL),XPOntologyList):-
        member(_=X,DL),
        member(XPOnt,XPOntologyList),
        belongs(X,XPOnt),
        !.
test_cdef_xp(cdef(_,DL),XPOntologyList):-
        member(_=X,DL),
        member(XPOnt,XPOntologyList),
        belongs(X,XPOnt),
        !.

show_supplementary_info(IDCDefPairs,_):-
        solutions(AnonClass,
                  (   member(_-CDef,IDCDefPairs),
                      cdef_extract_anon(CDef,AnonClass,0)),
                  AnonClasses),
        format(user_error,'! anonymous primitive or inner classes created as by-products:~n',[]),
        debug(obol,'anon classes: ~w',[AnonClasses]),
        maplist(show_anon_class,AnonClasses).


cdef_extract_anon(Anon,Anon,_):-
        Anon=anon(_,_).
cdef_extract_anon(CDef,CDef,Depth):-
        CDef=cdef(_,_),
        Depth>0.
cdef_extract_anon(cdef(G,_),Anon,Depth):-
        DepthPlus1 is Depth+1,
        cdef_extract_anon(G,Anon,DepthPlus1).
cdef_extract_anon(cdef(_,Diffs),Anon,Depth):-
        member(_=To,Diffs),
        DepthPlus1 is Depth+1,
        cdef_extract_anon(To,Anon,DepthPlus1).

show_anon_class(AnonClass):-
        show_anon_class1(AnonClass),
        !.
show_anon_class(AnonClass):-
        format(user_error,'! Could not write anon class: ~w~n',[AnonClass]).

show_anon_class1(AnonClass):-
        AnonClass=anon(N,NS),
        !,
        obo_anon_id(AnonClass,ID),
        format('[Term]~n'),
        format('id: ~w~n',[ID]),
        format('is_anonymous: true~n'),
        format('name: ~w~n',[N]),
        format('namespace: ~w~n',[NS]),
        nl.

show_anon_class1(CDef):-
        CDef=cdef(_,_),
        !,
        obo_cdef_id(CDef,ID),
        format('[Term]~n'),
        obo_id_comment(ID,IDTagVal),
        format('id: ~w~n',[IDTagVal]), 
        show_classdef_body(CDef),
        forall(show_classdef_extra(ID,CDef,[]),
               true),
        debug(obol,'done with ~w',[ID]),
        nl.

% show_classdef_parse(+ID,?CDefBest,+Opts) is semidet
% shows AND finds best
% todo: split showing from finding
% case1: already have 
show_classdef_parse(ID,_,Opts):-
        member(xp_policy(newonly),Opts),
        class_cdef(ID,CDef),
        !,
        class(ID,N),
        format(user_error,'! Already have xp for: ~w ~w~n',[ID,N]),
        show_classdef_label(ID,'CURRENT ',CDef),
        !,
        fail.
show_classdef_parse(ID,CDefBest,Opts):-
        member(parse_rule(def),Opts),
        def(ID,TextDef),
        !,
        (   class(ID,Name)
	->  true
	;   Name='?'),
        (   setof(Label-CDef,(def(ID,Label),
                              debug(obol,'textdef_to_cdef ~w ~w',[ID,Label]),
                              textdef_to_cdef_via_cfg(Label,CDef),
                              \+invalid_cdef(ID,CDef),
                              \+atom(CDef)),
                  LabelCDefPairs)
        ->  best_classdef(ID,LabelCDefPairs,CDefBest,Opts)
        ;   format(user_error,'! No parse for: ~w ! ~w "~w"~n',[ID,Name,TextDef]),
            fail).
show_classdef_parse(ID,CDefBest,Opts):-
        (   class(ID,Name)
	->  true
	;   Name='?'),
        !,
        % use name and exact syns as substrate for parsing
        solutions(Label,class_label_exact(ID,Label),Labels),

        % we can be more efficient by not searching all possible terms;
        % eg we may know we want a biological process
        (   member(parse_rule(DCGRule),Opts)
        ->  true
        ;   DCGRule=term_label),
        
        debug(obol,'labels to parse for ~w: ~w',[ID,Labels]),
        (   setof(Label-CDef,(member(Label,Labels),
                              debug(obol,'name_to_cdef ~w ~w',[ID,Label]),
                              name_to_cdef_via_cfg(Label,CDef,DCGRule),
                              debug(obol,'candidate cdef ~w',[CDef]),
                              \+invalid_cdef(ID,CDef),
                              \+atom(CDef)),
                  LabelCDefPairs)
        ->  best_classdef(ID,LabelCDefPairs,CDefBest,Opts)
        ;   format(user_error,'! No parse for: ~w ! ~w~n',[ID,Name]),
            fail).

% todo: if there is a tie, choose the one with most votes
best_classdef(ID,LabelCDefPairs,CDefBest,Opts):-
        findall(CDef-Score,
                (   member(Label-CDef,LabelCDefPairs),
                    id_cdef_fitness(ID,CDef,Score),
                    format(user_error,'! parse: ~w using "~w" => ~w [~w]~n',[ID,Label,CDef,Score])),
                CDefScorePairs),
        findmax(CDef,Score,member(CDef-Score,CDefScorePairs),_,BestScore), % scores are penalities[??], find highest
        (   member(minscore(MinScore),Opts)
        ->  BestScore >= MinScore
        ;   true),
        solutions(CDef,member(CDef-BestScore,CDefScorePairs),BestCDefs), % equal top-scoring cdefs
        debug(obol,'equal top scoring: ~w',[BestCDefs]),
        findmax(CDef,
                Num,
                (   member(CDef,BestCDefs),
                    findall(Score,member(CDef-Score,CDefScorePairs),Scores),
                    length(Scores,Num)),
                CDefBest,
                HighestNum),
        debug(obol,'winner: ~w (with ~w votes)',[CDefBest,HighestNum]),
        !.

% todo: 
show_classdef(ID,CDef,Opts):-
        !,
        show_compare_xp(ID,CDef,Opts),
        (   def(ID,TextDef)
        ->  format('! def: "~w"~n',[TextDef])
        ;   true),
        show_classdef_label(ID,'PROPOSED',CDef),
        format('[Term]~n'),
        obo_id_comment(ID,IDTagVal),
        format('id: ~w~n',[IDTagVal]), 
        show_classdef_body(CDef),
        forall(show_classdef_extra(ID,CDef,Opts),
               true),
        debug(obol,'done with ~w',[ID]),
        nl.

show_classdef_label(_ID,Tag,CDef):-
        cdef_label(CDef,Label),
        !,
        format(user_error,'! ~w:  ~w [~w]~n',[Tag,Label,CDef]).
show_classdef_label(_,_,_).

show_compare_xp(ID,_CDef,_Opts):-
        class_cdef(ID,CDefCurrent),
        !,
        show_classdef_label(ID,'CURRENT ',CDefCurrent).
show_compare_xp(_,_,_) :-
        format(user_error,'! no existing xp def~n',[]).

show_classdef_extra(ID,CDef,Opts):-
        \+ member(synonym_policy(hide),Opts),
        debug(obol,'showing new synonyms for ~w',[ID]),
        generate_synonyms(ID,CDef,Opts,Syns),
        debug(obol,'syns: ~w',[Syns]),
        (   member(synonym_policy(name),Opts)
        ->  Syns=[Name|Syns2],
            format('name: ~w~n',[Name]),
            write_synonyms(ID,Syns2)
        ;   write_synonyms(ID,Syns)).
show_classdef_extra(ID,CDef,Opts):-
        % \+ def(ID,_), show all for now
        \+ member(parse_rule(def),Opts),  % too explosive
        \+ member(def_policy(hide),Opts),
        debug(obol,'showing defs for ~w',[ID]),
        solutions(TextDef,cdef_to_textdef_via_cfg(CDef,TextDef),[TextDef|_]),
        format('def: "~w" [OBOL:automatic]~n',[TextDef]).

show_classdef_body(cdef(G,Diffs)):-
        obo_id_comment(G,GN),
        format('intersection_of: ~w~n',[GN]),
        maplist(show_classdef_diff,Diffs).
show_classdef_diff(Diff):-      % allow both styles..
        Diff=..[R,To],
        !,
        show_classdef_diff(R=To).
show_classdef_diff(card(R,Q)=To):-
        !,
        obo_id_comment(To,ToTxt),
        format('intersection_of: ~w ~w {cardinality="~w"} ! ~w~n',[R,To,Q,ToTxt]).
show_classdef_diff(R=To):-
        !,
        obo_id_comment(To,ToTxt),
        format('intersection_of: ~w ~w~n',[R,ToTxt]).
        
obo_id_comment(ID,Txt):-
        class(ID,N),
        !,
        sformat(Txt,'~w ! ~w',[ID,N]).
obo_id_comment(anon(Atom,NS),ID):-
        !,
        obo_anon_id(anon(Atom,NS),ID).
obo_id_comment(cdef(G,Diffs),ID):-
        !,
        (   obo_cdef_id(cdef(G,Diffs),ID)
        ->  true
        ;   atom_to_term(ID,cdef(G,Diffs),[])).
obo_id_comment(ID,ID).

obo_anon_id(CDef,ID):-
        CDef=cdef(_,_),
        obo_cdef_id(CDef,ID),
        !.
obo_anon_id(anon(Atom,NS),ID):-
        obo_id_safe(Atom,ID,NS).

:- dynamic cdef_id/2.
obo_cdef_id(CDef,ID):-
        cdef_id(CDef,ID),
        !.
% TODO: change this to use ID syntax
obo_cdef_id(CDef,ID):-
        cdef_to_name_via_cfg(CDef,N),
        !,
        obo_id_safe(N,ID,'_'),
        assertz(cdef_id(CDef,ID)).
obo_cdef_id(CDef,ID):-
        % cannot generate suitable name
        term_to_atom(CDef,N),
        obo_id_safe(N,ID,'_'),
        assertz(cdef_id(CDef,ID)).
        
obo_id_safe(Label,SafeID,NS):-
        atom_chars(Label,Toks),
        strip_non_ws_tokens(Toks,Toks2),
        atom_chars(SafeID1,Toks2),
        concat_atom([NS,SafeID1],':',SafeID).

strip_non_ws_tokens([],[]).
strip_non_ws_tokens([T|L],[T|L2]):-
        non_ws(T),
        !,
        strip_non_ws_tokens(L,L2).
strip_non_ws_tokens([_|L],['_'|L2]):-
        strip_non_ws_tokens(L,L2).

non_ws(T):- T @>= 'a',T @=< 'z'.
non_ws(T):- T @>= 'A',T @=< 'Z'.
non_ws(T):- T @>= '0',T @=< '9'.
foo___('0').   % does nothing; fixes emacs syntax highighting bug!

generate_synonyms(ID,CDef,Opts,Syns):-
        solutions(Syn,
                  (   cdef_to_name_via_cfg(CDef,Syn),
                      debug(obol,'syn: ~w',[Syn]),
                      \+ class(ID,Syn), % exclude name from synonyms list
                      (   member(synonym_policy(showall),Opts)
                      ;   \+ entity_synonym(ID,Syn))),
                  Syns).


write_synonyms(ID,Syns):-
        forall(member(Syn,Syns),
               (   (   entity_synonym(ID,Syn)
                   ->  Xref=''
                   ;   Xref='OBOL:automatic'),
                   format('synonym: "~w" EXACT [~w]~n',[Syn,Xref]))).

:- blip('obol-xp-query',
        'suggest xps based on query',
        [
         atom(query,QAtom1),
         atom(lookup,LookupAtom),
         atom(xp_policy,XPPolicy,no),
         terms(inverse,Inverses),
         atom(synonym_policy,SynonymPolicy,hide)
        ],
        QAtoms,
        (   
            Opts=[xp_policy(XPPolicy),
                  def_policy(hide),
                  inverses(Inverses),
                  synonym_policy(SynonymPolicy)],
            solutions(ID,
                      (   member(QAtom,[QAtom1|QAtoms]),
                          nonvar(QAtom),
                          atom_to_term(QAtom,Q,Bindings),
                          member('ID'=ID,Bindings),
                          debug(obol,'Q=~w',[Q]),
                          Q),
                      IDs1),
            solutions(ID,
                      (   nonvar(LookupAtom),
                          atom_to_term(LookupAtom,Lookup,[]),
                          lookup_class(Lookup,ID)),
                      IDs2),
            append(IDs1,IDs2,IDs),
            length(IDs,NumIDs),
            debug(obol,'ids to parse [~w]: ~w',[NumIDs,IDs]),
            findall(ID-CDef,
                    (   member(ID,IDs),
                        %format('! parsing: ~w~n',[ID]),
                        xp(ID,G,Diffs),
                        CDef=cdef(G,Diffs),
                        show_classdef(ID,CDef,Opts)),
                    IDCDefPairs),
            writeln(IDCDefPairs),
            show_supplementary_info(IDCDefPairs,[]))).

% remember to run using:
% blip-ddb -u blipkit_obol
:- blip('obol-quickterm',
        'quickterm template',
        [
         terms(arg,Args),
         term(idspace,Ont),
         term(template,T), % main request
         atom(ontology_dir,ODir),
         atom(subfile,NFile),
         atom(addfile,AFile),
         atom(delfile,DFile),
         number(idnum_min,IDNumMin,1),
         bool(commit,CommitX)
        ],
        _,
        (
         (   nonvar(CommitX),
             CommitX=1
         ->  Commit=true
         ;   Commit=false),
         Opts=[idspace(Ont),
               ontology_dir(ODir),
               commit(Commit),
               addfile(AFile),
               subfile(NFile),
               delfile(DFile),
               idnum_min(IDNumMin)
              |
              Args],
         (   nonvar(NFile)
         ->  load_biofile(NFile)
         ;   true),
         (   nonvar(AFile)
         ->  load_biofile(AFile)
         ;   true),
         template_request(T,Msg,Opts),
         writeln(msg=Msg))).

:- blip('obol-suggest',
        'suggest xps based on necessary conditions only',
        [
         atom(query,QAtom1),
         atom(lookup,LookupAtom),
         atom(xp_policy,XPPolicy,no),
         terms(inverse,Inverses),
         atom(synonym_policy,SynonymPolicy,hide)
        ],
        QAtoms,
        (   
            Opts=[xp_policy(XPPolicy),
                  def_policy(hide),
                  inverses(Inverses),
                  synonym_policy(SynonymPolicy)],
            solutions(ID,
                      (   member(QAtom,[QAtom1|QAtoms]),
                          nonvar(QAtom),
                          atom_to_term(QAtom,Q,Bindings),
                          member('ID'=ID,Bindings),
                          debug(obol,'Q=~w',[Q]),
                          Q),
                      IDs1),
            solutions(ID,
                      (   nonvar(LookupAtom),
                          atom_to_term(LookupAtom,Lookup,[]),
                          lookup_class(Lookup,ID)),
                      IDs2),
            append(IDs1,IDs2,IDs),
            length(IDs,NumIDs),
            debug(obol,'ids to parse [~w]: ~w',[NumIDs,IDs]),
            findall(ID-CDef,
                    (   member(ID,IDs),
                        format('! parsing: ~w~n',[ID]),
                        guess_cdef(ID,CDef),
                        show_classdef(ID,CDef,Opts)),
                    IDCDefPairs),
            show_supplementary_info(IDCDefPairs,[]))).

guess_cdef(ID1,cdef(G,Diffs)):-
        \+ genus(ID1,_),
        solutions(rel(R,To,ToN),
                (   parent(ID1,R,To),
                    class(To,ToN)),
                Rels),
        Rels=[_,_|_],
        \+ ((
             class(ID2),
             ID2\=ID1,
             forall(member(rel(R,To,_),Rels),
                    parent(ID2,R,To)))),
        select(rel(subclass,G,_),Rels,RelsT),
        solutions(Diff,(member(rel(R,To,_),RelsT),guess_diff(R,To,Diff)),Diffs).

guess_diff(subclass,X,R=Y):-
        genus(X,Root),
        \+ subclass(Root,_),
        findall(R=Y,differentium(X,R,Y),[R=Y]),
        !.
guess_diff(R,To,R=To).



:- blip('obol-stats',
        'cross-product statistics ',
        [],
        Files,
        (   
            ensure_loaded(bio(ontol_db)),
            maplist(load_biofile,Files),
            forall((obol_stat(Desc,X,G),G),
                   format('~w: ~w~n',[Desc,X])),
            forall((obol_stat_group(Desc,L,G),G),
                   forall(member(Group-Count,L),
                          format('  ~w:~n    ~w: ~w~n',[Desc,Group,Count]))),
            nl)).

obol_stat('Total xp defs (redundant)',X,setof_count(C,genus(C,_),X)).
obol_stat('Classes defined (distinct)',X,setof_count(C,genus(C,_),X)).
obol_stat('Genus classes (distinct)',X,setof_count(G,genus(_,G),X)).

% grouped-by
obol_stat_group('Classes defined [by ontology]',
                X,
                count_by(NS,C,(genus(C,_),
                               obol_ns(C,NS)),X)).

obol_stat_group('Classes defined [by ontology, source]',
                X,
                count_by(NS*S,C,(genus(C,G),
                                 obol_ns(C,NS),
                                 clause_source_short(genus(C,G),S)),X)).

obol_ns(C,S):- belongs(C,S),S\='gene_ontology',S\='unknown'.



:- blip('ontol-diff-cdef',
        'compares cdefs in 2 or more files ',
        [],
        _,
        (   
            %forall(member(File,FileL),
            %       load_factfile(File,Mod)),
            ensure_loaded(bio(classdef_parser)),
            solutions(ID,genus(ID,_),IDs),
            maplist(cdef_diff,IDs))).

cdef_diff(ID):-
        (   class(ID,N)
        ->  true
        ;   N=ID),
        format('~n! ---~nClass: ~w "~w"~n',[ID,N]),

        findall(File-cdef(Genus,Diffs),
                (   genus(ID,Genus),
                    clause(genus(ID,Genus),_,Clause),
                    clause_property(Clause,file(File)),
                    findall(R=To,
                            (   differentium(ID,R,To),
                                clause(differentium(ID,R,To),_,DClause),
                                clause_property(DClause,file(File))),
                            Diffs)),
                FileCDefPairs),
        setof(File,CDef^member(File-CDef,FileCDefPairs),Files),
        sort(Files,FilesSorted),
        debug(blip,'Pairs=~w',[FileCDefPairs]),
        debug(blip,'Files=~w',[FilesSorted]),
        
        (   FilesSorted=[File1,File2]
        ->  member(File1-CDef1,FileCDefPairs),
            member(File2-CDef2,FileCDefPairs),
            debug(blip,'pair: ~w - ~w~n',[CDef1,CDef2]),
            serialize_cdef(abstract,CDef1,CDef1Atom),
            serialize_cdef(abstract,CDef2,CDef2Atom),
            format('[1]  ~w~n',[CDef1Atom]),
            format('[2]  ~w~n',[CDef2Atom]),
            CDef1=cdef(G1,Diffs1),
            CDef2=cdef(G2,Diffs2),
            (   G1\=G2
            ->  format('MISMATCH genus: ~w NOT= ~w~n',[G1,G2])
            ;   format('MATCH    genus: ~w    = ~w~n',[G1,G2])),
            (   forall(member(_=To,Diffs1),
                       member(_=To,Diffs2)),
                forall(member(_=To,Diffs2),
                       member(_=To,Diffs1))
            ->  format('MATCH differentia~n')
            ;   format('MISMATCH differentia: ~w NOT= ~w~n',[Diffs1,Diffs2]))
        ;   format('Unmatched. only in: ~w~n',[Files])).


% to be deprecated..


blipkit:example('obol mine-adjectives -r obol_av -u ontol_bridge_from_av_noun -r disease -r fma disease_ontology',
                'finds potential adjective-noun pairs in DO via stemming').
:- blip('mine-adjectives',
        'finds relational adjectives and associated noun via stemming. Writes adj-noun-ont-stem-adjSourceClass.',
        [],
        Onts,
        (   findall(WordDn-Ont,(class(Class,Word),belongs(Class,Ont),contains_no_whitespace(Word),downcase_atom(Word,WordDn)),WordOntPairs),
            debug(nlp,'Finding stems',[]),
            findall(w(Word,Stem,Ont),(member(Word-Ont,WordOntPairs),porter_stem(Word,Stem)),WTermPairs),
            debug(nlp,'Finding adjectives',[]),
            forall((   belongs(Class,Ont),
                       member_or_not_applicable(Ont,Onts),
                       entity_label_or_synonym(Class,Label)),
                   mine_and_show_adjectives(Class,Label,WTermPairs)))).

member_or_not_applicable(_,[]):- !.
member_or_not_applicable(E,L):- member(E,L).

contains_no_whitespace(Word):-
        \+ sub_atom(Word,_,_,_,' ').
mine_and_show_adjectives(Class,Label,WTermPairs):-
        tokenize_atom(Label,TokensMixedCase),
        maplist(downcase_atom,TokensMixedCase,Tokens),
        forall((   member(Token,Tokens),
                   porter_stem(Token,Stem),
                   member(w(CoreWord,Stem,Ont),WTermPairs),
                   % exclude potential adjectives that are already classes; eg avoid neuron/neuronitis
                   \+ member(w(Token,_,_),WTermPairs),
                   test_adj_coreword(Token,CoreWord)),
               format('relational_adj_ra ~w ~w ~w ~w ~w~n',[Token,CoreWord,Ont,Stem,Class])).

test_adj_coreword(Adj,CoreWord):-
        Adj\=CoreWord,
        \+ atom_concat(Adj,s,CoreWord),
        \+ atom_concat(CoreWord,s,Adj),
        \+ relational_adj_ra(Adj,CoreWord,_), % exclude existing
        \+ relational_adj_pp(Adj,CoreWord,_).

% deprecation candidate..
:- blip('mine-synonyms',
        'finds additional synonyms based on syns of matching words in other ont',
        [bool(stem,Stem),
         atom(root,Root)],
        Onts,
        (   load_bioresource(obol_av),
            _Opts=[stem(Stem)],
            (   nonvar(Root)
            ->  (   class(Root,_)
                ->  RootID=Root
                ;   (class(RootID,Root),\+((belongs(RootID,Ont),member(Ont,Onts))))
                ->  true
                ;   throw(error(no_such_class(Root))))
            ;   true),
            format(user_error,'RootID: ~w~n',[RootID]),
            forall(member(Ont,Onts),
                   forall((belongs(ID,Ont),
                           class(ID,N),
                           synonym_match(ID,ID2,_Label,Label2),
                           belongs(ID2,Ont2),
                           Ont\=Ont2,
                           (nonvar(RootID)->subclassRT(ID2,RootID);true)
                           ),
                          write_termrow(synmatch(ID,Ont,N,Label2)))))).
                          
synonym_match(ID,MatchingID,Label,Label2):-
        class_by_name_or_synonym(Label,ID),
        class_by_name_or_synonym(Label,MatchingID),
        class_by_name_or_synonym(Label2,MatchingID),
        Label\=Label2.

blipkit:example('obol qobol -ontology MP -tag morphology -tag mp -undefined_only true -export obo',
                'parses MP using morphology/mp templates, only writing new xps, in obo').
:- blip('qobol',
        'quick obol',
        [options([ontology,xont,tag,subclass,export,undefined_only,compare,scope,id,lexical_variant,noindex],Opts)],
        _Onts,
        (   qobol_prep(Opts),
            qobol_index(Opts),
            qobol_process_all(Opts))).

:- blip('qobol-mismatch',
        'test for mismatches between parse and asserted xp',
        [options([ontology,tag,noindex],Opts)],
        _Onts,
        (   qobol_prep(Opts),
            qobol_index(Opts),
            forall(show_class_parse_mismatch(_,Opts),
                   true))).

blipkit:example('obol qobol-newterms -ontology MP -tag morphology -tag mp -undefined_only true',
                'generates new term suggestions').
:- blip('qobol-newterms',
        'Candidate new terms based on parse',
        [options([ontology,tag,xtag,undefined_only,id,subclass,subclass,taxon,noindex],Opts)],
        _Onts,
        (   qobol_prep(Opts),
            qobol_index(Opts),
            forall(suggest_term(E,Label,X,NewTerm,Opts),
                   show_factrow([],
                                suggest_term(E,Label,X,NewTerm)))
        )).


        
