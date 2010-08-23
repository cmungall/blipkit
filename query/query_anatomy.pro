
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(bio(tabling)).
:- use_module(bio(io)).
:- use_module(library(porter_stem),[]).

label_or_id(X,L):- entity_label(X,L).
label_or_id(X,X):- \+ entity_label(X,_).

id_db(X,DB):- concat_atom([DB,_],':',X).


non_isom_xref(A-AN,B-BN,C-CN):-
        non_isom_xref1(A,B,C),
        label_or_id(A,AN),
        label_or_id(B,BN),
        label_or_id(C,CN).

non_isom_xref1(Y,X1,X2):-
        class_xref(X1,Y),
        class_xref(X2,Y),
        X1\=X2,
        id_db(X1,DB),
        id_db(X2,DB).


non_isom_xref1(Y,X1,X2):-
        class_xref(Y,X1),
        class_xref(Y,X2),
        X1\=X2,
        id_db(X1,DB),
        id_db(X2,DB).


/*

  blip -r worm_anatomy -r mouse_anatomy -r fly_anatomy -r zebrafish_anatomy -r fma -i fly-to-worm-homology.obo -i ma-to-fma.obo -i fly-to-fma-homology.obo -i zfa-to-fma-homology.obo -u query_anatomy findall group_hset/3

  blip -r worm_anatomy -r mouse_anatomy -r fly_anatomy -r zebrafish_anatomy -r fma -i fly-to-worm-homology.obo -i ma-to-fma.obo -i fly-to-fma-homology.obo -i zfa-to-fma-homology.obo -u query_anatomy findall write_all_facts/0

  see anatomy_xp/Makefile


  what about recursive approach?

  [[[[MA+FMA] + XAO] + ZFA] + FB + WB]

  is we are throwing out non-isom sets we throw out less this way

  Initial Comment:
Thought these could help drive incorporation of homology, CARO, and the Chris-hacked MIAA (from discussion with Paula).

A common goal of searching across species is to find candidate genes that underly the development of some structure.  It is obvious there will be similarities in the genetic basis of homologous structures. However, it is also known that the same genetic toolkit is sometimes used in the development of NON-homologous structures (e.g. fly legs and mouse legs) that may be either analogous, or structurally similar.  So there are three search strategies:

1) Homology searches: "Give me the common set of genes involved in the development of Structure A and its Homologues"  

2) Analogy searches: "Give me the common set of genes involved in the development of Structure A and its functional analogues" (e.g. all things used in locomotion)

3) High-level structural similarity searches: "Give me the common set of genes involved in the development of Structure A and other appendages. (in this case appendages, such as antennae, might not be involved in locomotion and thus are not functional analogues)

It seems 1) can be queried using a set of homology statements 2) could be queried using MIAA and/or a organismal function ontology, and 3) could be queried using a structure based ontology such as an extended CARO.

Not sure how this would enter the UI.....but hopefully helpful for setting things up in the database.

  
*/


homologous_to(X,Y):- restriction(X,'OBO_REL:homologous_to',Y).
homologous_to(X,Y):- class_xref(X,Y),class(Y). % for recursive grouping
homologous_to(X,Y):- entity_alternate_identifier(X,Y),class(Y). % for MIAA
%homologous_to(X,Y):- class_xref(X,X1),restriction(X1,'OBO_REL:homologous_to',Y1),class_xref(Y,Y1).

homologous_toS(X,Y):- homologous_to(X,Y).
homologous_toS(X,Y):- homologous_to(Y,X).

homologous_toT(X,Y):-
        homologous_toT(X,Y,[]).

homologous_toT(X,Y,Done):-
        homologous_toS(X,Z),
        \+ member(Z,Done),
        homologous_toT(Z,Y,[X|Done]).

homologous_toT(X,Y,_):-
        homologous_toS(X,Y).



hchain(X,L):-
        solutions(X,homologous_toS(X,_),Xs),
        member(X,Xs),
        findall(Y,(member(X,Xs),homologous_toT(X,Y)),L).




/*
hchain(X,L):-
        homologous_to(X,_Y),
        hchain(X,L,[]).

hchain(X,[Y|L],In):-
        (   homologous_to(X,Y),
            \+ member(Y,In)
        ->  hchain(Y,L,[Y|In])
        ;   L=[]).

hchain(X,[Y|L],In):-
        homologous_to(X,Y),
        \+ member(Y,In),
        hchain(Y,L,[Y|In]).
hchain(X,[X],In):-
        \+ ((homologous_to(X,Y),
             \+ member(Y,In))).
*/


hset(Set):-
        hchain(_,L),
        sort(L,Set),
        Set=[_,_|_],
        debug(anatomy,'hset: ~w',[Set]).


% only works if x ont loaded
% too restrictive??
% TODO: merge sets if any members are shared
% why aren't these merged already??
% i_restrictionNR UBERON:FBbt_00004522-FMA_76808  develops_from   labrum  UBERON:FBbt_00004993-FMA_70544-ZFA_0001136      epidermis
% i_restrictionNR UBERON:FBbt_00004522-FMA_76808  develops_from   labrum  UBERON:FBbt_00004993-FMA_70596-MA_0000153-ZFA_0000105   epidermis
isom_hset(Set):-
        hset(Set),
        is_isom(Set).

is_isom(Set):-
        forall(member(X,Set),
               (   \+ ((belongs(X,Ont),
                        member(Y,Set),
                        X\=Y,
                        belongs(Y,Ont))))).

nonisom_hset(Set):-
        hset(Set),
        \+ is_isom(Set).

nonisom_hset(Set,Names):-
        nonisom_hset(Set),
        findall(N,(member(X,Set),entity_label(X,N)),Names).

isom_hset(Set,Size):-
        isom_hset(Set),
        length(Set,Size).

group_hset(Tax,Set):-
        isom_hset(Set),
        set_tax(Set,Tax).


group_hset(Tax,Set,Names):-
        group_hset(Tax,Set),
        findall(N,(member(X,Set),entity_label(X,N)),Names).

group_hset(Tax,Set,Names,Size):-
        group_hset(Tax,Set,Names),
        length(Set,Size).

canned_comment(true,isomorphic).
canned_comment(fail,'non-isomorphic').

% given a set of IDs, create a new class that is the union of these
group_facts(Sets,_IndexedSets,Set,[class(ID),entity_resource(ID,uberon),metadata_db:entity_comment(ID,'This class was created automatically from a combination of ontologies'),metadata_db:entity_label(ID,Label),def(ID,CombinedDef)|Facts]):-
        %isom_hset(Set),
        member(Set,Sets),
        gensetid(Set,ID),
        debug(anatomy,'set: ~w',[ID]),
        best_label(Set,Label),
        findall(Def,(member(X,Set),def(X,Def)),Defs),
        concat_atom(Defs,' // ',CombinedDef),
        findall(def_xref(ID,Xref),(member(X,Set),def_xref(X,Xref)),DefXrefFacts),
        findall(metadata_db:entity_xref(ID,X),member(X,Set),XrefFacts),
        findall(metadata_db:entity_synonym(ID,Syn),(member(X,Set),class_label(X,Syn,_),Syn\=Label),SynFacts1),
        findall(metadata_db:entity_synonym_scope(ID,Syn,Scope),(member(X,Set),class_label(X,Syn,Scope),Syn\=Label),SynFacts2),
        %findall(Fact,relfact(Set,IndexedSets,Fact),RelFacts),
        RelFacts=[],
        flatten([DefXrefFacts,XrefFacts,SynFacts1,SynFacts2,RelFacts],Facts).


                                %
%% gensetid(+List,?Id) det
% joins ids then replaces : with _
gensetid(Set,ID):-
        concat_atom(Set,'-',HyphenatedAtom), % join
        concat_atom(Toks,':',HyphenatedAtom), % split
        findall(Tok,(member(Tok,Toks),Tok\='UBERON'),FilteredToks),
        concat_atom(FilteredToks,'_',LocalID), % join
        atom_concat('UBERON:',LocalID,ID).

% NOT USED: see i_subclass
relfact(Set,IndexedSets,subclass(SetID,Set2ID)):-
        select(X,Set,SetR),
        subclassT(X,Y),
        member(Y-Set2,IndexedSets),
        member(A,SetR),
        subclassT(A,B),
        member(B,Set2),
        gensetid(Set,SetID),
        gensetid(Set2,Set2ID).
       
relfact(Set,IndexedSets,subclass(SetID,Set2ID)):-
        select(X,Set,SetR),
        parent_over(_Rel,X,Y),
        member(Y-Set2,IndexedSets),
        member(A,SetR),
        %parent_overT(_,A,B),    % weaken Rel constraint; lazy way of allowing for OBO_REL: vs no prefix..
        parentT(A,B), % weaken Rel constraint; lazy way of allowing for OBO_REL: vs no prefix..
        member(B,Set2),
        gensetid(Set,SetID),
        gensetid(Set2,Set2ID).

best_label([X|_],N):- entity_label(X,N1),downcase_atom(N1,N),!.
best_label([_|L],N):- best_label(L,N).

write_all_group_facts:-
        %setof(Set,isom_hset(Set),Sets),
        setof(Set,hset(Set),Sets),
        findall(X-Set,(member(Set,Sets),member(X,Set)),IndexedSets),
        forall(group_facts(Sets,IndexedSets,_,Facts),
               maplist(write_db_fact,Facts)),
        !,
        fail.

write_all_link_facts:-
        forall(i_subclassNR(X,Y),write_db_fact(subclass(X,Y))),
        forall(i_restrictionNR(X,R,Y),write_db_fact(restriction(X,R,Y))),
        !,
        fail.

write_interspecies_facts:-
        forall(class_xref(X,Y),write_db_fact(subclass(Y,X))),
        solutions(class(Y),class_xref(_,Y),Ys),
        maplist(write_db_fact,Ys),
        !,
        fail.


i_subclass(C,P):-
        belongs(C,uberon),
        entity_xref(C,CX),
        subclassT(CX,PX),
        \+ untrusted(CX),
        entity_xref(P,PX),
        belongs(P,uberon).

i_subclassNR(C,P):-
        i_subclass(C,P),
        \+ ((i_subclass(C,Z),
             i_subclass(Z,P))),
        debug(query,'~w',subclassNR(C,P)).


i_subclassNR(C,CN,P,PN):-
        i_subclassNR(C,P),
        class(C,CN),
        class(P,PN).

i_restriction(C,R,P):-
        belongs(C,uberon),
        entity_xref(C,CX),
        parent_over(R,CX,PX),
        R\=subclass,
        entity_xref(P,PX),
        belongs(P,uberon).

i_restrictionNR(C,R,P):-
        i_restriction(C,R,P),
        \+ ((i_restriction(C,_,Z),
             i_restriction(Z,_,P))),
        \+ ((i_restriction(C,_,Z),
             i_subclass(Z,P))).

i_restrictionNR(C,CN,R,P,PN):-
        i_restrictionNR(C,R,P),
        class(C,CN),
        class(P,PN).

untrusted(C):-
        belongs(C,worm_anatomy).

:- table_pred(i_subclass/2).
:- table_pred(i_restrictionNR/2).

        
set_has(Set,Ont):-
        member(X,Set),
        belongs(X,Ont).
        
set_tax(Set,eukaryote):-
        set_has(Set,fma),
        set_has(Set,zebrafish_anatomy),
        set_has(Set,'fly_anatomy.ontology'),
        !.

set_tax(Set,vertebrate):-
        set_has(Set,fma),
        set_has(Set,zebrafish_anatomy),
        !.

set_tax(_Set,unk):-!.

/*

  incrementally build ontologies based on phylogenetic tree

  each ontology subsumes the ones beneath it

  try and match each pairwise sub-ontology node N1, N2

  if isomorphic, add a new node N1+N2
    do we need to obsolete N1, N2
  
  no need to retain nodes - we can take the union of all ontologies at the end
    // subtract?
  
  at the end, throw in other union ontologies such as MIAA


  */

write_pair(X):-
        ontology_sib(X,R1,R2,AF),
        xload_bioresource(R1),
        xload_bioresource(R2),
        load_biofile(AF),
        write_all_group_facts.

xload_bioresource(X):-
        load_bioresource(X),
        load_extra(X).

load_extra(anu(X)):-
        !,
        ontology_sib(X,R1,R2,_),
        xload_bioresource(R1),
        xload_bioresource(R2).
load_extra(_).

% xref(u1,t1) ht(t1,t2) => u1,t2
ontology_sib(mammal,mouse_anatomy,fma,'ma-to-fma.obo').
%ontology_sib(tetrapod,mammal,'ma-to-fma.obo').
ontology_sib(vertebrate,anu(mammal),zebrafish_anatomy,'zfa-to-fma-homology.obo').
ontology_sib(bilateria,mammal,fly_anatomy,'fly-to-fma.obo').

xrefR(X,X).
xrefR(X,Y):- class_xref(X,Y).

/*
ontology_phylogeny(metazoan(
                            vertebrate(
                                       tetrapod(
                                                mammal(mouse,
                                                       human),
                                                xenopus),
                                       teleost(zebrafish,
                                               tao)),
                            fly)).
ontology_phylogeny(all(
                       uber(
                            miaa,
                            cell),
                       metazoan(
                                fly,
                                vertebrate(
                                           zebrafish,
                                           mammal(mouse,
                                                  human)))
                      )).

*/

% make the bigger one come 2nd..?
ontology_phylogeny(all(
                       uber(
                            miaa,
                            cell),
                       metazoan(
                                fly,
                                vertebrate(
                                           teleost(zebrafish,
                                                   teleost),
                                           tetrapod(
                                                    xenopus,
                                                    mammal(mouse,
                                                           human)
                                                   )
                                          )
                               )
                      )).

label_ontology(fly,'fly_anatomy.ontology').
label_ontology(mouse,'adult_mouse_anatomy.gxd').
label_ontology(human,fma).
label_ontology(zebrafish,zebrafish_anatomy).
label_ontology(xenopus,xenopus_anatomy).
label_ontology(miaa,miaa).
label_ontology(cell,cell).
label_ontology(teleost,teleost_anatomical_ontology).

/*
blip -debug anatomy  -i zf_lite.obo -i mouse_lite.obo -i fma_lite.obo -i fly_lite.obo -i ma-to-fma.obo -i fly-to-fma-homology.obo -i zfa-to-fma-homology.obo -u query_anatomy findall build_ontology/0

   blip -i cell_lite.obo -i fly_lite.obo -i mouse_lite.obo -i fma_lite.obo -i zf_lite.obo  -i fly-to-worm-homology.obo -i ma-to-fma.obo -i fly-to-fma-homology.obo -i zfa-to-fma-homology.obo  -u query_anatomy findall build_ontology/0 > test.pro
  
  */


%% write_unode(Node) det
% 
write_unode(U):-
        debug(anatomy,'  unode: ~w',[U]),
        unode_facts(U,Facts),
        debug(anatomy,'    facts: ~w',[Facts]),
        maplist(write_db_fact,Facts).



%% build_ontology
% assumes a phylogeny is defined using ontology_phylogeny/1
% recursively builds ontology, then writes it as facts
build_ontology:-
        ontology_phylogeny(Tree),
        build_ontology(Tree,AStructsU),
        %writeln(AStructsU).
        maplist(write_unode,AStructsU),
        debug(anatomy,'Done!',[]),
        fail.


%% unode_facts(+Node,?Facts) det
% Node = SetID-[C1,C2,...]
% if ID belongs to an existing AO, omit facts
unode_facts(ID-_,[]):-
        belongs(ID,_),
        !.

%% if ID not in existing ontology, then
% make up ontol_db facts
% TODO: DRY (or retire previous code..)
unode_facts(ID-Set,[class(ID),entity_resource(ID,uberon),metadata_db:entity_comment(ID,'This class was created automatically from a combination of ontologies'),metadata_db:entity_label(ID,Label),def(ID,CombinedDef)|Facts]):-
        best_label(Set,Label),
        findall(Def,(member(X,Set),def(X,Def)),Defs),
        concat_atom(Defs,' // ',CombinedDef),
        solutions(def_xref(ID,Xref),(member(X,Set),def_xref(X,Xref)),DefXrefFacts),
        solutions(metadata_db:entity_xref(ID,X),member(X,Set),XrefFacts),
        solutions(metadata_db:entity_synonym(ID,Syn),(member(X,Set),class_label(X,Syn,_),Syn\=Label),SynFacts1),
        solutions(metadata_db:entity_synonym_scope(ID,Syn,Scope),(member(X,Set),class_label(X,Syn,Scope),Syn\=Label),SynFacts2),
        %solutions(Fact,relfact(Set,IndexedSets,Fact),RelFacts),
        RelFacts=[],
        flatten([DefXrefFacts,XrefFacts,SynFacts1,SynFacts2,RelFacts],Facts).

%% build_ontology(+PNode,?NodeXrefsPairs) semidet
% PNode = foo(X,Y)
% if PNode is intermediate node:
%   build ontology for child nodes
build_ontology(PNode,AStructsAllSorted):-
        PNode =.. [_Label,Child1,Child2],
        !,
        debug(anatomy,'building ontology for clade: ~w',[PNode]),
        build_ontology(Child1,AStructs1),
        build_ontology(Child2,AStructs2),
        length(AStructs1,Len1),
        length(AStructs2,Len2),
        debug(anatomy,'finding union of sub-ontologies: ~w [~w,~w]',[PNode,Len1,Len2]),
        findall(AStructU/N1-N2,
                match_nodes(AStructs1,AStructs2,N1,N2,AStructU),
                AStructsUX),
        % new union nodes
        findall(AStruct,member(AStruct/_-_,AStructsUX),AStructsU),
        % we still want to include unmatched nodes from 2 input sets;
        % but we don't want absorbed nodes re-occurring
        findall(AStruct,(member(AStruct,AStructs1),(Class-_) = AStruct,(\+ member(_/Class-_,AStructsUX))),AStructs1R),
        findall(AStruct,(member(AStruct,AStructs2),(Class-_) = AStruct,(\+ member(_/_-Class,AStructsUX))),AStructs2R),
        length(AStructs1R,Len1R),
        length(AStructs2R,Len2R),
        debug(anatomy,'nodes remaining: ~w [~w,~w]',[PNode,Len1R,Len2R]),
        flatten([AStructs1R,AStructs2R,AStructsU],AStructsAll),
        sort(AStructsAll,AStructsAllSorted),
        debug(anatomy,'got union: ~w = ~w',[PNode,AStructsU]).


% if PNode is leaf node,
% e.g. human,[FMA:123-[ZF:345,FB:987, ...], ..]
build_ontology(PNode,AStructs):-
        label_ontology(PNode,Ont), % map to namespace
        debug(anatomy,'leaf: ~w',[Ont]),
        findall(AStruct-Xrefs,(belongs(AStruct,Ont),
                             findall(Xref,xrefR(AStruct,Xref),Xrefs)),
                AStructs).

%% match_nodes(+AStructs1,+AStructs2,?N1,?N2,?Union-XrefsU) nondet
% AStruct = NodeID-Xrefs
% true if homology holds between 2 xrefs in either set;
% make a new parent anatomy node whose xrefs are the union of
% the initial xrefs
% Example:
%  match_nodes(Nodes1,Nodes2,?,?,?)
%  Nodes1 has member a-[a1,a2,a3]
%  Nodes2 has member b-[b2,b4]
%  N1=a, N2=b
%  new-ab-[a1,a2,a3,b2,b4]
match_nodes(AStructs1,AStructs2,N1,N2,Union-XrefsU):-
        member(N1-Xrefs1,AStructs1),
        member(X1,Xrefs1),
        homologous_toS(X1,X2),
        member(N2-Xrefs2,AStructs2),
        member(X2,Xrefs2),
        flatten([Xrefs1,Xrefs2],XrefsU1),
        sort(XrefsU1,XrefsU),
        gensetid([N1,N2],Union),
        %gensetid(XrefsU,Union),
        debug(anatomy,'union: ~w',[Union]).

beats(exact,_):- !.
beats('exact syn',X):- X\=exact,!.
beats(_,_).

xxbestmatch(X,Y):-
        match(X,Y,_).

bestmatch(X,Y):-
        match(X,Y,Type),
        \+ ((match(X2,Y,Type2),
             X2\=X,
             beats(Type2,Type))),
        \+ ((match(X,Y2,Type2),
             Y2\=Y,
             beats(Type2,Type))).

        
matches_to_obo:-
        forall(bestmatch(X,Y),writematch(obo,X,Y)),
        writeln('!!'),
        debug(anatomy,'Done!',[]),
        fail.


writematch(obo,X,Y):-
        label_or_id(X,XN),
        label_or_id(Y,YN),
        format('[Term]\nid: ~w ! ~w\nrelationship: OBO_REL:homologous_to ~w ! ~w~n~n',[X,XN,Y,YN]).

% e.g. UBERON to TAO vs ZFIN
xref_extend(C1,C2,O1,O2):-
        entity_xref(C1,X),
        belongs(C1,O1),
        entity_xref(C2,X),
        belongs(C2,O2).

%  blip -debug anatomy -r zebrafish_anatomy -r xenopus_anatomy -r uberon -u query_anatomy -i zfa-xao-aln.tbl findall extend_ontology/0
extend_ontology:-
	new_class_from_aln(Facts),
        debug(anatomy,'    facts: ~w',[Facts]),
        maplist(write_db_fact,Facts),
	fail.

valid_ext(Set):-
	eq(_,A,B,_,_,_,_),
	\+ mapped(A),
	\+ mapped(B),
	A @< B,
	Set=[A,B].

new_class_from_aln([class(ID),entity_resource(ID,uberon),metadata_db:entity_comment(ID,'This class was created automatically from a combination of ontologies'),metadata_db:entity_label(ID,Label),def(ID,CombinedDef)|Facts]):-
	valid_ext(Set),
        gensetid(Set,ID),
        best_label(Set,Label),
        debug(anatomy,'set: ~w ~w',[ID,Label]),
        findall(Def,(member(X,Set),def(X,Def)),Defs),
        concat_atom(Defs,' // ',CombinedDef),
        findall(def_xref(ID,Xref),(member(X,Set),def_xref(X,Xref)),DefXrefFacts),
        findall(metadata_db:entity_xref(ID,X),member(X,Set),XrefFacts),
        findall(metadata_db:entity_xref(ID,Y),(member(X,Set),entity_xref(X,Y),sub_atom(Y,0,_,_,'CARO')),XrefFacts2),
        findall(metadata_db:entity_synonym(ID,Syn),(member(X,Set),class_label(X,Syn,_),Syn\=Label),SynFacts1),
        findall(metadata_db:entity_synonym_scope(ID,Syn,Scope),(member(X,Set),class_label(X,Syn,Scope),Syn\=Label),SynFacts2),
	findall(ontol_db:subclass(ID,PID),(member(X,Set),subclass(X,PX),uberon_mod(PID,PX)),SubClassFacts),
	findall(ontol_db:restriction(ID,R,PID),(member(X,Set),restriction(X,R,PX),uberon_mod(PID,PX)),RelFacts),
        flatten([DefXrefFacts,XrefFacts,XrefFacts2,SynFacts1,SynFacts2,SubClassFacts,RelFacts],Facts).

uberon_mod(U,C):-
	class_xref(U,C),
	belongs(U,uberon).
% (?,+)
uberon_mod(U,C):-
	valid_ext(Set),
	member(C,Set),
	gensetid(Set,U).
mapped(C):-
	class_xref(U,C),
	belongs(U,uberon).

suggest_merge(A,B):-
        class_xref(A,X),
        class_xref(B,X),
        A\=B.

%emapa_superclass(C,P):-
%        class(C,N),
%        class(C2,N),
%        C2\=C,
%        foo.


 
class_wikipage(C,Match):-
        % ensure_loaded(bio(web_fetch_wikipedia)),
        class(C,N),
        sleep(1),
        (   web_search_wikipedia(N,[Page],[])
        ->  Match=N
        ;   entity_synonym_scope(C,S,exact),
            web_search_wikipedia(S,[Page],[])
        ->  Match=S
        ;   fail).

        
        
fix_ehdaa(C,N,N2) :-
        class(C,N),
        class(C2,N),
        C\=C2,
        restriction(C,_,P),
        class(P,PN),
        concat_atom([N,', ',PN],N2).

ubmatch(X,Y):-
        same_label_as(X,Y).
ubmatch(X,Y):-
        class_xref(Y,X),
        \+ ((class_xref(Y2,X),
             Y2\=Y)).


ehda_uberon_xp(Fact) :-
        class(A,N),
        belongs(A,Ont),
        Ont \= uberon,
        restriction(A,part_of,P),
        %class(A2,N),
        ubmatch(A,A2),
        belongs(A2,uberon),
        \+ \+ ((class(AX,N),
                AX\=A,
                belongs(AX,Ont))),
        class(P,PN),
        ubmatch(P,P2),
        belongs(P2,uberon),
        solutions(PXN,(class(AX,N),AX\=A,belongs(AX,Ont),restriction(AX,part_of,PX),class(PX,PXN)),PXNs),
        concat_atom(PXNs,', ',PXNA),
        sformat(SXN,'~w of ~w',[N,PN]),
        sformat(SX,'~w^part_of(~w)',[A2,P2]),
        string_to_atom(SXN,XN),
        string_to_atom(SX,X),
        \+ ((genus(X2,A2),
             differentium(X2,part_of,P2))),
        member(Fact,
               [ontol_db:class(X),
                metadata_db:entity_label(X,XN),
                metadata_db:entity_xref(X,A),
                metadata_db:entity_comment(X,PXNA),
                ontol_db:genus(X,A2),
                ontol_db:differentium(X,part_of,P2)]).

% generate new classes G2^R(P2)
% based on ssAOs. here
% G2 corresponds to G in ssAO, and P2 to P in ssAO
% where ssAO has G^R(P)
uberon_xp(Fact) :-
        genus(A,G),
        (   entity_xref(G2,G)
	->  true
	;   id_idspace(G,'UBERON'),
	    G2=G),		% allow eg fma_xp_uberon
        \+ entity_xref(_,A),
	\+ id_idspace(A,'UBERON'),
	debug(uberon,'candidate: ~w',[A]),
        differentium(A,R,P),
        (   entity_xref(P2,P)
	;   id_idspace(P,'UBERON'),
	    P2=P),
        \+ ((genus(A2,G2),
	     id_idspace(A2,'UBERON'),
             differentium(A2,_,P2))),
        sformat(SX,'~w^part_of(~w)',[G2,P2]),
        string_to_atom(SX,X),
        class(A,XN),
        member(Fact,
               [ontol_db:class(X),
                metadata_db:entity_label(X,XN),
                metadata_db:entity_xref(X,A),
                ontol_db:genus(X,G2),
                ontol_db:differentium(X,R,P2)]).

uberon_xp_align(metadata_db:entity_xref(A2,A)) :-
        genus(A,G),
        entity_xref(G2,G),
        \+ entity_xref(_,A),
        differentium(A,_R,P),
        entity_xref(P2,P),
        genus(A2,G2),
        differentium(A2,_,P2).

uberon_xp_align(metadata_db:entity_xref(A2,A)) :-
        genus(A,G),
        entity_xref(G2,G),
        \+ entity_xref(_,A),
        differentium(A,_R,P),
        entity_xref(P2,P),
        genus(A2,G2),
        differentium(A2,_,P2).

uberon_xp_align(A2,A) :-
        genus(A,G),
        entity_xref(G2,G),
        \+ entity_xref(_,A),
        differentium(A,_R,P),
        entity_xref(P2,P),
        genus(A2,G2),
        differentium(A2,_,P2).

% find xrefs based on xp def
uberon_xp_align_write :-
        genus(A,G),
        (   entity_xref(G2,G)
	;   id_idspace(G,'UBERON'),
	    G2=G),		% fma_xp_uberon
        \+ entity_xref(_,A),
        differentium(A,_R,P),
        entity_xref(P2,P),
        genus(A2,G2),
        differentium(A2,_,P2),
        class(A2,A2N),
        class(A,AN),
        writeln('[Term]'),
        format('id: ~w ! ~w~n',[A2,A2N]),
        format('xref: ~w ! ~w~n',[A,AN]),
        nl,
        fail.

phenotype_class(X) :- id_idspace(X,'MP').
phenotype_class(X) :- id_idspace(X,'HP').

class_from_pheno(N,AN) :-
	atom_concat('abnormal ',N1,N),
	atom_concat(AN,' morphology',N1).
class_from_pheno(N,AN) :-
	atom_concat('Abnormality of the ',AN,N).
class_from_pheno(N,AN) :-	% abnormal caput
	concat_atom([abnormal,AN],' ',N).
class_from_pheno(N,AN) :-	% 
	atom_concat('absent ',AN,N).

class_from_bp(N,AN) :-
	atom_concat(AN,' development',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' morphogenesis',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' formation',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' cavitation',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' closure',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' growth',N).


%% use MP and MP-XP
uberon_mpxp_write :-
	setof(MP-N,(class(MP,N),phenotype_class(MP)),MPNs),
	member(MP-N,MPNs),
	\+ \+ ((differentium(MP,_,EMAP),
		id_idspace(EMAP,'EMAP'))), % TEMPORARY - find replacements for EMAPs
	%\+ genus(MP,_), % undefined
	debug(phenotype,'pc: ~w ~w',[MP,N]),
	class_from_pheno(N,AN),
	debug(phenotype,'  name: ~w',[AN]),
	\+ ((entity_label_or_synonym(A,AN),
	     (	 belongs(A,uberon)
	     ;	 belongs(A,cell)))),
        writeln('[Term]'),
        format('id: NEW_~w~n',[MP]),
        format('name: ~w~n',[AN]),
	mpxp_def(MP,Def),
        format('def: "~w" [~w]~n',[Def,MP]),
	writeln('is_a: UBERON:0000061 ! anatomical structure'),
	write_mpxp_parents(MP),
	write_xrefs_for_name(AN),
	nl,
	fail.
uberon_mpxp_write :- !.

uberon_mpxp_write_defs :-
	setof(MP-N,(class(MP,N),id_idspace(MP,'MP')),MPNs),
	member(MP-N,MPNs),
	atom_concat('abnormal ',N1,N),
	atom_concat(AN,' morphology',N1),
	entity_label_or_synonym(A,AN),
	belongs(A,uberon),
	\+ def(A,_),
	mpxp_def(MP,Def),
	Def\='.',
        writeln('[Term]'),
        format('id: ~w~n',[A]),
        format('def: "~w" [~w]~n',[Def,MP]),
	nl,
	fail.
uberon_mpxp_write_defs.


mpxp_def(MP,Def) :-
	def(MP,D1),
	concat_atom([_,Def],'anomaly of ',D1),
	!.
mpxp_def(MP,Def) :-
	def(MP,D1),
	concat_atom([_,Def],'anomaly in ',D1),
	!.
mpxp_def(MP,Def) :-
	def(MP,D1),
	concat_atom([_,Def],'location of ',D1),
	!.
mpxp_def(MP,Def) :-
	def(MP,D1),
	concat_atom([_,Def],'presence of ',D1),
	!.
mpxp_def(MP,Def) :-
	def(MP,D1),
	concat_atom([_,Def],'structure of ',D1),
	!.
mpxp_def(_,'.').

goxp_def(MP,Def) :-
	def(MP,D1),
	concat_atom(L,'.',D1),
	reverse(L,[_,Def,_|_]),
	!.
goxp_def(_,'.').


write_mpxp_parents(A) :-
	subclass(A,B),
	differentium(B,_,Y),
	entity_xref(X,Y),
	belongs(X,uberon),
	(   class(X,XN)
	->  true
	;   XN = '?'),
	format('relationship: part_of ~w ! ~w~n',[X,XN]),
	fail.
write_mpxp_parents(_) :- !.

write_goxp_parents(A) :-
	parent(A,B),
	differentium(B,_,X),
	belongs(X,uberon),
	(   class(X,XN)
	->  true
	;   XN = '?'),
	format('relationship: part_of ~w ! ~w~n',[X,XN]),
	fail.
write_goxp_parents(_) :- !.


write_xrefs_for_name(N) :-
	entity_label_or_synonym(X,N),
	class(X,XN),
	format('xref: ~w ! ~w~n',[X,XN]),
	fail.
write_xrefs_for_name(_).

uberon_goxp_write :-
	setof(GO-N,(class(GO,N),belongs(GO,biological_process),
		    subclassT(GO,'GO:0032502')),
	GONs),
	member(GO-N,GONs),
	\+ genus(GO,_), % undefined
	class_from_bp(N,AN),
	\+ ((entity_label_or_synonym(A,AN),
	     (	 belongs(A,uberon)
	     ;	 belongs(A,cell)))),
        writeln('[Term]'),
        format('id: NEW_~w~n',[GO]),
        format('name: ~w~n',[AN]),
	goxp_def(GO,Def),
        format('def: "~w" [~w]~n',[Def,GO]),
	writeln('is_a: UBERON:0000061 ! anatomical structure'),
	write_goxp_parents(GO),
	write_xrefs_for_name(AN),
	nl,
	fail.
uberon_goxp_write :- !.
	
uberon_dv(XD,X,UX,YD,Y,UY) :-
	disjoint_from_violation(UX,UY,XD), % e.g. acellular, cellular, ZF:otolith
	entity_xref(UX,X),
	entity_xref(UY,Y),
	subclassRT(XD,X),
	id_idspace(XD,XDS),
	entity_xref(U,XD),
	entity_xref(U,YD),
	id_idspace(YD,YDS),
	XDS\=YDS,
	subclassRT(YD,Y).



                 