/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.7 $
  @date @cvskw $Date: 2007/06/09 21:45:23 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| blipkit_ontol_align - text alignment between ontologies

  @s1 Synopsis

  @cl
  :- use_module(blipkit_ontol_align).

  @/cl

  @s1 Description

**/

:- module(blipkit_ontol_align,[]).

:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(graph)).
:- use_module(bio(io)).
:- use_module(bio(av_db)).



blipkit:example('obol ontol-align -stem -r go -r cell > go-to-cell-alignment.tbl').
blipkit:example('obol ontol-align -debug ontol_align -stem -shownames -r go -r fma >  gocc-to-fma.tbl').

:- blip('ontol-align',
        'compare class names in ontologies. uses stems, synonyms',
        [atom(style,Style,table),
         bool(shownames,Shownames),
         bool(stem,Stem)],
        Onts,
        align_ontols(Style,Onts,[shownames(Shownames),
                                 stem(Stem)])).

:- blip('ontol-align-set',
        'uses user:align_ont_pair/2 to determine pairs to align',
        [atom(style,Style,table),
         bool(shownames,Shownames),
         bool(stem,Stem)],
        _,
        align_ontol_pairs(Style,[shownames(Shownames),
                                 stem(Stem)])).

align_ontol_pairs(Style,Opts):-
        solutions(Ont,(alignset_element(Ont);alignset_base(Ont)),Onts),
        solutions(Ont,(alignset_element(Ont)),OntsToCompare),
        debug(ontol_align,'Aligning: ~w',[Onts]),
        maplist(load_bioresource,Onts),
        index_classes_by_text,
        forall(alignset_base(BaseOnt),
               align_ontol(Style,BaseOnt,OntsToCompare,Opts)).

% special case for 1
align_ontols(Style,[Ont],Opts):-
        !,
        load_bioresource(obol_av),
        debug(ontol_align,'Indexing classes: ~w',[Opts]),
        index_classes_by_text,
        setof(Ont1,ID^belongs(ID,Ont1),Onts),
        debug(ontol_align,'Aligning: ~w vs ~w',[Ont,Onts]),
        align_ontol(Style,Ont,Onts,Opts).
align_ontols(Style,Onts1,Opts):-
        load_bioresource(obol_av),
        debug(ontol_align,'Indexing classes: ~w',[Opts]),
        index_classes_by_text,
        (   Onts1=[]
        ->  setof(Ont,ID^belongs(ID,Ont),Onts)
        ;   Onts=Onts1),
        debug(ontol_align,'Aligning: ~w',[Onts]),
        forall(member(Ont,Onts),
               align_ontol(Style,Ont,Onts,Opts)).

% align_ontol(+Style,+BaseOntology,+OtherOntologies,+Opts):-
align_ontol(Style,Ont1,Onts,Opts):-
        debug(ontol_align,'Aligning ont: ~w [~w]',[Ont1,Style]),
        forall(belongs(ID1,Ont1),
               align_class(Style,ID1,Ont1,Onts,Opts)).

align_class(table,ID1,Ont1,Onts,Opts):-
        debug(ontol_align,'  Aligning class: ~w',[ID1]),
        forall_distinct((   class_match(ID1,ID2,ST1-S-ST2),
                            belongs(ID2,Ont2),
                            \+ Ont2=Ont1,
                            member(Ont2,Onts)),
               % write row for every ID match pair
               (   writecols([S,ST1-ST2,ID1,ID2,Ont1,Ont2]),
                   (   member(shownames(1),Opts)
                   ->  class(ID1,N1),
                       class(ID2,N2),
                       writecols(['',N1,N2])
                   ;   true),
                   nl)).

score_match(N1,N2,S):-
        cached_atom_to_stem_list(N1,L1),
        cached_atom_to_stem_list(N2,L2),
        concat_atom(L1,N1B),
        concat_atom(L2,N2B),
        score_match1(N1B,N2B,S).
        
score_match1(N,N,eq):- !.
score_match1(N1,N2,substr):-
        sub_atom(N2,Start,_,_,N1),
        \+ trivial_match(N2,N1),
        % we wish to prevent matches like "nucleate[D cell]"
        % the substr should either be at the beginning, or the
        % start of a word (preceeded by spc)
        (   Start=0
        ->  true                % substring at start of word
        ;   Prev is Start-1,
            sub_atom(N2,Prev,1,_,' ')). % start of word

% a trivial match is a 3-char atom that is not a complete word
% in the match sentence
trivial_match(S,Sub):-
        atom_length(Sub,3),
        concat_atom(Toks,' ',S),
        \+ member(Sub,Toks).

%% class_match(+ID1,?ID2,?S)
% S = SynType1-S-SynType2
class_match(ID1,ID2,SynType1-S-SynType2):-
        class_label_or_reladj(ID1,N1,SynType1),
        sub_atom(N1,0,3,_,Sub), % first 3 chars must match
        setof(ID2,cached_substr(Sub,ID2),ID2s),
        member(ID2,ID2s),       % only count each matching ID at most once
        class_label_or_reladj(ID2,N2,SynType2),
        \+ ID1=ID2,
        score_match(N1,N2,S),   % check if a real match or just seed match
        belongs(ID1,O1),
        belongs(ID2,O2),
        \+ O1=O2.

class_label_or_reladj(ID,N,SynType):-
        class_label(ID,N,SynType).
class_label_or_reladj(ID,N,SynType):-
        class_label(ID,N1,SynType),
        concat_atom(Toks1,' ',N1),
        maplist(adj2noun,Toks1,Toks2),
        concat_atom(Toks2,' ',N),
        N\=N1.

adj2noun(A,N):- relational_adj_ra(A,N,_),!.
adj2noun(N,N).

%:- dynamic cached_substr/2.
index_classes_by_text:-
        !,
        use_module(library(porter_stem),[atom_to_stem_list/2]),
        debug(blip,'indexing classes by text',[]),
        forall(belongs(ID,_),
               index_class_by_text(ID)),
        compile_predicates([cached_substr/2]),
        compile_predicates([cached_atom_to_stem_list/2]),
        debug(blip,'classes now indexed by text',[]).
        
index_class_by_text(ID):-
        class(ID),
        !,
        forall_distinct(class_label(ID,Syn,_),
                        index_text(ID,Syn)),
        !.
index_class_by_text(_).         % no name known

xxxxxxxxxxxxxxxindex_class_by_text(ID):-
        class(ID,N),
        !,
        index_text(ID,N),
        forall(synonym(ID,_,Syn),
               index_text(ID,Syn)),
        !.


        

index_text(ID,N):-
        forall(sub_atom(N,_,3,_,Sub),
               assert(cached_substr(Sub,ID))),
        atom_to_stem_list(N,Toks),
        assert(cached_atom_to_stem_list(N,Toks)),
        !.
index_text(ID,N):-
        throw(index_text(ID,N)).

