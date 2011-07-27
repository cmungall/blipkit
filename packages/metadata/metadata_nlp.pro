:- module(metadata_nlp,
	  [
	   term_token/2,
	   term_token_stemmed/3,
	   term_nth_token/3,
	   term_nth_token_stemmed/4,
	   entity_label_token/2,
	   entity_label_token/3,
	   entity_label_token_stemmed/4,
	   entity_nlabel_scope_stemmed/4,
           term_tokenset_stemmed/3,
           label_lexical_variant/2,
	   token_syn/2,
	   corpus_label_token/1,
	   corpus_label_token_frequency/2,
	   simindex_labeled_entities/1,
	   simindex_labels/1,
	   simindex_corpus_by_labels/1,
	   simindex_corpus_by_labels/4,
	   entity_pair_nlp_subset_of/3,
	   entity_pair_nlp_subset_of_cross_idspace/3,
	   entity_pair_nlp_match/7,
           entity_pair_simple_label_match/3,
           index_entity_pair_label_match/0,
           nlp_index_all/0,
	   entity_pair_label_match/2,
	   entity_pair_label_match/3,
	   entity_pair_label_match/5,
	   entity_pair_label_match/6,
           entity_pair_label_best_match/3,
           entity_pair_label_reciprocal_best_match/3,
           entity_pair_label_intermatch/5,
           entity_pair_label_best_intermatch/3,
           entity_pair_label_reciprocal_best_intermatch/3,
	   atom_search/5,
	   corpus_search/6,
           entity_label_token_list_stemmed/4,
           label_query_results/3,
           label_query_best_result/4,
           label_full_parse/3,

           term_split/6,
           term_split_over/7,
           term_split_over_strict/4,
           term_ends_with/6,
           add_synonyms_minus_words/1,
           label_template_match/2,
           label_template_match/3,
           entity_label_template_match/3,
           entity_label_template_match/5
	   ]).

:- use_module(metadata_db).
:- use_module(bio(simmatrix)).
:- use_module(bio(index_util)).
:- use_module(bio(ontol_db)). % consider moving the one dependency on this
:- use_module(library(porter_stem)).
:- use_module(bio(av_db)).  % obol dependency - consider moving this

tokenize_atom_wrap(A,TL) :-
        tokenize_atom(A,TL_1),
        findall(T,(member(T,TL_1),\+ignore_token(T)),TL).

ignore_token(',').
ignore_token('.').
ignore_token('-').
ignore_token(';').
ignore_token(':').
ignore_token('_').


term_token(A,T) :-
	tokenize_atom_wrap(A,TL),
	member(T,TL).

term_token_stemmed(A,T) :-
	term_token_stemmed(A,T,true).

term_token_stemmed(A,T,true) :-
        % acronym
        \+ ((sub_atom(A,_,1,_,C),
             C@>='a',
             C@=<'z')),
        !,
        % we still convert to lowercase for matching purposes,
        % but we don't stem.
        % this is still not ideal: someone can search with
        % an acronym, but the porter stemming part may have truncated the acronym
        % already, so best to stem anyway??
        % ideally this would be a hook in the porter stemming..
        downcase_atom(A,A_dn),
        term_token_stemmed(A_dn,T,false).
term_token_stemmed(A,T,true) :-
        % not acronym
	term_token(A,T1),
	custom_porter_stem(T1,T).
term_token_stemmed(A,T,false) :-
	term_token(A,T).

custom_porter_stem(T,S) :-
        atom_concat(X,eous,T),
        atom_concat(X,eus,T2),
        !,
        porter_stem(T2,S).
custom_porter_stem(T,S) :-
        porter_stem(T,S).



term_nth_token(A,N,T) :-
	tokenize_atom_wrap(A,TL),
	nth(N,TL,T).

term_nth_token_stemmed(A,N,T) :-
	term_nth_token_stemmed(A,N,T,true).
term_nth_token_stemmed(A,N,T,true) :-
	term_nth_token(A,N,T1),
	porter_stem(T1,T).
term_nth_token_stemmed(A,N,T,false) :-
	term_nth_token(A,N,T).

%% entity_label_token(?E,?Tok) is nondet
% see entity_label_token/3
entity_label_token(E,T) :-
	entity_label_token(E,_,T).

%% entity_label_token(?E,?Label,?Tok) is nondet
% each entity has multiple labels, and each label has multiple tokens
entity_label_token(E,A,T) :-
	entity_label_or_synonym(E,A),
	term_token(A,T).

%% entity_label_token_stemmed(?Entity,?Token,?Stemmed:boolean)
entity_label_token_stemmed(E,T,Stemmed) :-
	entity_label_token_stemmed(E,_,T,Stemmed).

%% entity_label_token_stemmed(?Entity,?LabelOrSyn,?Token,?Stemmed:boolean)
entity_label_token_stemmed(E,A,T,true) :-
	entity_label_or_synonym(E,A),
	term_token_stemmed(A,T,true).
entity_label_token_stemmed(E,A,T,false) :-
	entity_label_or_synonym(E,A),
	term_token(A,T).

%% entity_nlabel_scope_stemmed(?Entity,?Token,?Type,?Stemmed:boolean)
% performs normalization on labels
%  * dehyphenate/2
%  * uses synset/1 (which also uses obol relational adjectives)
%  * canonical token ordering
%  * removes all whitespaces
% note that relational adjectives will not work in stemmed mode
entity_nlabel_scope_stemmed(E,A,Scope,St) :-
	entity_label_scope_ext(E,L,Scope),
	\+ exclude_entity(E),
	debug(nlp,'calculating nlabel ~w "~w" (~w)',[E,L,Scope]),
        term_nlabel_stemmed(L,A,St).

% consider moving this elsewhere, it introduces a dependency on ontol package.
% the idea is to make a synonym using a superclass, as some subclasses
% may use an abbreviated label assuming the parent is implicit. E.g.
%          is_a FMA:49033 ! Extra-ocular muscle ***
%            is_a FMA:49035 ! Superior rectus
entity_label_scope_ext(E,L,Scope) :-
	entity_label_scope(E,L,Scope).
entity_label_scope_ext(E,L,mixed-Scope) :-
	entity_label_scope(E,L1,Scope1),
        parent_entity_hook(E,E2),
	entity_label_scope(E2,L2,Scope2),
        combine_scope(Scope1,Scope2,Scope),
        debug(nlp_detail,'~w SYN: ~w + ~w',[E,L1,L2]),
        atomic_list_concat([L2,L1],' ',L).

combine_scope(label,label,exact) :- !.
combine_scope(exact,exact,exact) :- !.
combine_scope(label,exact,exact) :- !.
combine_scope(exact,label,exact) :- !.
combine_scope(_,_,related).

% see metadata_nlp_parent_dist2_hook.pro for example
:- multifile parent_entity_hook/2.





% HOOK
:- multifile exclude_entity/1.

%% term_nlabel_stemmed(+Term:atom,?NLabel:atom,+Stemmed:bool) 
%
% translates a term such as 'foo bar two hippocampi' to a label like
% '2barfoohippocamp'
% this is used for exact matching, but is not useful for finding labels
% nested inside larger labels or blocks of text.
%
% canonical ordering  fails in certain cases: e.g. 'chordo neural hinge' vs 'chordo neural hinge',
% so we also provide the same ordering too
term_nlabel_stemmed(Term,NLabel,St) :-
        term_tokenset_stemmed(Term,Toks,St),
        debug(nlp,'  term:~w ==> ~w',[Term,Toks]),
	concat_atom(Toks,'',NLabel).
% original ordering
term_nlabel_stemmed(Term,NLabel,St) :-
	dehyphenate(Term,TermNoHyphen), % nd
        (   St=true;St=false),
        findall(Tok,term_token_stemmed(TermNoHyphen,Tok,St),Toks_1),
        % both with replacements ( if different) and without
	(   maplist(token_syn_refl,Toks_1,Toks_2),
            Toks_1\=Toks_2,
            concat_atom(Toks_2,'',NLabel)
        ;   concat_atom(Toks_1,'',NLabel)).


%% term_tokenset_stemmed(+Term:atom,?Toks:list,+Stemmed:boolean)
% 
% translates a term such as 'foo bar two hippocampi' to a set such as
% [2,bar,foo,hippocamp]
term_tokenset_stemmed(Term,Toks,St) :-
	dehyphenate(Term,TermNoHyphen), % nd
        setof(Tok,term_token_stemmed(TermNoHyphen,Tok,St),Toks_1),
	maplist(token_syn_refl,Toks_1,Toks_2), % normalize using synsets - nondet (MAKE DET?)
	sort(Toks_2,Toks).  % need to sort again, synsets may introduce re-ordering

label_lexical_variant(Term,Variant) :-
        atomic_list_concat(Toks,' ',Term),
        maplist(nd_token_syn_refl,Toks,ToksV),
        findall(Tok,(member(Tok,ToksV),Tok\=''),ToksV2),
        atomic_list_concat(ToksV2,' ',Variant).
label_lexical_variant(Term,Variant) :-
        dehyphenate(Term,Term2), % todo - all possible variations
        Term2\=Term,
        label_lexical_variant(Term2,Variant).
label_lexical_variant(Term,Variant) :-
        downcase_atom(Term,X),
        X\=Term,
        label_lexical_variant(X,Variant).

        

dehyphenate(X,X).
dehyphenate(X,Y) :- concat_atom(L,'-',X),L\=[_],concat_atom(L,'',Y).


% TODO: allow custom synsets
synset(['1','1st',first,'I','01']).
synset(['2','2nd',second,'II','02']).
synset(['3','3rd',third,'III','03']).
synset(['4','4th',fourth,'IV','04']).
synset(['5','5th',fifth,'V','05']).
synset(['6','6th',sixth,'VI','06']).
synset(['7','7th',seventh,'VII','07']).
synset(['8','8th',eight,'VIII','08']).
synset(['9','9th',ninth,'IX','09']).
synset(['10','10th',tenth,'X']).
synset(['11','11th',eleventh,'XI']).
synset(['12','12th',twelfth,'XII']).
synset(['13','13th',thirteenth,'XIII']).
synset(['20','20th',twentyth,'XX']).

synset([caudal,posterior]).
synset([rostral,anterior]).
synset([dorsal,superior]).
synset([ventral,inferior]).
synset([future,presumptive]).
synset([division,segment]).

% TODO: make this configurable, otherwise too many false positives
synset(['S',sacral]).
synset(['L',lumbar]).
synset(['T',thoracic]).
synset(['C',cervical]).


% eliminate prepositions. assumes we flatten without spaces
synset(['',P]) :- prep(P).
synset(['',a]).
synset(['',an]).
synset(['',the]).
synset(['','-']).
synset(['','/']).
synset(['',':']).


:- multifile synset_hook/1.
synset(X) :- synset_hook(X).

prep(of).
prep(to).
prep(by).


/*
  NONDET
% consider indexing
token_syn(T,S) :- synset(L),member(T,L),member(S,L),T\=S.
token_syn(T,S) :- relational_adj(T,S,_,_).
token_syn(T,S) :- relational_adj(S,T,_,_).
*/

% DET
token_syn(T,S) :- relational_adj(T,S,_,_),!.   % ensure noun-form is used preferentially
token_syn(T,S) :- synset([S|L]),member(T,L).

% reflexive
token_syn_refl(T,S) :- token_syn(T,S),!.
token_syn_refl(T,T) :- nonvar(T).

% non-det. also enumerate all possible synset members
nd_token_syn(T,S) :- relational_adj(T,S,_,_).
%nd_token_syn(T,S) :- relational_adj(S,T,_,_).
nd_token_syn(T,T2) :- synset(Syns),member(T,Syns),member(T2,Syns),T\=T2.

% reflexive
nd_token_syn_refl(T,S) :- nd_token_syn(T,S).
nd_token_syn_refl(T,T) :- nonvar(T).

corpus_label_token(T) :-
	entity_label_token(_,T).
corpus_label_token_stemmed(T,Stemmed) :-
	entity_label_token_stemmed(_,T,Stemmed).

% consider using simmatrix instead
corpus_label_token_frequency(T,Freq) :-
	aggregate(count,E,entity_label_token(E,T),Freq).
corpus_label_token_stemmed_frequency(T,Freq,Stemmed) :-
	aggregate(count,E,entity_label_token_stemmed(E,T,Stemmed),Freq).

% todo?
entity_pair_nlp_match(A,B,label,LA,LB,Stemmed,J-Sim) :-
	entity_label_token_stemmed(A,LA,Token,Stemmed),
	entity_label_token_stemmed(B,LB,Token,Stemmed),
	A\=B,
	feature_pair_simj(A,B,J),
	J>0.5,
	feature_pair_simGIC(A,B,Sim).

%% entity_pair_nlp_subset_of(A,B,S)
% ensure index_labels/1 is called first
entity_pair_nlp_subset_of(A,B,S) :-
	var(B),
	entity_label(A,AN),
	feature_pair_subset_of(AN,BN,S),
	entity_label(B,BN),
	A\=B.
entity_pair_nlp_subset_of(A,B,S) :-
	nonvar(B),
	entity_label(B,BN),
	feature_pair_subset_of(AN,BN,S),
	entity_label(A,AN),
	A\=B.


entity_pair_nlp_subset_of_cross_idspace(A,B,S) :-
	entity_pair_nlp_subset_of(A,B,S),
	id_idspace(A,AO),
	id_idspace(B,BO),
	AO\=BO.

nlp_index_all :-
	materialize_index(entity_nlabel_scope_stemmed(1,1,-,-)),
	materialize_index(entity_label_token_stemmed(1,-,1,-)),
	materialize_index(entity_label_token_list_stemmed(1,-,-,-)).


entity_pair_simple_label_match(A,B,N) :-
        entity_label_or_synonym(A,N),
        entity_label_or_synonym(B,N).

        
index_entity_pair_label_match :-
        materialize_index(entity_nlabel_scope_stemmed(1,1,-,-)).

%% entity_pair_label_match(?A,?B)
%% entity_pair_label_match(?A,?B,?Stemmed)
%% entity_pair_label_match(?A,?B,?Stemmed,?ScopeA,?ScopeB)
%% entity_pair_label_match(?A,?B,?Stemmed,?ScopeA,?ScopeB,?NormalizedLabel)
%
% true if A and B share labels/synonyms, and neither are obsolete
entity_pair_label_match(A,B) :-
	entity_pair_label_match(A,B,true).
entity_pair_label_match(A,B,Stemmed) :-
	entity_nlabel_scope_stemmed(A,N,_ScA,Stemmed),
	entity_nlabel_scope_stemmed(B,N,_ScB,Stemmed),
        A\=B,
        \+ entity_obsolete(A,_),
        \+ entity_obsolete(B,_).
entity_pair_label_match(A,B,Stemmed,ScA,ScB) :-
        entity_pair_label_match(A,B,Stemmed,ScA,ScB,_).
entity_pair_label_match(A,B,Stemmed,ScA,ScB,N) :-
	entity_nlabel_scope_stemmed(A,N,ScA,Stemmed),
	entity_nlabel_scope_stemmed(B,N,ScB,Stemmed),
        A\=B,
        \+ entity_obsolete(A,_),
        \+ entity_obsolete(B,_).

entity_pair_label_intermatch(A,B,Stemmed,ScA,ScB) :-
        entity_pair_label_match(A,B,Stemmed,ScA,ScB),
        id_idspace(A,SA),
        id_idspace(B,SB),
        SA\=SB.

%% entity_pair_label_best_match(A,B,Stemmed)
%
% true if A and B share labels, and there is no B'
% such that B' is a better match for A, where better
% match is defined in terms of synonym scope
% (exact syns or labels beat other syns).
%
% note that this is asymmetric; a1 may have best match
% with b1, but b1 may have a better match a2
entity_pair_label_best_match(A,B,Stemmed) :-
        entity_pair_label_match(A,B,Stemmed,ScA,ScB),
        % there exists no better match to another term B2
        \+ ((entity_pair_label_match(A,B2,Stemmed,ScA2,ScB2),
             B2\=B,
             scope_pair_better_than(ScA2,ScB2,ScA,ScB))).


%% entity_pair_label_reciprocal_best_match(A,B,Stemmed)
%
% as entity_pair_label_best_match/3, but only holds
% if base relation holds in both directions
entity_pair_label_reciprocal_best_match(A,B,Stemmed) :-
        entity_pair_label_best_match(A,B,Stemmed),
        entity_pair_label_best_match(B,A,Stemmed).

%% entity_pair_label_reciprocal_best_match(A,B,Stemmed)
%
% as entity_pair_label_best_match/3, but A and B must be in different
% ID spaces
entity_pair_label_best_intermatch(A,B,Stemmed) :-
        entity_pair_label_intermatch(A,B,Stemmed,ScA,ScB),
        \+ ((entity_pair_label_intermatch(A,B2,Stemmed,ScA2,ScB2),
             B2\=B,
             scope_pair_better_than(ScA2,ScB2,ScA,ScB))).

%% entity_pair_label_reciprocal_best_intermatch(A,B,Stemmed)
%
% as entity_pair_label_best_intermatch/3, (i.e. A and B
% must be in different ID spaces) but only holds
% if base relation holds in both directions
entity_pair_label_reciprocal_best_intermatch(A,B,Stemmed) :-
        entity_pair_label_best_intermatch(A,B,Stemmed),
        entity_pair_label_best_intermatch(B,A,Stemmed).

scope_pair_better_than(X1,X2,Y1,Y2) :-
        X1-X2 \= Y1-Y2,
        scope_better_than_or_eq(X1,Y1),
        scope_better_than_or_eq(X2,Y2).

scope_better_than_or_eq(X,X) :- !.
scope_better_than_or_eq(mixed-X,mixed-Y) :-
        !,
        scope_better_than_or_eq(X,Y).
scope_better_than_or_eq(_,mixed-_) :- !.
scope_better_than_or_eq(mixed-_,_) :- !,fail.
scope_better_than_or_eq(X,Y) :-
        (X=label;X=exact),
        \+((Y=label;Y=exact)).


simindex_labels(Stemmed) :-
	debug(nlp,'indexing [stemmed:~w]',[Stemmed]),
	generate_term_indexes(Label,Token,
			      metadata_nlp:entity_label_token_stemmed(_,Label,Token,Stemmed)).

% DON'T USE THIS!! only use if each entity has a single label
simindex_labeled_entities(Stemmed) :-
	debug(nlp,'indexing entities [stemmed:~w]',[Stemmed]),
	generate_term_indexes(E,Token,
			      entity_label_token_stemmed(E,_,Token,Stemmed)).


% index is per-label rather than per-entity
simindex_corpus_by_labels(Stemmed):-
	!,
	simindex_labels(Stemmed),
	debug(nlp,'force cache...',[]),
	findall(F,feature_vector(F,_),_),
	% for frequencies we count by entity, not label
	debug(nlp,'calculating freqs [stemmed:~w]',[Stemmed]),
	retractall(simmatrix:template(_,_,_)),
        assert(simmatrix:template(Entity,Token,
				  metadata_nlp:entity_label_token_stemmed(Entity,_,Token,Stemmed))),
	materialize_index(simmatrix:attribute_feature_count(1,1)).

%% simindex_corpus_by_labels(Entity,Term,Goal,Stemmed)
%
% e.g. simindex_corpus_by_labels(Gene,Rif,gene_rif(_,Gene,_,_,Rif),true)
%
% main index is constructed from ontology
simindex_corpus_by_labels(Entity,Term,Goal,Stemmed):-
	!,
	debug(nlp,'indexing [stemmed:~w]',[Stemmed]),
        % generate initial index using ontology
	generate_term_indexes(Label,Token,
			      entity_label_token_stemmed(_,Label,Token,Stemmed)),
	debug(nlp,'force cache...',[]),
	findall(F,feature_vector(F,_),_),
	% for frequencies we count by entity (e.g. class), not label
	debug(nlp,'calculating freqs [stemmed:~w]',[Stemmed]),
	retractall(simmatrix:template(_,_,_)),
        assert(simmatrix:template(Entity,Token,
				  (   Goal,
				      term_token_stemmed(Term,Token,Stemmed)))),
	materialize_index(simmatrix:attribute_feature_count(1,1)).


% after indexing a database of terms, use corpus as query and find matches
corpus_search(_QueryTemplate,TermTemplate,Goal,HitLabel-HitToks,Sim,Stemmed) :-
	Goal,
	atom_search(TermTemplate,HitLabel,HitToks,Sim,Stemmed).


atom_search(TermTemplate,HitLabel,HitToks,Sim,Stemmed) :-
	debug(nlp,'testing ~w [stemmed:~w]',[TermTemplate,Stemmed]),
	setof(T,term_token_stemmed(TermTemplate,T,Stemmed),Ts),
	debug(nlp,'   query toks: ~w',[Ts]),
	setof(Num,T^AI^(member(T,Ts),simmatrix:attribute_ix(T,AI),Num is 2**AI),Nums),
        sumlist(Nums,QueryAV),
	debug(nlp,'   queryAV: ~w',[QueryAV]),
	feature_vector(HitLabel,HitAV),
	AVI is HitAV /\ QueryAV,
	% proportion of hit that matches
	Sim is popcount(AVI) / popcount(HitAV),
	Sim > 0,
	vector_attributes(AVI,HitToks).

corpus_search_ic(_QueryTemplate,TermTemplate,Goal,HitLabel-HitToks,Sim,Stemmed) :-
	Goal,
	debug(nlp,'testing ~w [stemmed:~w]',[TermTemplate,Stemmed]),
	setof(T,term_token_stemmed(TermTemplate,T,Stemmed),Ts),
	debug(nlp,'   query toks: ~w',[Ts]),
	setof(Num,T^AI^(member(T,Ts),simmatrix:attribute_ix(T,AI),Num is 2**AI),Nums),
        sumlist(Nums,QueryAV),
	debug(nlp,'   queryAV: ~w',[QueryAV]),
	feature_vector(HitLabel,HitAV),
	%debug(nlp,'     Hit: ~w ~w',[HitLabel, HitAV]),
	%Sim is popcount(HitAV /\ QueryAV) / popcount(HitAV).
        AVI is HitAV /\ QueryAV,
	AVI > 0,
	simmatrix:vector_sumIC(AVI,Sim),
	Sim > 12.5,
	vector_attributes(AVI,HitToks).
%        simmatrix:vector_sumIC(AVI,ICI),
%	ICI > 3,
%        simmatrix:vector_sumIC(HitAV,ICU),
%        Sim is ICI/ICU,
%	Sim > 0.5.


% NEW (slow)

%% label_query_results(+QueryLabel,+Stemmed,?MaxScorePairsDesc:list)
% MaxScorePairsDesc = [Score1-Entity1, ...]
label_query_results(QueryLabel,Stemmed,MaxScorePairsDesc) :-
        % query -> tokens
        term_tokenset_stemmed(QueryLabel,QueryToks,Stemmed),
        
        % find all match entities
        debug(nlp,'query tokens: ~w',[QueryToks]),
        setof(Score-E,MatchLabel^match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score),Pairs),

        % aggregate matches by best score for each entity
        setof(MaxScore-E,aggregate(max(Score),member(Score-E,Pairs),MaxScore),MaxScorePairs),

        % Descending order
        reverse(MaxScorePairs,MaxScorePairsDesc).

label_query_best_result(QueryLabel,Stemmed,Score,E) :-
        label_query_results(QueryLabel,Stemmed,[Score-E|_]).

label_full_parse(QueryLabel,Stemmed,MatchPairs) :-
        % query -> tokens
        term_tokenset_stemmed(QueryLabel,QueryToks,Stemmed),
        debug(nlp,'query tokens: ~w',[QueryToks]),

        toks_parse_recursive(QueryToks,Stemmed,MatchPairs).



toks_parse_recursive(QueryToks,Stemmed,[MaxScore-BestE|MatchPairs]) :-  

        % all matches
        setof(Score-E-MatchToks,MatchLabel^match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score,MatchToks),Matches),
        !,

        % best match
        aggregate(max(Score,E-MatchToks),member(Score-E-MatchToks,Matches),max(MaxScore,BestE-BestMatchToks)),

        % carry on with remaining set
        ord_subtract(QueryToks,BestMatchToks,RemainingToks),

        toks_parse_recursive(RemainingToks,Stemmed,MatchPairs).

toks_parse_recursive(_,_,[]).


match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score) :-
        match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score,_).

match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score,MatchToks) :-
        % search database of entities
        entity_label_token_list_stemmed(E,MatchLabel,MatchToks,Stemmed),
        entity_label_modifier(E,MatchLabel,Mod),
        entity_additional_modifier(E,Mod2),
        
        % find tokens in common - must have at least one
        ord_intersection(QueryToks,MatchToks,IToks),
        IToks\=[],

        % here we now assume the query is a sentence; for now
        % we do not care baout its length
        length(QueryToks,QLen),
        length(MatchToks,MLen),
        length(IToks,ILen),
        Score1 is ILen/MLen, % prop of match

        MaxLen is max(QLen,MLen),
        Score2 is ILen/MaxLen, % prop of query or match
        Score is (Score1 + Score2*Score2) * Mod * Mod2.

entity_label_modifier(E,L,M) :- entity_label_scope(E,L,S),scope_modifier(S,M),!.

scope_modifier(label,1).
scope_modifier(exact,1).
scope_modifier(narrow,0.8).
scope_modifier(_,0.5).

entity_additional_modifier(E,0.5) :- entity_obsolete(E,_),!.
entity_additional_modifier(_,1).
        
entity_label_token_list_stemmed(E,Label,Toks,Stemmed) :-
        setof(Tok,entity_label_token_stemmed(E,Label,Tok,Stemmed),Toks).


%% match_entity_by_label(+QueryLabel,?E,?MatchLabel,+Stemmed,?Score)
match_entity_by_label(QueryLabel,E,MatchLabel,Stemmed,Score) :-
        term_tokenset_stemmed(QueryLabel,QueryToks,Stemmed),
        match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score).
        

% ----------------------------------------
% obol-ish stuff
% ----------------------------------------

%% label_split(T,A,B)
% e.g. label_split('foo bar',foo,bar).
label_split(T,A,B) :-
        atom_concat(A,' ',Ax),
        atom_concat(Ax,B,T).

%% term_split(E,A,B,S1,S2,S3)
% e.g term_split(x:foo_bar,x:foo,x:bar,exact,exact,exact)
term_split(E,A,B,S1,S2,S3) :-
        entity_label_scope(E,EN,S1),
        entity_label_scope(A,AN,S2),
        label_split(EN,AN,BN),
        entity_label_scope(B,BN,S3).

%% term_split_over(+W,?E,?A,?B,?S1,?S2,?S3) is nondet
% true if E = A W B
term_split_over(W,E,A,B,S1,S2,S3) :-
        concat_atom([' ',W,' '],W_padded),
        entity_label_scope(E,EN,S1),
        concat_atom([AN,BN],W_padded,EN),
        entity_label_scope(A,AN,S2),
        entity_label_scope(B,BN,S3).

term_split_over_strict(W,E,A,B) :-
        term_split_over(W,E,A,B,S1,S2,S3),
        is_exact(S1),
        is_exact(S2),
        is_exact(S3).

is_exact(exact).
is_exact(label).

remove_word(N,W,N2) :-
        atom_concat(Toks,' ',N),
        select(W,Toks,Toks2),
        atom_concat(Toks2,' ',N2).

remove_words(N,[],N).
remove_words(N,[W|WL],N2) :-
        (   remove_word(N,W,N3)
        ->  true
        ;   N3=N),
        remove_words(N3,WL,N2).

%% add_synonyms_minus_words(+Words:list)
%
% used to generate a version of an ontology for
% a particular context. For example, bone literature may
% use labels such as 'nasals' rather than nasal bone.
% we can add synonyms to represent the explicit context here.
add_synonyms_minus_words(WL) :-
        findall(s(E,L2,S),
                (   entity_label_scope(E,L,S1),
                    (   S1=label
                    ->  S=exact
                    ;   S=S1),
                    remove_words(L,WL,L2)
                ),Syns),
        forall(member(s(E,L,S),Syns),
               assert(metadata_db:entity_label_scope(E,L,S))).

               

        
        

term_ends_with(E,S,SN,Tail,S1,S2) :-
        atom_concat(' ',Tail,Tail_ws),
        entity_label_scope(E,EN,S1),
        atom_concat(SN,Tail_ws,EN),
        entity_label_scope(S,SN,S2).

%% label_template_match(+T,+Toks:list) is nondet
%% label_template_match(+T,+Toks:list,?Del) is nondet
% true if T can be split into Toks. Toks can be a mix of ground atoms and variables that will be unified with atoms.
% Tok can also be a term A/B/C/... which represents an option
label_template_match(T,Toks) :-
        label_template_match(T,Toks,_).
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % A/B : OR condition
        % ----------
        nonvar(Tok),
        Tok=A/B,
        !,
        (   label_template_match(T,[A|Toks],Del)
        ;   label_template_match(T,[B|Toks],Del)).
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % A+B : join ignoring Del
        % ----------
        nonvar(Tok),
        Tok=A+B, % e.g. X+s
        !,
        sub_atom(T,0,Pos,_,A),
        sub_atom(T,Pos,LenB,NumRest,B),
        (   NumRest > 0
        ->  Pos2 is Pos+LenB,
            sub_atom(T,Pos2,_,0,Rest_1),
            atom_concat(' ',Rest,Rest_1),
            label_template_match(Rest,Toks,Del)
        ;   Toks=[]). % last token
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % optional plurals
        % ----------
        nonvar(Tok),
        Tok=opt_plural(A),
        !,
        (   label_template_match(T,[plural(A)|Toks],Del) % try and match plural first
        ;   label_template_match(T,[A|Toks],Del)).
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % optional tokens
        % ----------
        nonvar(Tok),
        Tok=opt(A),
        !,
        (   label_template_match(T,[A|Toks],Del) % try and match optional token first
        *-> true                                 % greedy
        ;   label_template_match(T,Toks,Del)).
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % plurals
        % ----------
        nonvar(Tok),
        Tok=plural(A),
        !,
        next_tok(T,Next,Del,Rest),
        depluralize(Next,A),
        label_template_match(Rest,Toks,Del).
label_template_match(T,[T],_) :- !.  % MATCH
label_template_match('',[],_) :- !.  % END - MATCHES
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % OPTIMIZATION: Match last token first
        % ----------        
        var(Tok),
        AllToks=[Tok|Toks],
        Toks\=[],
        reverse(AllToks,[LastTok|ToksRev]),
        atom(LastTok),
        reverse(ToksRev,RestToks),
        !,
        % more efficient to match LAST token
        sub_atom(T,Start,_,0,LastTok),
        Pos1 is Start-1,
        Pos1>0,
        sub_atom(T,Pos1,1,_,Del),
        sub_atom(T,0,Pos1,_,Rest),
        label_template_match(Rest,RestToks,Del).
label_template_match(T,[Tok|Toks],Del) :-
        % ----------
        % DEFAULT: match next token and carry on processing
        % ----------        
        !,
        sub_atom(T,0,Pos,_,Tok),
        Pos1 is Pos+1,
        sub_atom(T,Pos,1,_,Del),
        sub_atom(T,Pos1,_,0,Rest),
        label_template_match(Rest,Toks,Del).

next_tok(T,T,_,'').
next_tok(T,Tok,Del,Rest) :-
        sub_atom(T,0,Pos,_,Tok),
        Pos1 is Pos+1,
        sub_atom(T,Pos,1,_,Del),
        sub_atom(T,Pos1,_,0,Rest).


% Example: entity_label_template_match(E,L,[abnormal,X,level/levels])
entity_label_template_match(E,T,Toks) :-
        entity_label_template_match(E,T,Toks,_,_).
entity_label_template_match(E,T,Toks,Del,S) :-
        entity_label_scope(E,T,S),
        label_template_match(T,Toks,Del).

depluralize(X,Y) :- entity_synonym_type(E,X,'PLURAL'),entity_label(E,Y),entity_synonym_scope(E,X,exact),!.
depluralize(X,Y) :- atom_concat(Base,ies,X),atom_concat(Base,y,Y),!.
depluralize(X,Y) :- atom_concat(Base,ae,X),atom_concat(Base,a,Y),!.
depluralize(X,Y) :- atom_concat(Y,s,X).


plural_suffix_atom(s).
plural_suffix_atom(e). % e.g. vertebrae

        


/** <module> 

  ---+ Synopsis


  look for occurrences of class labels in definitions
==
  blip-findall -debug nlp -u metadata_nlp -r cell -goal "simindex_corpus_by_labels(false)" "corpus_search(E,Def,def(E,Def),Hit,Sim,false),class(E,N)" -select "match(E,N,Def,Hit,Sim)"
==

    
==
  blip-findall -debug nlp -u metadata_nlp -r omim -r human_phenotype -r uberon -r pato -r go -goal "simindex_corpus_by_labels(E,Def,def(E,Def),false)" "corpus_search(E,Def,def(E,Def),Hit,Sim,true),class(E,N)" -select "match(E,N,Def,Hit,Sim)"
==


  ==
blip-findall -debug index -index "metadata_db:entity_label_token_list_stemmed(1,0,0,0)" -u metadata_nlp -i generifs_basic.pro  -r cell "gene_rif(_,GID,P,_,Rif),label_query_results(Rif,true,Pairs),member(Sc-X,Pairs),Sc>0.75" -select "m(Rif,Sc,X)" -label
  ==

  ==
blip-findall -debug index -index "metadata_db:entity_label_token_list_stemmed(1,0,0,0)" -u metadata_nlp -i generifs_basic.pro  -r nif_downcase "gene_rif(_,GID,P,_,Rif),label_query_results(Rif,true,Pairs),member(Sc-X,Pairs),Sc>0.75" -select "m(Rif,Sc,X)" -label
  ==

  search for gene/protein labels
  ==
blip-findall -debug index -index "metadata_db:entity_label_token_list_stemmed(1,0,0,0)" -u metadata_nlp -i generifs_basic.pro  -r protein -r gene/10090 "gene_rif('NCBITaxon:10090',GID,P,_,Rif),label_query_results(Rif,true,Pairs),member(Sc-X,Pairs),Sc>0.95" -select "m(Rif,Sc,X)" -label
  ==

  
---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
