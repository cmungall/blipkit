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
	   token_syn/2,
	   corpus_label_token/1,
	   corpus_label_token_frequency/2,
	   index_labels/1,
	   index_labeled_entities/1,
	   index_corpus_by_labels/1,
	   index_corpus_by_labels/4,
	   entity_pair_nlp_subset_of/3,
	   entity_pair_nlp_subset_of_cross_idspace/3,
	   entity_pair_nlp_match/7,
           index_entity_pair_label_match/0,
	   entity_pair_label_match/2,
	   entity_pair_label_match/3,
	   atom_search/5,
	   corpus_search/6,
           entity_label_token_list_stemmed/4,
           label_query_results/3,

           term_split/6,
           term_split_over/7,
           term_split_over_strict/4,
           term_ends_with/6
	   ]).

:- use_module(metadata_db).
:- use_module(bio(simmatrix)).
:- use_module(bio(index_util)).
:- use_module(library(porter_stem)).
:- use_module(bio(av_db)).  % obol dependency - consider moving this

term_token(A,T) :-
	tokenize_atom(A,TL),
	member(T,TL).

term_token_stemmed(A,T) :-
	term_token_stemmed(A,T,true).

term_token_stemmed(A,T,true) :-
        % acronym
        \+ ((sub_atom(A,_,1,_,C),
             C@>='a',
             C@=<'z')),
        !,
        term_token_stemmed(A,T,false).
term_token_stemmed(A,T,true) :-
        % not acronym
	term_token(A,T1),
	porter_stem(T1,T).
term_token_stemmed(A,T,false) :-
	term_token(A,T).

term_nth_token(A,N,T) :-
	tokenize_atom(A,TL),
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
%  * uses synset/1
%  * canonical token ordering
%  * removes all whitespaces
entity_nlabel_scope_stemmed(E,A,Scope,St) :-
	entity_label_scope(E,L,Scope),
	\+ exclude_entity(E),
	debug(nlp,'calculating nlabel ~w "~w" (~w)',[E,L,Scope]),
        term_nlabel_stemmed(L,A,St).

% HOOK
:- multifile exclude_entity/1.

%% term_nlabel_stemmed(+Term:atom,?NLabel:atom,+Stemmed:bool) 
%
% translates a term such as 'foo bar two hippocampi' to a label like
% '2barfoohippocamp'
% this is used for exact matching, but is not useful for finding labels
% nested inside larger labels or blocks of text
term_nlabel_stemmed(Term,NLabel,St) :-
        term_tokenset_stemmed(Term,Toks,St),
	concat_atom(Toks,'',NLabel).

%% term_tokenset_stemmed(+Term:atom,?Toks:list,+Stemmed:boolean)
% 
% translates a term such as 'foo bar two hippocampi' to a set such as
% [2,bar,foo,hippocamp]
term_tokenset_stemmed(Term,Toks,St) :-
	dehyphenate(Term,TermNoHyphen), % nd
        setof(Tok,term_token_stemmed(TermNoHyphen,Tok,St),Toks_1),
	maplist(token_syn_refl,Toks_1,Toks_2), % normalize using synsets - nondet (MAKE DET?)
	sort(Toks_2,Toks).  % need to sort again, synsets may introduce re-ordering

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
token_syn(T,S) :- relational_adj(T,S,_,_),!.
token_syn(T,S) :- relational_adj(S,T,_,_),!.
token_syn(T,S) :- synset([S|L]),member(T,L).

% reflexive
token_syn_refl(T,S) :- token_syn(T,S),!.
token_syn_refl(T,T) :- nonvar(T).

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

	
index_entity_pair_label_match :-
        materialize_index(entity_nlabel_scope_stemmed(1,1,-,-)).

entity_pair_label_match(A,B) :-
	entity_pair_label_match(A,B,true).
entity_pair_label_match(A,B,Stemmed) :-
	entity_nlabel_scope_stemmed(A,N,_ScA,Stemmed),
	entity_nlabel_scope_stemmed(B,N,_ScB,Stemmed),
        A\=B.


index_labels(Stemmed) :-
	debug(nlp,'indexing [stemmed:~w]',[Stemmed]),
	generate_term_indexes(Label,Token,
			      metadata_nlp:entity_label_token_stemmed(_,Label,Token,Stemmed)).

% DON'T USE THIS!! only use if each entity has a single label
index_labeled_entities(Stemmed) :-
	debug(nlp,'indexing entities [stemmed:~w]',[Stemmed]),
	generate_term_indexes(E,Token,
			      entity_label_token_stemmed(E,_,Token,Stemmed)).


% index is per-label rather than per-entity
index_corpus_by_labels(Stemmed):-
	!,
	index_labels(Stemmed),
	debug(nlp,'force cache...',[]),
	findall(F,feature_vector(F,_),_),
	% for frequencies we count by entity, not label
	debug(nlp,'calculating freqs [stemmed:~w]',[Stemmed]),
	retractall(simmatrix:template(_,_,_)),
        assert(simmatrix:template(Entity,Token,
				  metadata_nlp:entity_label_token_stemmed(Entity,_,Token,Stemmed))),
	materialize_index(simmatrix:attribute_feature_count(1,1)).

%% index_corpus_by_labels(Entity,Term,Goal,Stemmed)
%
% e.g. index_corpus_by_labels(Gene,Rif,gene_rif(_,Gene,_,_,Rif),true)
%
% main index is constructed from ontology
index_corpus_by_labels(Entity,Term,Goal,Stemmed):-
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
label_query_results(QueryLabel,Stemmed,MaxScorePairsDesc) :-
        % query -> tokens
        term_tokenset_stemmed(QueryLabel,QueryToks,Stemmed),
        
        % find all match entities
        setof(Score-E,MatchLabel^match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score),Pairs),

        % aggregate matches by best score for each entity
        setof(MaxScore-E,aggregate(max(Score),member(Score-E,Pairs),MaxScore),MaxScorePairs),

        % Descending order
        reverse(MaxScorePairs,MaxScorePairsDesc).

match_entity_by_toks(QueryToks,E,MatchLabel,Stemmed,Score) :-
        % search database of entities
        entity_label_token_list_stemmed(E,MatchLabel,MatchToks,Stemmed),

        % find tokens in common - must have at least one
        ord_intersection(QueryToks,MatchToks,IToks),
        IToks\=[],

        % here we now assume the query is a sentence; for now
        % we do not care baout its length
        length(MatchToks,MLen),
        length(IToks,ILen),
        Score is ILen/MLen.
        
        /*
        length(QueryToks,QLen),
        length(MatchToks,MLen),
        length(IToks,ILen),
        MLen is max(QLen,MLen),
        Score is ILen/MLen.
        */
        
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

term_split_over(E,A,B,W,S1,S2,S3) :-
        concat_atom([' ',W,' '],W_padded),
        entity_label_scope(E,EN,S1),
        concat_atom([AN,BN],W_padded,EN),
        entity_label_scope(A,AN,S2),
        entity_label_scope(B,BN,S3).

term_split_over_strict(E,A,B,W) :-
        term_split_over(E,A,B,W,S1,S2,S3),
        is_exact(S1),
        is_exact(S2),
        is_exact(S3).

is_exact(exact).
is_exact(label).


term_ends_with(E,S,SN,Tail,S1,S2) :-
        atom_concat(' ',Tail,Tail_ws),
        entity_label_scope(E,EN,S1),
        atom_concat(SN,Tail_ws,EN),
        entity_label_scope(S,SN,S2).
        


/** <module> 

  ---+ Synopsis


  look for occurrences of class labels in definitions
==
  blip-findall -debug nlp -u metadata_nlp -r cell -goal "index_corpus_by_labels(false)" "corpus_search(E,Def,def(E,Def),Hit,Sim,false),class(E,N)" -select "match(E,N,Def,Hit,Sim)"
==

    
==
  blip-findall -debug nlp -u metadata_nlp -r omim -r human_phenotype -r uberon -r pato -r go -goal "index_corpus_by_labels(E,Def,def(E,Def),false)" "corpus_search(E,Def,def(E,Def),Hit,Sim,true),class(E,N)" -select "match(E,N,Def,Hit,Sim)"
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
