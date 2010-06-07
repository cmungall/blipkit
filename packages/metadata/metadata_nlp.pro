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
	   entity_pair_label_match/2,
	   entity_pair_label_match/3,
	   atom_search/5,
	   corpus_search/6
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
	dehyphenate(L,L2), % nd
	setof(Tok,term_token_stemmed(L2,Tok,St),Toks),
	maplist(token_syn_refl,Toks,Toks2), % nondet
	sort(Toks2,Toks3), % need to sort again, synsets may introduce re-ordering
	concat_atom(Toks3,'',A).

:- multifile exclude_entity/1.


dehyphenate(X,X).
dehyphenate(X,Y) :- concat_atom(L,'-',X),L\=[_],concat_atom(L,'',Y).


% TODO: allow custom synsets
synset(['1','1st',first,'I']).
synset(['2','2nd',second,'II']).
synset(['3','3rd',third,'III']).
synset(['4','4th',fourth,'IV']).
synset(['5','5th',fifth,'V']).
synset(['6','6th',sixth,'VI']).
synset(['7','7th',seventh,'VII']).
synset(['8','8th',eight,'VIII']).
synset(['9','9th',ninth,'IX']).
synset(['10','10th',tenth,'X']).
synset(['11','11th',eleventh,'XI']).
synset(['12','12th',twelfth,'XII']).
synset(['13','13th',thirteenth,'XIII']).
synset(['20','20th',twentyth,'XX']).

% eliminate prepositions. assumes we flatten without spaces
synset([of,'']).
synset(['-','']).
synset(['/','']).
synset([':','']).

:- multifile synset_hook/1.
synset(X) :- synset_hook(X).

% consider indexing
token_syn(T,S) :- synset(L),member(T,L),member(S,L),T\=S.
token_syn(T,S) :- relational_adj(T,S,_,_).
token_syn(T,S) :- relational_adj(S,T,_,_).

token_syn_refl(T,S) :- token_syn(T,S).
token_syn_refl(T,T) :- nonvar(T).

corpus_label_token(T) :-
	entity_label_token(_,T).
corpus_label_token_stemmed(T,Stemmed) :-
	entity_label_token_stemmed(_,T,Stemmed).

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

	

entity_pair_label_match(A,B) :-
	entity_pair_label_match(A,B,true).
entity_pair_label_match(A,B,Stemmed) :-
	entity_nlabel_scope_stemmed(A,N,_ScA,Stemmed),
	entity_nlabel_scope_stemmed(B,N,_ScB,Stemmed).


index_labels(Stemmed) :-
	debug(nlp,'indexing [stemmed:~w]',[Stemmed]),
	generate_term_indexes(Label,Token,
			      entity_label_token_stemmed(_,Label,Token,Stemmed)).

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
				  entity_label_token_stemmed(Entity,_,Token,Stemmed))),
	materialize_index(simmatrix:attribute_feature_count(1,1)).

index_corpus_by_labels(Entity,Term,Goal,Stemmed):-
	!,
	debug(nlp,'indexing [stemmed:~w]',[Stemmed]),
	generate_term_indexes(Label,Token,
			      entity_label_token_stemmed(_,Label,Token,Stemmed)),
	debug(nlp,'force cache...',[]),
	findall(F,feature_vector(F,_),_),
	% for frequencies we count by entity, not label
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



/** <module> 

  ---+ Synopsis

  
==
  blip-findall -debug nlp -u metadata_nlp -r cell -goal "index_corpus_by_labels(false)" "corpus_search(E,Def,def(E,Def),Hit,Sim,false),class(E,N)" -select "match(E,N,Def,Hit,Sim)"
==

    
==
  blip-findall -debug nlp -u metadata_nlp -r omim -r human_phenotype -r uberon -r pato -r go -goal "index_corpus_by_labels(E,Def,def(E,Def),false)" "corpus_search(E,Def,def(E,Def),Hit,Sim,true),class(E,N)" -select "match(E,N,Def,Hit,Sim)"
==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
