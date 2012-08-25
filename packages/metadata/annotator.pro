:- module(annotator,
          [
           test_annotator/1,
           initialize_annotator/0,
           sentence_annotate/2,
           annotate_file/1,
           annotate_file2/1
           ]).

:- use_module(metadata_db).
:- use_module(metadata_nlp).
:- use_module(bio(simmatrix)).
:- use_module(bio(index_util)).
:- use_module(bio(ontol_db)). % consider moving the one dependency on this
:- use_module(library(porter_stem)).
:- use_module(bio(av_db)).  % obol dependency - consider moving this

%entity_label_attr(E,N,T) :-
%        entity_label_or_synonym(E,N,S),
%        entity_label_token_stemmed(E,N,T,true).

initialize_annotator :-
        generate_term_indexes(Label,Token,metadata_nlp:entity_label_token_stemmed(_,Label,Token,true)),
        debug(annotator,'initialized annotator',[]).

%% sentence_wbv(+Sentence:atom,?TokenBitVectorV:integer)
%
% given an atom, tokenize it and translate to bitvector.
% TODO - elim dets, preps; replace reladjs
sentence_tbv(Sentence,V,ToksOrdered) :-
	findall(Tok,term_token_stemmed(Sentence,Tok,true),ToksOrdered),
        sort(ToksOrdered,Toks),
	debug(annotator,'   query toks: ~w',[Toks]),
	setof(Num,Tok^AI^(member(Tok,Toks),simmatrix:attribute_ix(Tok,AI),Num is 2**AI),Nums),
        sumlist(Nums,V).

%% sentence_annotate(+Sentence:atom,?Ann:term)
%
% given a setence atom, annotate it
%
% Ann = m(IDs,Label,Word)
sentence_annotate(Sentence,Ann2) :-
        sentence_tbv(Sentence,QV,SToks),
	debug(annotator,'   building minspanset for: ~w',[Sentence]),
        nb_setval(minspanset,[]),
        % note: become much slower?
        forall(feature_vector(E,HV),
               expand_minspanset(QV,E,HV)),
        nb_getval(minspanset,MinSpanSet),
	debug(annotator,'   built minspanset: ~w',[MinSpanSet]),
        findall(Hit,member(Hit-_,MinSpanSet),Hits),
        sentence_align_hits(SToks,Hits,Ann),
        add_ids(Ann,Ann2).
        %minspanset_stoks_anns(MinSpanSet,SToks,Ann).

% given a list of matches [m(_,_),...], add ids
% to yield a list [m(IDs,_,_),....]
add_ids([],[]).
add_ids([H|T],[H2|T2]) :-
        add_id(H,H2),
        add_ids(T,T2).
add_id(m(N,W),m(EL,N,W)) :-
        !,
        findall(E,entity_label_or_synonym(E,N),EL).
add_id(X,X).

minspanset_stoks_anns([],_,[]) :- !.
minspanset_stoks_anns([H-_|MSL],SentenceToks,[Score-Ann|Anns]) :-
        feature_attributeset(H,HToks),
        label_to_ann(H,Ann),
        score_hit(SentenceToks,HToks,Score),
        minspanset_stoks_anns(MSL,SentenceToks,Anns),
        !.
minspanset_stoks_anns(MSL,SentenceToks,Anns) :-
        throw(minspanset_stoks_anns(MSL,SentenceToks,Anns)).

score_hit(SentenceToks,HToks,Score) :-
        toks_chains(SentenceToks,HToks,[],Chains),
        maplist(length,Chains,ChainLengths),
        findall(S,(member(Len,ChainLengths),S is (2**Len)-1),SL),
        sumlist(SL,Score),
        !.
score_hit(SentenceToks,HToks,Score) :-
        throw(score_hit(SentenceToks,HToks,Score)).
        
toks_chains([],[],InL,[InL]) :- !.
toks_chains([Tok|SToks],HToks,InL,S) :-
        select(Tok,HToks,HToks2),
        !,
        toks_chains(SToks,HToks2,[Tok|InL],S).
toks_chains([_|SToks],HToks,InL,[InL|S]) :-
        !,
        toks_chains(SToks,HToks,[],S).

% todo - exact 1st
label_to_ann(N,ann(E,N,S)) :-
        entity_label_scope(E,N,S),
        !.

%% sentence_align_hits(+SToks:list,+Hits:list,?Path:list) is semidet.
%
% given a list of tokens, and a list of matching tokens, find path through setence tokens.
% note that Hits is a list of matching labels
sentence_align_hits(SToks,Hits,CombinedPath) :-
        debug(annotator,'SToks:~w Hits:~w',[SToks,Hits]),
        % align each hit separately
        % TODO - use DP here?
        findall(Path,
                (   member(Hit,Hits),
                    debug(annotator,'ALIGNING:~w',[Hit]),
                    sentence_align_hit(u,SToks,Hit,Path),
                    debug(annotator,'    Path:~w',[Path])
                    ),
                Paths),
        debug(annotator,'Pre-zipped Paths:~w',[Paths]),
        zip_hit_paths(Paths,CombinedPath).

zip_hit_paths(Paths,[State|CombinedPath]) :-
        findall(H,member([H|_],Paths),Hs),
        Hs\=[],
        !,
        findall(T,member([_|T],Paths),Paths2),
        combine_states(Hs,State),
        debug(annotator,'     Combined:~w ==> ~w',[Hs,State]),
        zip_hit_paths(Paths2,CombinedPath).
zip_hit_paths(_,[]).
        
% if all parallel states are identical, use a singleton.
% ignore bare token states if there are match states.
combine_states(L,S) :- setof(S,member(S,L),[S]),!.
combine_states(L,S) :- setof(S,(member(S,L),\+atom(S)),[S]),!.
combine_states(L,Ss) :- setof(S,(member(S,L),\+atom(S)),Ss).


        
%% sentence_align_hit(+State,+SToks:list,+Hit,?Path:list) is semidet.
%
% Path = [S1, S2, ...], one state for each sentence token
% Sn = m(InPhrase,Token) | gap(InPhrase,Token) | Token
%
% 
%
% potentially expensive - using backtracking rather than DP.

%sentence_align_hit(S,[STok|SToks],_,_) :-
%        debug(annotator,'  IN:~w NEXT:~w',[S,STok]),
%        fail.

% state: start/unmatched
% transition: to end
% note: if S=m, then it should first switch to u after matching last token
sentence_align_hit(u,[],_,[]) :- !.

% state: start/unmatched
% transition: to match
sentence_align_hit(u,[STok|SToks],Hit,[m(Hit,STok)|Path]) :-
        feature_attributeset(Hit,HToks),
        select(STok,HToks,HToks2),
        sentence_align_hit(m(Hit,HToks2),SToks,Hit,Path),
        !.

% state: start/unmatched
% no transition
sentence_align_hit(u,[STok|SToks],Hit,[STok|Path]) :-
        !,
        sentence_align_hit(u,SToks,Hit,Path).

% state: match
% transition: to unmatched (success)
% [do not advance]
sentence_align_hit(m(Hit,[]),SToks,Hit,Path) :-
        !,
        sentence_align_hit(u,SToks,Hit,Path).

% state: match
% transition: no transition (next token matches)
sentence_align_hit(m(Hit,HToks),[STok|SToks],Hit,[m(Hit,STok)|Path]) :-
        select(STok,HToks,HToks2),
        !,
        sentence_align_hit(m(Hit,HToks2),SToks,Hit,Path).

% state: match
% transition: to gap (token did not match)
% [do not advance]
sentence_align_hit(m(Hit,HToks),SToks,Hit,Path) :-
        !,
        sentence_align_hit(gap(Hit,HToks),SToks,Hit,Path).

% state: gap
% transition: to match
sentence_align_hit(gap(Hit,HToks),[STok|SToks],Hit,[m(Hit,STok)|Path]) :-
        select(STok,HToks,HToks2),
        !,
        sentence_align_hit(m(Hit,HToks2),SToks,Hit,Path).

% state: gap
% transition: to gap (no match)
sentence_align_hit(gap(Hit,HToks),[STok|SToks],Hit,[gap(Hit,STok)|Path]) :-
        !,
        sentence_align_hit(gap(Hit,HToks),SToks,Hit,Path).

%% expand_minspanset(+QV:integer, +Entity:atom, +HV:integer) is det
%
% side effect: modifies minspanset nb variable incremenrally
%
% QV: query (sentence) token bitvector
% HV: hit (candidate match, e.g. term) token bitvector
%
% first determine overlap between query and candidate match;
% currently all tokens in candidate must be subsumed by query.
%
% then add the hit to the minspanset, provided it is not
% redundant with existing hits; if existing hits become redundant with
% it, they are removed
expand_minspanset(QV,E,HV) :-
        IV is HV /\ QV,
        %debug(annotator_detail,'   ~w : ~w^~w = ~w',[E,QV,HV,IV]),
        debug(annotator_detail,'   IV = ~w',[IV]),
        %(   E=deep_to->trace;true),
        expand_minspanset(IV,QV,E,HV).

expand_minspanset(0,_,_,_) :- !. % no overlap, no effect on minspanset
expand_minspanset(HV,_QV,E,HV) :- % subsumed (intersection equals hit)
        debug(annotator,'~w VECTOR: ~w',[E,HV]),
        % IV=HV
        nb_getval(minspanset,SpanL),
        % fails if no change
        replace_minspanset(SpanL,E,HV,NewSpanL),
        sort([E-HV|NewSpanL],NewSpanSet),
        nb_setval(minspanset,NewSpanSet),
        !.
expand_minspanset(_IV,_QV,_E,_HV) :- !.

% semidet
% fails if minspanset remains unchanged
replace_minspanset([],_,_,[]).
replace_minspanset([X-XV|InSpanL],E,EV,OutSpanL) :-
        UV is XV \/ EV,
        debug(annotator,'  rmms ~w :: ~w /\ ~w = ~w',[E,XV,EV,UV]),
        (   UV=XV                        % candidate adds nothing new, subsumed or equal to X
        ->  !,
            fail
        ;   UV=EV                        % candidate subsumes current
        ->  OutSpanL=RestL               % remove
        ;   OutSpanL=[X-XV|RestL]), % keep
        replace_minspanset(InSpanL,E,EV,RestL).

% ----------------------------------------
% TEST
% ----------------------------------------
test_annotator(Ann) :-
        ensure_loaded(bio(io)),
        load_bioresource(caro),
        initialize_annotator,
        sentence_annotate('serially hermaphroditic foo, organism of blah atypical epithelium',Ann).

% T cell recognition of the OspA epitope is important in the induction of autoimmunity in treatment-resistant Lyme arthritis

% ----------------------------------------
% I/O
% ----------------------------------------

% Example:
%  obol -u annotator -r go -r cell -goal "annotate_file('foo.txt')"
annotate_file(File) :-
        initialize_annotator,
        open(File,read,IO,[]),
        repeat,
        (   at_end_of_stream(IO)
        ->  !
        ;   
            read_line_to_codes(IO,Codes),
            atom_codes(Line,Codes),
            atomic_list_concat(Sentences,'.',Line),
            member(S,Sentences),
            sentence_annotate(S,Ann),
            format('~q - ~q.~n',[S,Ann]),
            fail
        ).

annotate_file2(File) :-
        initialize_annotator,
        open(File,read,IO,[]),
        repeat,
        (   at_end_of_stream(IO)
        ->  !
        ;   
            read_line_to_codes(IO,Codes),
            atom_codes(Line,Codes),
            atomic_list_concat([ID,Term],'\t',Line),
            sentence_annotate(Term,Ann),
            format('~q.~n',[p(ID,Term,Ann)]),
            fail
        ).


       
