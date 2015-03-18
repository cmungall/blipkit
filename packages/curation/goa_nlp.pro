****
replaced by classifier.pro in project directory.

: fail.

:- module(goa_nlp,
          [
           gopmid/1,
           annotate_gopmid/3,
           gopmid_token/2,
           gopmid_info/3,

           xp_predict/3,
           xp_predict/4,
           xp_predict_best/3,
           xp_predict_best/4,
           xp_predict_uniq/3,
           xp_predict_testprecision/4,
           all_predictor_testprecision/5,
           all_predictor_testrecall/5,
           all_predictor_pr/6,

           xp_suggest/5,
           
           recap_gopmid/6
           ]).


:- use_module(bio(remote_annotator)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(tabling)).


/*

  gaf2pro.sh gene_association.mgi.gz  > mgi.pro
  
*/


gopmid(PMID) :-
        call_unique(gopmid_1(PMID)).
gopmid_1(PMID) :-
        assoc(_G,_GN,_T,Ref,_Ev),
        assocref_pmid(Ref,PMID).

assocref_pmid(Ref,PMID) :-
        atom_concat('PMID:',PMID,Ref).

/*
  Recommended:

  
*/

annotate_gopmid(PMID,Toks,Text) :-
        gopmid(PMID),
        debug(nlp,'pmid=~w',[PMID]),
        annotate_pmid(PMID,Toks,Text,[]).

gopmid_token(PMID,Tok) :-
        annotate_gopmid(PMID,Toks,_),
        member(Tok,Toks).

gopmid_info(PMID,token,Tok) :-
        gopmid_token(PMID,Tok).
gopmid_info(PMID,text,Text) :-
        annotate_gopmid(PMID,_,Text).



% never make the same service call twice
:- persistent_table_pred(remote_annotator:annotate_pmid/4, 'annotate_cache.pro').
:- table_pred(ontol_db:subclassT/2).
:- table_pred(ontol_db:bf_parentRT/2).

% a term is in the blacklist if it is excluded from testing
blacklist(Term) :- hard_exclude(Term).
blacklist(Term) :- class(Term,'protein binding'),!.
blacklist(Term) :- genus(Term,'GO:0005575'),class_cdef(Term,cdef('GO:0005575',[part_of=_])).  % never predict 'X part'
blacklist(Term) :- class(Term,cell),!.  
blacklist(Term) :- class(Term,'cell part'),!.

% any entity match to these terms is likely to be dubious or useless
hard_exclude(Term) :- entity_obsolete(Term,_),!.
hard_exclude(Term) :- class(Term,'Be cell'),!.
hard_exclude(Term) :- class(Term,molecule),!.

/*

  genus mapping:

  sometimes genus labels are interchangeable. Use context. E.g. "cilium formation" means "cilium assembly", "formation" is syn for biosynthesis, but biosynthesis isn't applied to CCs

  "formation" (biosynthetic process) -> assembly. E.g. http://www.ncbi.nlm.nih.gov/pubmed/17185389

  Also, use match to RO 'regulates' as proxy for regulation
  
  */

% map to a more generic form that is more likely to be found
genus_variant(X,X).
genus_variant('GO:0048856','GO:0032502'). % anatomical structure development -> developmental process (development)
genus_variant('GO:0022607', 'GO:0009058'). % cellular component assembly TO biosynthetic process (formation)
genus_variant('GO:0050789', 'GO:0065007'). % RoBP -> BR (regulation)
genus_variant('GO:0050789', 'RO:0002211'). % RoBP -> rel
genus_variant('GO:0048519', 'RO:0002212'). % -RoBP -> rel
genus_variant('GO:0048518', 'RO:0002213'). % +RoBP -> rel


class_helts(C,[GAs|DAsL]) :-
        genus(C,G),
        solutions(GA,subclassRT(G,GA),GAs),
        solutions(DAs,
                  (   (   differentia(C,D),
                          solutions(DA,part_ofRT(D,DA),DAs))),
                  DAsL).

:- table_pred(class_helts/2).

part_ofRT(X,Y) :- subclassRT(X,Y).
part_ofRT(X,Y) :- part_of1(X,Z),part_ofRT(Z,Y).
part_of1(X,Y) :-
        subclassRT(X,A),
        parent(A,part_of,B),
        subclassRT(B,Y).

:- table_pred(part_of/2).

        
        

        

predict(basic,PMID,Term,Score) :-        
        basic_predict(PMID,Term,Score,_).
predict(xp,PMID,Term,Score) :-        
        xp_predict(PMID,Term,Score,_).

basic_predict(PMID,Term,mtch,Toks) :-
        annotate_gopmid(PMID,Toks,_Text),
        member(Tok,Toks),
        token_term(Tok,Term),
        id_idspace(Term,'GO'),
        \+ blacklist(Term).
        

%% xp_predict(?PMID,?Term,?Score)
%% xp_predict(?PMID,?Term,?Score,?Toks)
%
% given a PMID (if unspecified will unify with available PMID from annotations),
% unify Term with a predicted annotation
xp_predict(PMID,Term,Score) :-
        xp_predict(PMID,Term,Score,_).
xp_predict(PMID,Term,Score,Toks) :-
      annotate_gopmid(PMID,Toks,_Text),
      member(BaseToken,Toks),
      token_term(BaseToken,BaseTerm),
      subclassRT(BaseTerm,Genus),
      
      % seed from genus
      call_unique(genus(Term,Genus)),
      \+ blacklist(Term),
      
      % do test on arbitrary diff to avoid time-wasting
      \+ \+ ((
              differentium(Term,_,XDiffTerm),
              member(token(XDiffTerm,_,_,_,_),Toks))),

      % ensure full match
      class_cdef(Term,cdef(Genus,Diffs)),
      findall(Y,member(_=Y,Diffs),DiffTerms),
      toklist_score(Toks,[BaseTerm|DiffTerms],Score).



xp_predict_best(PMID,Term,Score) :-
        xp_predict_best(PMID,Term,Score,_).
xp_predict_best(PMID,Term,Score,Toks) :-
        gopmid(PMID),
        annotate_gopmid(PMID,Toks,_), % dumb way to get tokens
        
        % note: can be hugely expensive is Term is unbound and there are some Terms with multiple Scores
        setof(m(PMID,Term,Score1),
              xp_predict(PMID,Term,Score1),
              Ms),
        aggregate(min(Score1),member(m(PMID,Term,Score1),Ms),Score).



xp_predict_uniq(PMID,Term,Score) :-
        xp_predict(PMID,Term,Score,Toks),
        \+ member(token(Term,_,_,_,_),Toks).

%% PRECISION

xp_predict_testprecision(PMID,Term,Score,TestResult) :-
        xp_predict_best(PMID,Term,Score,Toks),
        \+ member(token(Term,_,_,_,_),Toks),
        test_candidate(Term,PMID,TestResult).

all_predictor_testprecision(Method,PMID,Term,Score,TestResult) :-
        gopmid(PMID),
        % we wrap the method in order to solve all
        % cases for a PMID together
        all_predictor_testprecision_1(Method,PMID,Term,Score,TestResult).

all_predictor_testprecision_1(xp,PMID,Term,Score,TestResult) :-
        xp_predict_testprecision(PMID,Term,Score,TestResult).
all_predictor_testprecision_1(direct,PMID,Term,Score,TestResult) :-
        basic_predict(PMID,Term,Score,_),
        test_candidate(Term,PMID,TestResult).

%% RECALL

all_predictor_testrecall(Method,PMID,Term,TestResult,BestPrediction) :-
        gopmid(PMID),
        assocref_pmid(Ref,PMID),
        assoc(_,_,Term,Ref,_),
        \+ blacklist(Term),
        prediction_method(Method),
        solutions(prediction(Method,PT,Score),predict(Method,PMID,PT,Score),Preds),
        best_prediction(Preds,Term,TestResult,BestPrediction).

prediction_method(xp).
prediction_method(basic).


%% best_prediction(+Preds:list, +CuratedTerm, ?SuccessCode, ?MatchingPrediction) is det
best_prediction(Preds,Term,perfect_match,Pred) :-
        member(Pred,Preds),
        Pred=prediction(_,Term,_),
        !.
best_prediction(Preds,Term,more_specific,Pred) :-
        member(Pred,Preds),
        Pred=prediction(_,PT,_),
        bf_parentRT(PT,Term),
        !.
best_prediction(Preds,Term,more_general,Pred) :-
        member(Pred,Preds),
        Pred=prediction(_,PT,_),
        bf_parentRT(Term,PT),
        !.
best_prediction(_,_Term,no_match,no_prediction) :- !.

        

test_candidate(Term,PMID,TestResult) :-
        assocref_pmid(Ref,PMID),
        solutions(CurTerm,assoc(_G,_GN,CurTerm,Ref,_Ev),CurTerms),
        test_term_call(Term,CurTerms,TestResult),
        !.


test_term_call(Term,CurTerms,perfect_match(Term)) :-
        member(Term,CurTerms),
        !.
test_term_call(Term,CurTerms,more_specific(Term,CurTerm)) :-
        member(CurTerm,CurTerms),
        bf_parentRT(Term,CurTerm),
        !.
test_term_call(Term,CurTerms,more_general(Term,CurTerm)) :-
        member(CurTerm,CurTerms),
        bf_parentRT(CurTerm,Term),
        !.
test_term_call(Term,_,no_match_found(Term)).


all_predictor_pr(PR,Method,PMID,Term,TestResult,BestPrediction) :-
        gopmid(PMID),
        (   PR=precision,
            all_predictor_testprecision(Method,PMID,Term,TestResult,BestPrediction)
        ;   PR=recall,
            all_predictor_testrecall(Method,PMID,Term,TestResult,BestPrediction)).

%% SUGGESTER

xp_suggest(PMID,Genus,Rel,DTerm,Score) :-
      annotate_gopmid(PMID,Toks,_Text),
      %debug(suggest,'pmid=~w',[PMID]),
      member(BaseToken,Toks),
      token_term(BaseToken,BaseTerm),
      \+ blacklist(BaseTerm),
      genus_variant(Genus, BaseTerm),
      id_idspace(Genus,'GO'),

      member(DToken,Toks),
      token_term(DToken,DTerm),
      \+ blacklist(DTerm),

      sort_tokens([BaseToken,DToken],Pair),
      debug(suggest,'pair=~w',[Pair]),
      toklist_padlength(Pair,Score),
      Score > -1,
      Score < 9,

      similar_to(Genus,DTerm,Rel),

      \+ xp_exists(Genus,DTerm).

xp_exists(G,D) :-
        genus_variant(GX,G),
        genus(T,GX),
        differentium(T,_,D).

similar_to(G,D,R) :-
        subclassRT(G,GX),
        genus(Z,GX),
        differentium(Z,R,Y),
        in_path(D,Y),
        !.


in_path(X,X) :- !.
in_path(X,Y) :-
        isapo(X,Y),
        !.
in_path(X,Y) :-
        isapo(Y,X),
        !.

isapo(X,Y) :- subclassRT(X,Y),!.
isapo(X,Y) :- subclassRT(X,Z),parent(Z,part_of,Z2),subclassRT(Z2,Y),!.

:- table_pred(isapo/2).
        


%% OLD:        

/*

  blip-findall -r go   -u goa_nlp -i mgi.pro recap_gopmid/6 -no_pred -label > z
    
  */

% recapi
recap_gopmid(PMID,G,GN,T,PMID,Results) :-
        annotate_gopmid(PMID,Toks,_Text),
        assocref_pmid(Ref,PMID),
        assoc(G,GN,T,Ref,_Ev),
        %GN='Wnt5a',
        %GN='B4galnt1',
        debug(foo,'In: ~w',[T]),
        eval_ann(Toks,T,Results),
        debug(foo,'Out: ~w = ~w',[T,Results]),
        %trace.
        true.

eval_ann(Toks,Term,term_match(Term)) :-
        member(token(Term,_,_,_,_),Toks),
        !.
eval_ann(Toks,Term,ldef_match(Genus,Ys,Score)) :-
        class_cdef(Term,cdef(Genus,Diffs)),
        findall(Y,member(_=Y,Diffs),Ys),
        genus_variant(Genus,GenusV),  % relax genus
        Elts=[GenusV|Ys],
        toklist_score(Toks,Elts,Score),
        !.
eval_ann(Toks,Term,differentia_match(Elts,Score)) :-
        class_cdef(Term,cdef(_Genus,Diffs)),
        findall(Y,member(_=Y,Diffs),Elts),
        toklist_score(Toks,Elts,Score),
        !.
eval_ann(_,Term,no_match(Term)).

toklist_score(Toks,Terms,Score) :-
        % first check all match before scoring:
        % TODO - use closure
        forall(member(Term,Terms),
               member(token(Term,_,_,_,_),Toks)),

        sort_tokens(Toks,Toks2),
        
        % TODO: there may be >1 score; this takes first alignment
        toklist_alignall(Toks2,Terms,MatchToks),
        toklist_padlength(MatchToks,Score).

        

% base case
toklist_alignall(_,[],[]).

% opt
toklist_alignall(Toks,[Term],[Tok]) :-
        member(Tok,Toks),
        % TODO - use closure
        token_term(Tok,Term).
% next token matches
toklist_alignall([Tok|Toks],Terms,[Tok|MatchToks]) :-
        Terms=[_,_|_], % at least two
        % TODO - use closure
        token_term(Tok,Term),
        select(Term,Terms,TermsTail), % TODO - use graph
        toklist_alignall(Toks,TermsTail,MatchToks).
% ignore next token
toklist_alignall([_|Toks],Terms,MatchToks) :-
        Terms=[_,_|_], % at least two
        %token_term(Tok,Term),
        %\+ member(Term,Terms),
        % lookahead
        %member(token(Term,_,_,_,_),Toks),
        toklist_alignall(Toks,Terms,MatchToks).

        
               

toklist_padlength([_],0).
toklist_padlength([Tok1,Tok2 | Toks],TotalLen) :-
        token_end(Tok1,E),
        token_start(Tok2,S),
        Len is S-E,
        toklist_padlength([Tok2 | Toks],TailLen),
        TotalLen is Len + TailLen.

        
%sort_tokens(Toks,SortedToks) :-
%        predsort(tokens_delta,Toks,SortedToks).
sort_tokens(Toks,SortedToks) :-
        map_list_to_pairs(token_start,Toks,Pairs),
        keysort(Pairs,SortedPairs),
        pairs_values(SortedPairs,SortedToks).

token_start(token(_ID,_Cats,_M,Start,_End),Start).
token_end(token(_ID,_Cats,_M,_Start,End),End).
token_term(token(ID,_Cats,_M,_Start,_End),ID).

tokens_delta(Delta,T1,T2) :-
        token_start(T1,S1),
        token_start(T2,S2),
        compare(Delta,S1,S2).






/*

  TODO: use version of GO with additional syns

  e.g. remove "activity"

  $ grep 10751225 zzz
10751225        MGI:MGI:1336884 Slc22a18        GO:0005887 ! integral component of plasma membrane      10751225        no_match(GO:0005887)
10751225        MGI:MGI:1336884 Slc22a18        GO:0006820 ! anion transport    10751225        ldef_match(GO:0006810,[CHEBI:22563],1606)
10751225        MGI:MGI:1336884 Slc22a18        GO:0008514 ! organic anion transmembrane transporter activity   10751225        differentia_match([CHEBI:25696],0)
10751225        MGI:MGI:1336884 Slc22a18        GO:0015711 ! organic anion transport    10751225        ldef_match(GO:0006810,[CHEBI:25696],1401)
10751225        MGI:MGI:1336884 Slc22a18        GO:0034220 ! ion transmembrane transport        10751225        no_match(GO:0034220)
10751225        MGI:MGI:892001  Slc22a6 GO:0006820 ! anion transport    10751225        ldef_match(GO:0006810,[CHEBI:22563],1606)
10751225        MGI:MGI:892001  Slc22a6 GO:0008514 ! organic anion transmembrane transporter activity   10751225        differentia_match([CHEBI:25696],0)
10751225        MGI:MGI:892001  Slc22a6 GO:0015711 ! organic anion transport    10751225        ldef_match(GO:0006810,[CHEBI:25696],1401)
10751225        MGI:MGI:1859559 Slc22a7 GO:0005887 ! integral component of plasma membrane      10751225        no_match(GO:0005887)
10751225        MGI:MGI:1859559 Slc22a7 GO:0008514 ! organic anion transmembrane transporter activity   10751225        differentia_match([CHEBI:25696],0)
10751225        MGI:MGI:1859559 Slc22a7 GO:0015711 ! organic anion transport    10751225        ldef_match(GO:0006810,[CHEBI:25696],1401)


todo: recursively unfold
10073953        MGI:MGI:88049   Apoa1   GO:0030300 ! regulation of intestinal cholesterol absorption    10073953        no_match(GO:0030300)


  also:

  proteasomal (protein catabolic process)


  what if no precomposed term. E.g.
  21178006        MGI:MGI:98923   Vav1    GO:0072593 ! reactive oxygen species metabolic process  21178006        differentia_match([CHEBI:26523],0)
  text says "formation", but ROSbp not in ontology
  
  
*/
