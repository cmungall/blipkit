:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(index_util)).
:- use_module(bio(dbmeta)).
:- use_module(bio(pkb_db)).
:- use_module(bio(simmatrix)).

rqoc('PATO:0001238').
mqoc('PATO:0001237').

qop(Q) :- subclassT(Q,'PATO:0001236').
qoc(Q) :- subclassT(Q,'PATO:0001241').

is_process(P) :- belongs(P,biological_process).
is_process(P) :- belongs(P,molecular_function).
is_continuant(C) :- \+ is_process(C).

simple_property_range_violation(P,Q,X) :-
        differentium(P,'OBO_REL:inheres_in',X),
        genus(P,Q),
        qop(Q),
        \+ is_process(X).
simple_property_range_violation(P,Q,X) :-
        differentium(P,'OBO_REL:inheres_in',X),
        genus(P,Q),
        qoc(Q),
        \+ is_continuant(X).


% remember to load full MP + PATO

missing_differentium(towards,C):-
        genus(C,G),
        rqoc(RQOC),
        subclassT(G,RQOC),
        \+ differentium(C,'OBOL:towards',_).


invalid_differentium(towards,X,G-GN,C):-
        genus(C,G),
        mqoc(MQOC),
        subclassT(G,MQOC),
        differentium(C,'OBOL:towards',X),
        class(G,GN).

diffpair(R,X1,X2,C):-
        genus(C,_),
        differentium(C,R,X1),
        differentium(C,R,X2),
        X1\=X2.

align_phenont(From,To,X1,X2) :-
        belongs(X1,From),
        genus(X1,G),
        differentium(X1,R,D1),
        align_diff(D1,D2),
        differentium(X2,R,D2),
        belongs(X2,To),
        genus(X2,G),
        forall(differentium(X1,Rn,Dn),
               (   align_diff(Dn,Dn2),
                   differentium(X2,Rn,Dn2))),
        forall(differentium(X2,Rn,Dn),
               (   align_diff(Dn,Dn2),
                   differentium(X1,Rn,Dn2))).

% pre-reasoned only
subsumes_phenont(From,To,X1,X2) :-
        belongs(X1,From),
        genus(X1,G1),
        differentium(X1,R1,D1),
        R1\=qualifier,
        subsumes_diff(R1-D1,R2-D2),
        differentium(X2,R2,D2),
        belongs(X2,To),
        genus(X2,G2),
        subclass(G1,G2),
        %debug(align,'candidate ~w ~w ~w ~w ~w',[X1,X2,D1,R1,D2]),
        forall(differentium(X1,Rn,Dn),
               (   (   subsumes_diff(Rn-Dn,Rn2-Dn2),
                       differentium(X2,Rn2,Dn2)
                   ;   \+ differentium(X2,Rn,_)))).
        
align_diff(X,X).
align_diff(D1,D2) :-
        entity_xref(U,D1),
        entity_xref(U,D2),
        D1\=D2.

% pre-reasoned only
subsumes_diff(R-X,R-X).
subsumes_diff(R-X,R-Y) :- subclass(X,Y).
subsumes_diff(R-D1,R-D2) :-
        entity_xref(U1,D1),
        subclass(U1,U2),
        entity_xref(U2,D2),
        D1\=D2.
subsumes_diff('OBO_REL:inheres_in'-D1,'OBO_REL:inheres_in_part_of'-D2) :-
        entity_xref(U1,D1),
        (   restriction(U1,part_of,U2)
        ;   U2=U1),
        entity_xref(U2,D2),
        D1\=D2.

% strict matching between two pre-composed classes in O1 and O2
pclasspair_match(A,B,O1,O2) :-
	genus(A,G),
	id_idspace(A,O1),
	best_differentium(A,_,AX), % det - assumes will match one
	diffmatch(AX,_BX,B,O2),
	id_idspace(B,O2),
	genus(B,G),
	forall(differentium(A,_,AX2),
	       diffmatch(AX2,_,B,O2)),
	forall(differentium(B,_,BX2),
	       diffmatch(BX2,_,A,O1)).

pclasspair_closematch(A,B,O1,O2) :-
	genus(A,G),
	id_idspace(A,O1),
	best_differentium(A,_,AX), % det - assumes will match one
	cdiffmatch(AX,_BX,B,O2),
	id_idspace(B,O2),
	genus(B,G),
	forall(differentium(A,_,AX2),
	       cdiffmatch(AX2,_,B,O2)),
	forall(differentium(B,_,BX2),
	       cdiffmatch(BX2,_,A,O1)).

cdiffmatch(AX,BX,B,O) :-
	diffmatch(AX,BX,B,O).
cdiffmatch(AX,BX,B,O) :-
	parent(AX,AX1),
	diffmatch(AX1,BX,B,O).
cdiffmatch(AX,BX,B,O) :-
	parent(AX1,AX),
	diffmatch(AX1,BX,B,O).

diffmatch(X,X,B,O) :-
	differentium(B,_,X),
	id_idspace(B,O).
diffmatch(AX,BX,B,O) :-
	entity_xref(U,AX),
	entity_xref(U,BX),
	differentium(B,_,BX),
	id_idspace(B,O).

%% best_differentium(+A,?R,?X) is det
best_differentium(A,R,X) :-
	R='OBO_REL:inheres_in',
	differentium(A,R,X),
	!.
best_differentium(A,R,X) :-
	R='OBO_REL:inheres_in_part_of',
	differentium(A,R,X),
	!.
best_differentium(A,R,X) :-
	R='OBO_REL:towards',
	differentium(A,R,X),
	!.
best_differentium(A,R,X) :-
	differentium(A,R,X),
	!.

pclass(G) :- class(G),id_idspace(G,'MP').
pclass(G) :- class(G),id_idspace(G,'HP').

pclass_ref(G,T) :-
	pclass(G),
	genus(G,T).
pclass_ref(G,T) :-
	pclass(G),
	differentium(G,_,T).

% pre-reasoned?
pclasspair_simj(F1,F2,S,Inter) :-
	debug(pheno,'indexing....',[]),
	%generate_term_indexes(G,T,(pclass_ref(G,T1),parentT(T1,T))),
	generate_xp_indexes(Inter),
	debug(pheno,'indexed',[]),
	simmatrix:feature_ix(F1,Ix1),
        simmatrix:feature_ix(F2,Ix2),
	Ix1<Ix2,		%bidirectional, all-by-all.
	id_idspace(F1,O1),
	id_idspace(F2,O2),
	(   Inter
	->  O1\=O2
	;   O1=O2),
        feature_pair_simj(F1,F2,S).

pclasspair_simj_pc(F1,F2,S,Inter) :-
	debug(pheno,'indexing....',[]),
	generate_term_indexes(G,T,
			      (	  pclass(G),
				  parentRT(G,T))
			     ),
	debug(pheno,'indexed',[]),
	simmatrix:feature_ix(F1,Ix1),
        simmatrix:feature_ix(F2,Ix2),
	Ix1<Ix2,
	id_idspace(F1,O1),
	id_idspace(F2,O2),
	(   Inter
	->  O1\=O2
	;   O1=O2),
        feature_pair_simj(F1,F2,S).

% only uses linked terms.
% useful if we are looking at MP vs HP - we don't expect any overlap where
% the species-specific AO is concerned
xpi(G,T) :-
	pclass_ref(G,T1),
	parentRT(T1,T).

generate_xp_indexes(true) :-
	!,
	generate_term_indexes(G,T,
			      (	  xpi(G,T),
				  \+ id_idspace(T,'MA'),
				  \+ id_idspace(T,'FMA')
			      )).
generate_xp_indexes(_) :-
	generate_term_indexes(G,T,xpi(G,T)).


is_ss_idspace('MA').
is_ss_idspace('FMA').
is_ss_idspace('ZFA').
is_ss_idspace('ZFS').
is_ss_ao(X) :- id_idspace(X,S),is_ss_idspace(S).

excluded(T) :- is_ss_ao(T).
excluded('-').
excluded(T) :- is_abnormal(T).


% TODO: move this into phenoblast
% hardcoded for inter-species, must use uberon
organism_pair_simj_maxIC(F1,F2,S,MaxIC,Matches) :-
	generate_term_indexes(G,T,
			      (	  organism_phenotype_affects(G,_,T),
				  \+ excluded(T))
			     ),
	debug(pheno,'indexed',[]),
	simmatrix:feature_ix(F1,Ix1),
        simmatrix:feature_ix(F2,Ix2),
	Ix1<Ix2,
        feature_pair_simj(F1,F2,S),
	feature_pair_maxIC_attributes(F1,F2,MaxIC,Matches).

organism_pair_simj_all(F1,F2,S,ICMatchPairs) :-
	generate_term_indexes(G,T,
			      (	  organism_phenotype_affects(G,_,T),
				  \+ excluded(T))
			     ),
	debug(pheno,'indexed',[]),
	simmatrix:feature_ix(F1,Ix1),
        simmatrix:feature_ix(F2,Ix2),
	Ix1<Ix2,
        feature_pair_simj(F1,F2,S),
	feature_vector(F1,V1),
	feature_vector(F2,V2),
	AVI is V1 /\ V2,
	Len is popcount(AVI),
	Len > 15,
	vector_attributes(AVI,Atts),
	setof(A,nr(A,Atts),AttsNR),
	setof(IC-A,(member(A,AttsNR),attribute_information_content(A,IC),IC>1.5),ICMatchPairs).



organism_phenotype_pair_simj(O1, P1, O2, P2, S, MaxIC) :-
	generate_term_indexes(P, T, 
			      (	  organism_phenotype_affects(_, P, T), 
				  \+ excluded(T))
			     ), 
	debug(pheno, 'indexed', []), 
	debug(pheno, 'force cache...', []), 
	findall(F,  feature_vector(F, _),  _), 
	% for frequencies we count by organism,  not phenotype
	debug(pheno, 'calculating freqs', []), 
	retractall(simmatrix:template(_, _, _)), 
        assert(simmatrix:template(O, T, 
				  organism_phenotype_affects(O, _, T))), 
	materialize_index(simmatrix:attribute_feature_count(1,0)), 
	debug(pheno, 're-indexed', []), 
	organism(O1), 
	organism_species(O1, S1), 
	organism(O2), 
	organism_species(O2, S2), 
	%S1\=S2, 
	O1 @< O2,
	organism_phenotype(O1, P1), 
	organism_phenotype(O2, P2), 
	%feature_pair_ci(P1, P2, S),
	%S>20.
	feature_pair_simj(P1, P2, S),
	(   S1=S2
	->  S > 0.6
	;   S > 0.4),
	feature_pair_maxIC(P1,P2,MaxIC),
	(   S1=S2
	->  MaxIC > 6.5
	;   MaxIC > 4.5).


organism_phenotype_pair_iscore(O1, P1, O2, P2, Atts, Prob) :-
	generate_term_indexes(P, T, 
			      (	  organism_phenotype_affects(_, P, T),
				  \+ excluded(T))
			     ), 
	debug(pheno, 'indexed', []), 
	organism(O1), 
	organism_species(O1, S1), 
	organism(O2), 
	organism_species(O2, S2), 
	S1\=S2, 
	O1\=O2,			% true if above true
	organism_phenotype(O1, P1), 
	organism_phenotype(O2, P2), 
	feature_pair_simj(P1, P2, S), 
	S > 0.4,
	phenotype_pair_intersection_probability(P1,P2,Atts,Prob).

phenotype_pair_prob_atts(P1, P2, Prob, Atts) :-
	generate_term_indexes(P, T, 
			      (	  organism_phenotype_affects(_, P, T),
				  \+ excluded(T))
			     ), 
	debug(pheno, 'indexed', []), 
	aggregate(count,Org,organism(Org),PopSize),
	simmatrix:feature_ix(P1,Ix1),
        simmatrix:feature_ix(P2,Ix2),
	Ix1<Ix2,
	phenotype_pair_intersection_probability(P1,P2,0.1,PopSize,Prob,Atts).


			     
%% feature_pair_intersection_probability(+F1,+F2,?Atts,?P)
% Atts is the set of attributes shared by F1 and F2, P
% is the frequency of features that have that set divided by
% total number of features.
% note: only works in cases where features have minimal number
%  of attributes and we expect to see the same set
phenotype_pair_intersection_probability(F1,F2,ProbThresh,PopSize,Prob,AttsNR) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVI is AV1 /\ AV2,
	ISize is popcount(AVI),
	ISize > 10,
	aggregate(count,Org,P^(av_subsumes_feature(AVI,P),organism_phenotype(Org,P)),SampleSize),
	Prob is SampleSize/PopSize,
	Prob < ProbThresh,
	vector_attributes(AVI,Atts),
	setof(A,nr(A,Atts),AttsNR).



nr(A,Atts) :-
	member(A,Atts),
	\+ ((member(A2,Atts),
	     A2\=A,
	     bf_parentRT(A2,A),
	     \+ bf_parentRT(A,A2))).

	

% recommend: index organism_phenotype_affects(1,0,1)
% post-comp
organism_phenotype_affects(O,P,A) :-
	organism_phenotype_quad(O,P,PQ),
	class_quad_aspect(A1,PQ,_),
	bf_parentRT(A1,A).
% pre-comp AND e.g. HP1 -> HP2 -> diff -> U1 -> U2 ...
organism_phenotype_affects(O,P,A) :-
	organism_phenotype(O,P),
	class(P),
	bf_parentRT(P,A).

%:- use_module(bio(swindex)).
%:- swindex(organism_phenotype_affects(1,0,1)).


%organism_phenotype_affects(O,P,A,OrgFreq,PopFreq,Ratio) :-
organism_phenotype_affects(O,P,A,SubPhenotypesFreq,TotalPhenotypes,Ratio) :-
	organism_phenotype_affects(O,P,A),
	affected_freq_org(A,OrgPopFreq),
	aggregate(count,P2,organism_phenotype_affects(O,P2,A),SubPhenotypesFreq),
	aggregate(count,P2,organism_phenotype(O,P2),TotalPhenotypes),
	Ratio is (SubPhenotypesFreq / TotalPhenotypes)/OrgPopFreq.	

affected_freq(A,Num) :-
	aggregate(count,O-P,organism_phenotype_affects(O,P,A),Num).

affected_freq_org(A,Num) :-
	aggregate(count,O,P^organism_phenotype_affects(O,P,A),Num).

abnormal_level(Ph,ab,C,E) :-
        genus(Ph,'PATO:0000033'), % concentration
        differentium(Ph,qualifier,AQ),
        is_abnormal(AQ),
        differentium(Ph,'OBO_REL:towards',C),
        (   differentium(Ph,'OBO_REL:inheres_in',E)
        ->  true
        ;   E=''),
        \+ extra_diff(Ph,_,_).


abnormal_process(Ph,ab,C,E) :-
        genus(Ph,'PATO:0000001'), % quality
        differentium(Ph,qualifier,AQ),
        is_abnormal(AQ),
        differentium(Ph,'OBO_REL:inheres_in',Proc),
        differentium(Proc,_,C),
        \+ extra_diff(Ph,_,_).

abnormal_process(Ph,ab,C,E) :-
        genus(Ph,'PATO:0000001'), % quality
        differentium(Ph,qualifier,AQ),
        is_abnormal(AQ),
        differentium(Ph,'OBO_REL:inheres_in',Proc),
        id_idspace(Proc,'GO'),
        differentium(Ph,'OBOL:has_central_participant',E).

extra_diff(X,R,Y) :-
        differentium(X,R,Y),
        \+ standard_eqr(R).

standard_eqr('OBO_REL:inheres_in').
standard_eqr('OBO_REL:towards').
standard_eqr('qualifier').

static_processual(SP,PP,R) :-
        abnormal_level(SP,Qual,C,E),
        abnormal_level(PP,Qual,C,E),
        inf_rel(SP,PP,R).

inf_rel(SP,PP,<) :- subclass(SP,PP).
inf_rel(SP,PP,>) :- subclass(PP,SP).
inf_rel(SP,PP,<<) :- \+subclass(SP,PP),subclassT(SP,PP).
inf_rel(SP,PP,>>) :- \+subclass(PP,SP),subclassT(PP,SP).



is_abnormal('PATO:0000460').




