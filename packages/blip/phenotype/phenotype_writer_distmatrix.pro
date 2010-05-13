:- module(phenotype_writer_distmatrix,[]).

:- use_module(phenotype_db).
:- use_module(pkb_db).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).

io:format_writer(distmatrix,phenotype_writer_distmatrix).

% semi-hack: indicates that this writes to stdout
io:redirect_stdout(distmatrix).
        
io:write_all(distmatrix,_,_Filter):-
        solutions(F1,
		  (   method_feature_pair_phenosim(_,F1,_,_),
		      organism_label(F1,_)),
		  Fs),
        length(Fs,Num),
        format(' ~d~n',[Num]),
        findall(F-ID,(member(F,Fs),
		      gensym(fnode,ID)),
		FMap),
        %findall(F-ID,(member(F,Fs),feature_nodelabel(F,ID)),FMap),
        forall(member(F-ID,FMap),
               (   organism_treenodelabel(F,N),
                   format(user_error,'~w ~w~n',[ID,N]))),
        forall(member(F1,Fs),
               write_row(FMap,Fs,F1)).

organism_treenodelabel(F,Label) :-
	organism_label(F,FN),
	solutions(GN,
		(   (	organism_disease_variant_gene(F,_,G)
		    ;	organism_variant_gene(F,G)),
		    entity_label(G,GN)),
		GNs),
	concat_atom([FN,'GENES:'|GNs],' ',Label).


write_row(FMap,Fs,F1) :-
        member(F1-F1X,FMap),
        writef('%10L',[F1X]),
        forall(member(F2,Fs),
               (   (   F1=F2
                   ->  Score = 0
                   ;   (   feature_pair_phenosim_value(F1,F2,max_ic,MaxIC),
			   feature_pair_phenosim_value(F1,F2,avg,ICCS)
				%                       ->  Score is 10 - (MaxIC + ICCS/5) / 5
                       ->  Score is  1 - (MaxIC + ICCS/5) / 15
                       ;   Score is 1)),
                   format(' ~4f',[Score]))),
        nl.

replace_unsafe([],[]) :- !.
replace_unsafe([H|T],[H2|T2]) :-
        replace_unsafe(H,H2),
        !,
        replace_unsafe(T,T2).
replace_unsafe([_|T],T2) :-
        !,
        replace_unsafe(T,T2).

replace_unsafe(H,H) :- H @>= 'a', H @=< 'z'.
replace_unsafe(H,H) :- H @>= 'A', H @=< 'Z'.
replace_unsafe('_','_').
replace_unsafe(' ','_').
replace_unsafe(H,H) :- H @>= '0', H @=< '9'.

/** <module> write a distance matrix from phenoblast similarities


*/
