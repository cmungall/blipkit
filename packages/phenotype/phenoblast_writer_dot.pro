:- module(phenoblast_writer_dot,
	  [feature_pair_to_dotgraph/3]).

:- use_module(bio(bioprolog_util)).
:- use_module(bio(phenoblast)).
:- use_module(bio(phenotype_db)).
:- use_module(bio(metadata_db)).

sk(ID,ID) :- atom(ID),!.
sk((A,B,C,D),S) :- concat_atom([A,B,C,D],'-',S).

format_label(N,N2) :-
        concat_atom(Toks,' ',N),
        concat_atom(Toks,'\\n',N2).

system:graphviz_pheno_param(E,P,_) :-
	system:graphviz_pheno_param(E,P).
system:graphviz_pheno_param(node((A,B,C,D)),label=N2):- !, L=[A,B,C,D], solutions(N1,(member(X,L),entity_label(X,N1)),N1s),concat_atom(N1s,' ',N), format_label(N,N2).
system:graphviz_pheno_param(node(ID),label=N2):- entity_label(ID,N), !, format_label(N,N2).
system:graphviz_pheno_param(node(ID),label=S):- sk(ID,S),!.
system:graphviz_pheno_param(edge(_,_,S),label=S).
system:graphviz_pheno_param(edge(_,_,S),weight=S).

feature_pair_to_dotgraph(F1,F2,G) :-
	method_feature_pair_phenosim(psimj,F1,F2,Results),
	member(bestmatches(L1,L2),Results),
	solutions(M,(   member(M,L1)
		    ;	member(M,L2)),Ms),
	solutions(P,(   member(P-_-_,Ms)
		    ;	member(_-P-_,Ms)),Ps),
	findall(Term,
                (   member(P,Ps),
                    findall(Param,system:graphviz_pheno_param(node(P),Param,Opts),NodeParams),
		    sk(P,PX),
                    Term=node(PX,NodeParams)),
                NodeTerms),
        solutions(edge(P1X,P2X,EdgeParams),
                (   member(P1-P2-S,Ms),
		    sk(P1,P1X),
		    sk(P2,P2X),
                    findall(Param,system:graphviz_pheno_param(edge(P1,P2,S),Param,Opts),EdgeParams)),
                EdgeTerms),
        append(NodeTerms,EdgeTerms,Terms),
	G=graph(g,[],Terms).


/** <module> 

  ---+ Synopsis

==
:- use_module(bio(phenoblast_writer_dot)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
