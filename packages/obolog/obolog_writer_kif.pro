/* -*- Mode: Prolog -*- */

:- module(obolog_writer_kif, [
                              write_formula/1,
                              formula_atom/2
                              ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(obolog_db)).
:- use_module(bio(bioprolog_util)).

io:redirect_stdout(kif).
io:redirect_stdout(cl).
io:redirect_stdout(clif).
io:redirect_stdout(obolog).

io:write_all(obolog,X,Y):-
	io:write_all(kif,X,Y).
io:write_all(clif,X,Y):-
	io:write_all(cl,X,Y).

io:write_all(kif,_,_):-
        forall(formula(F),
               wsxpr(F)).

io:write_all(cl,_,_):-
        ensure_loaded(bio(kif2cl)),
        forall(formula(F),
               (   kif_to_cl(F,FX),
                   wsxpr(FX),
                   nl)).

write_formula(F):- wsxpr(F).

wsxpr(Term):-
        sxpr(Term,Tokens,[]),
        maplist(write,Tokens),
        nl.


formula_atom(F,A):-
        sxpr(F,Toks,[]),
        concat_atom(Toks,A).

sxpr(_-id(X)) --> {idprivileged_local(X,X2),!},[X2].
sxpr(_-id(X)) --> !,{concat_atom(L,':',X),concat_atom(L,'__',X2)},[X2].
sxpr(_-dq(X)) --> !,{concat_atom(L,'"',X),concat_atom(L,'\\"',X2)},['"',X2,'"'].
sxpr(D-forall(VL,T)) --> {D2 is D+1},['(forall ('],sxpr_list(D-VL),[')'],['\n'],tab(D2),sxpr(D2-T),[')'].
sxpr(D-if(A,C)) --> {D2 is D+1},['(if '],sxpr(D-A),['\n'],tab(D2),sxpr(D2-C),[')'].
sxpr(D-L) --> {is_list(L),forall(member(X,L),atom(X)),!},['('],sxpr_list(D-L),[')']. % one line
sxpr(D-L) --> {is_list(L),!,D2 is D+1},['\n'],tab(D),['('],sxpr_list(D2-L),[')']. % split over lines
sxpr(D-T) --> {compound(T),!,T=..L},sxpr(D-L).
sxpr(_-X) --> {is_safe(X)},!,[X].
sxpr(_-X) --> !,sxpr(_-dq(X)).
sxpr(X)   --> sxpr(0-X).        % initialize tabbing
sxpr_list(D-[H]) --> !,sxpr(D-H).
sxpr_list(D-[H|T]) --> !,sxpr(D-H),[' '],sxpr_list(D-T).
sxpr_list(_-[]) --> [].
tab(0) --> !,[].
tab(D) --> !,[' '],{Dm1 is D-1},tab(Dm1).



idprivileged_local(X,X2):- concat_atom([DB,X2],':',X),privileged(DB).
privileged('OBO_REL').
privileged('span').
privileged('snap').
privileged('bfo').

%is_safe(X):- sformat(X,'~q',[X]).
is_safe(X):- concat_atom([_],' ',X). % TODO - need better way pf preserving symbol vs string distinction

/** <module> writes obolog out as KIF

  ---+ Synopsis

==
:- use_module(bio(obolog_writer_kif)).

% 
demo:-
  nl.
  

==

---+ Details

TODO - this is currently more of a generic logic-formula-as-S-expression writer


@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
