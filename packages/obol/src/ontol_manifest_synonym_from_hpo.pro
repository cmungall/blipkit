:- module(ontol_manifest_synonym_from_hpo,
          []).

:- use_module(bio(ontol_db)).

metadata_db:entity_synonym(Class,Stemmed):-
        plsyn(Class,Stemmed).
metadata_db:entity_synonym_scope(Class,Stemmed,exact):-
        plsyn(Class,Stemmed).

%plsyn(C,S):-
%        class(C,N),
%        atom_concat(S,s,N).
% sclerae -> sclera
%plsyn(C,S):-
%        class(C,N),
%        atom_concat(SX,ae,N),
%        atom_concat(SX,a,S).

plsyn(C,S):- plsyn1(C,S).
plsyn(C,S):- plsyn1(C,S1),downcase_atom(S1,S),S1\=S.
plsyn(C,S):-
        class(C,N),
        downcase_atom(N,S),
        S\=N.

plsyn1(C,S):-
        class(C,N),
        concat_atom(Toks,' ',N),
        maplist(repl,Toks,XToks),
        flatten(XToks,XToks2),
        concat_atom(XToks2,' ',S).

%plsyn1(C,S):-
%        class(C,N),
%        concat_atom(Toks,' ',N),
%        findall(Tok,(member(Tok,Toks),\+omit(Tok)),XToks),
%        concat_atom(XToks,' ',S).

plsyn1(C,S):-
        class(C,N),
        joinword(W,WX),
        atom_concat(W,Rest,N),
        concat_atom([WX,Rest],' ',S).


joinword(micro,small).
joinword(macro,large).
joinword(mega,large).
joinword(hyper,increased).
joinword(hypo,decreased).
joinword(hepatocellular,'liver cell').

endrepl(s,'').
endrepl(ae,a).
endrepl(ed,e).
endrepl(ies,y). % eg ovary
%endrepl(ii,ius).
endrepl(i,us).
endrepl(ses,sis).
endrepl(al,'').
endrepl(ic,'').
endrepl(ing,ed).

repl(involving,of).
repl(phalangeal,phalanx).
repl(tali,talus).

repl(calves,calf).
repl(feet,foot).
repl(fistula,divided).
repl('1st',first).
repl('2nd',second).
repl('3rd',third).
repl('4th',fourth).
repl('5th',fifth).
repl('(feet)',[of,foot]).
repl('(hands)',[of,hand]).
repl('phalanges',[phalanx,of,foot]).
repl(metacarpal,[metacarpal,bone]).
repl(metatarsal,[metatarsal,bone]).
repl(finger,[digit,of,hand]).
repl(toe,[digit,of,foot]).
repl(the,[]).
repl(a,[]).

repl(N,S):-
        endrepl(From,To),
        atom_concat(X,From,N),
        atom_concat(X,To,S).
repl(X,X).

omit(the).
omit(a).

