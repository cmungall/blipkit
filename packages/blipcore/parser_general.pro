newline(C):- [C]="\r".
newline(C):- [C]="\n".
newline --> "\r\n".
newline --> "\n".

newlines --> newline,!,newlines.
newlines --> [].

word(W) --> alphanumerics(CL),{atom_codes(W,CL)}.

fnumber(N) --> fnumerics(CL), {atom_codes(A,CL),atom_number(A,N)}.
fnumerics([N|NL]) --> fnumeric(N),!,fnumerics(NL).
fnumerics([]) --> [].

number(N) --> numerics(CL), {atom_codes(A,CL),atom_number(A,N)}.
numerics([N|NL]) --> numeric(N),!,numerics(NL).
numerics([]) --> [].

alphanumerics([H|T]) --> alphanumeric(H), alphanumerics(T), !.
alphanumerics([H]) --> alphanumeric(H), !.

alphanumeric(A) --> [A], { code_type(A, csym) }.

numeric(A) --> [A], { code_type(A, digit) }.

fnumeric(A) --> numeric(A).
fnumeric(A) --> [A],{A=0'.}.

valuecodes([H|T]) --> valuecode(H), !, valuecodes(T).
valuecodes([]) --> [].

valuecode(A) --> alphanumeric(A).
valuecode(A) --> [A],{A=0'.}.

token(X^SL) --> token_codes(CL^SL),!,{atom_codes(X,CL)}.
token_codes([C|Cs]^SL) --> "\\",[C],!,token_codes(Cs^SL).
token_codes([C|Cs]^SL) --> [C],{not(token_member(C,SL))},token_codes(Cs^SL),!.
%token_codes([]^_) --> [].
token_codes([C]^SL) --> [C],{not(token_member(C,SL))},!.

not_newlines([C|Cs]) --> not_newline(C),!,not_newlines(Cs).
not_newlines([]) -->[].

not_newline(C) --> [C],{\+newline(C)}.
not_newline(C) --> ["\\"],[C].

%not_dquotes([C|Cs]) --> [C],{[C]\=""""},!,not_dquotes(Cs).
%not_dquotes([]) --> [].

not_ws_atom(A) --> not_ws(Cs),{atom_codes(A,Cs)}.
not_ws([C|Cs]) --> [C],{\+ws(C)},not_ws(Cs),!.
not_ws([C]) --> [C],{\+ws(C)},!.

ws_star --> [C],{ws(C)},!,ws_star.
ws_star --> [].

ws_plus --> [C],{ws(C)},!,ws_star.

ws(0' ).
ws(10).
ws(9).
ws(C):- newline(C).

token_member(X,L):- newline(X),member(nl,L),!.
token_member(X,L):- ws(X),member(ws,L),!.
token_member(X,L):- member(X,L).
