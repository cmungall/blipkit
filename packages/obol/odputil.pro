:- module(odputil,
          [
           pattern/5,
           write_tuple/1,
           write_yaml/1
           ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).


% ========================================
% UTILS
% ========================================

% ignore synonym and def fields
pattern(P,G,Ds,Fs,Vs) :- pattern(P,G,Ds,Fs,Vs,_,_).




% ========================================
% REVERSE ENGINEERING
% ========================================


% some patterns are paired -
% check both members present
nomatch(P1,no-P2,C1) :-
        paired(P1,P2),
        pattern_class_vars(P1,C1,Vars),
        \+ pattern_class_vars(P2,_,Vars).
nomatch(no-P1,P2,C2) :-
        paired(P1,P2),
        pattern_class_vars(P2,C2,Vars),
        \+ pattern_class_vars(P1,_,Vars).

:- dynamic(written_tuple/1).

%% write_tuple(+Pattern)
%
% writes a class tuple
write_tuple(P) :-
        % write header row
        pattern_idmap(P,_,_,Cols,_),
        format('iri,label'),
        forall(member(Col,Cols),
               format(',~w,~w label',[Col,Col])),
        nl,
        fail.

write_tuple(P) :-
        % write row in body
        pattern_idmap(P,G,Ds,_,Vars),
        setof(C,Ds^class_cdef(C,cdef(G,Ds)),Cs),
        member(C,Cs),
        call_unique(class_cdef(C,cdef(G,Ds))),
        write_class(C),
        maplist(tab_write_class,Vars),
        nl,
        fail.

%% label_object(+Name, ?ObjectID)
%
% maps a class label to a class ID
% or a relation label to the ID
label_object(N,C) :- class(C,N),!.
label_object(N,X) :- property(R,N),entity_xref(R,X),!.
label_object(N,X) :- property(N),entity_xref(N,X),!.
label_object(N,P) :- property(P,N),!.
label_object(N,C) :-
        concat_atom(L,'_',N),
        L=[_,_|_],
        concat_atom(L,' ',N2),
        label_object(N2,C).


tab_write_class(C) :-
        write(','),
        write_class(C).
write_class(C) :-
        write(C),
        write(','),
        (   class(C,N)
        ->  format('"~w"',N)
        ;   format(user_error,'NO LABEL: ~w~n',[C])).

% ---
% utils for reverse engineering
% ---

pattern_idmap(P,G,Ds,Fs,Vs) :-
        pattern(P,G1,Ds1,Fs,Vs),
        map_class(G1,G),
        maplist(map_class,Ds1,Ds).

map_class(V,V) :- var(V),!.
map_class(R=D,R2=D2) :- !, map_class(R,R2),map_class(D,D2).
map_class(n(N),C) :- label_object(N,C),!.
map_class(n(N),_) :- throw(no_label(N)).
map_class(C,C) :- !.



pattern_class_vars(P,C,Vars) :-
        pattern_idmap(P,G,Ds,_,Vars),
        setof(C,Ds^class_cdef(C,cdef(G,Ds)),Cs),
        member(C,Cs),
        class_cdef(C,cdef(G,Ds)).




% ========================================
% YAML PATTERN GENERATION
% ========================================

lex_prop(Scope=Lex,Prop,Lex) :- !, scope_prop(Scope,Prop).
lex_prop(Lex,Prop,Lex) :- !, scope_prop(exact,Prop).

scope_prop(exact,'oio:hasExactSynonym').
scope_prop(related,'oio:hasRelatedSynonym').
scope_prop(broad,'oio:hasBroadSynonym').
scope_prop(narrow,'oio:hasNarrowSynonym').


write_yaml(P) :-
        pattern(P,G,Ds,Fields,PrologVars,Lexs,LexDef),
        format('pattern_name: ~w~n',[P]),
        nl,
        format('classes:~n',[]),
        forall((cdef_class_in_sig(cdef(G,Ds),n(N)),nonvar(N),label_object(N,C)),
               format('  ~w: ~w~n',[N,C])),
        nl,
        format('relations: ~n',[]),
        forall((cdef_relation_in_sig(cdef(G,Ds),N),label_object(N,R)),
               format('  ~w: ~w~n',[N,R])),
        % TODO
        nl,
        format('vars:~n',[]),
        forall(member(F,Fields),
               format('  ~w: "Thing"~n',[F])),
        nl,
        Lexs=[Name|Syns],

        lex_vars(Name,NameVars),
        format('name:~n',[]),
        format('  text: "'),write_lexpattern(Name),format('"~n'),
        format('  vars:~n'),
        forall(member(V,NameVars),
               format('   - ~w~n',[V])),
        nl,
        format('annotations:~n'),
        forall(member(SynLex,Syns),
               (   lex_prop(SynLex,LexProp,Syn),
                   lex_vars(Syn,SynVars),
                   format('  - property: ~w~n',[LexProp]),
                   format('    text: "'),write_lexpattern(Syn),format('"~n'),
                   format('    vars:~n'),
                   forall(member(V,SynVars),
                          format('     - ~w~n',[V])))),
        nl,
        LexDef=DefFmt-DefVars,
        format('def:~n',[]),
        format('  text: "~w"~n',[DefFmt]),
        format('  vars:~n'),
        forall(member(V,DefVars),
               format('    - ~w~n',[V])),               
        nl,
        cdef_owl(cdef(G,Ds),Fields,PrologVars,OwlFmt,OwlVars),
        format('equivalentTo:~n',[]),
        format('  text: "~w"~n',[OwlFmt]),
        format('  vars:~n'),
        forall(member(V,OwlVars),
               format('    - ~w~n',[V])),
               
        nl,
               
        nl.

lexical_to_rlist(H-T,[T|HL]) :-
        !,
        lexical_to_rlist(H,HL).
lexical_to_rlist(H,[H]).

%% lexical_to_list(+LexPatternTerm,?TokList)
%
% e.g. LexPatternTerm = duct-of-n(gland)
lexical_to_list(H,L) :-
        lexical_to_rlist(H,RL),
        reverse(RL,L).

lex_vars(H,Vs) :-
        lexical_to_list(H,L),
        findall(V,
                member(v(V),L),
                Vs).


write_lexpattern(H) :-
        lexical_to_list(H,L),
        write_lexlist(L).

write_lexlist([H|T]) :-
        T=[_|_],
        !,
        write_lexunit(H),
        format(' '),
        write_lexlist(T).
write_lexlist([H]) :-
        !,
        write_lexunit(H).


write_lexunit(v(_)) :-
        !,
        write('%s').
write_lexunit(X) :-
        write(X).

cdef_owl(CDef,Fields,PrologVars,A,Vars) :-
        % HACK
        entity_partition(R,logical_definition_view_relation),
        !,
        cdef_owl_impl(CDef,Fields,PrologVars,A1,Vars),
        sformat(A,'\'~w\' some (~w)',[R,A1]).

cdef_owl(CDef,Fields,PrologVars,A,Vars) :-
        cdef_owl_impl(CDef,Fields,PrologVars,A,Vars).


cdef_owl_impl(cdef(G,Ds),Fields,PrologVars,A,Vars) :-
        findall(v(F),member(F,Fields),VFields),
        maplist(=,PrologVars,VFields),
        findall(EA-EAVars,
                (   member(R=D,Ds),
                    sformat(RQ,'\'~w\'',[R]),
                    toks_to_atom([RQ,some,D],EA,EAVars)),
                Pairs),
        toks_to_atom([G],GA,GVars),
        findall(EA,member(EA-_,Pairs),EAs),
        findall(EAVars,member(_-EAVars,Pairs),EAVarsSets),
        flatten(EAVarsSets,EAVarsAll),
        concat_atom([GA|EAs],' and ',A),
        append(GVars,EAVarsAll,Vars).


/*
old___cdef_owl_impl(cdef(G,Ds),Fields,PrologVars,A,Vars) :-
        findall(v(F),member(F,Fields),VFields),
        maplist(=,PrologVars,VFields),
        findall(EA,    %%%  [RQ,some,D],
                (   member(R=D,Ds),
                    sformat(RQ,'\'~w\'',[R]),
                    toks_to_atom([RQ,some,D],EA)),
                EAs),
        flatten(DToks,DToksFlat),
        Toks=[G,and|DToksFlat],
        toks_to_atom(G,GA,Vars),
        toks_to_atom(Toks,A,Vars).
*/

toks_to_atom(Toks,A,Vars) :-
        toks_to_lexs(Toks,As,Vars),
        concat_atom(As,' ',A).


toks_to_lexs([],[],[]).
toks_to_lexs([v(V)|T],['%s'|T2],[V|Vs]) :-
        !,
        toks_to_lexs(T,T2,Vs).
toks_to_lexs([n(N)|T],[Tok|T2],Vs) :-
        !,
        sformat(Tok,'\'~w\'',[N]),
        toks_to_lexs(T,T2,Vs).
toks_to_lexs([H|T],[H|T2],Vs) :-
        !,
        toks_to_lexs(T,T2,Vs).

%% cdef_class_in_sig(+CDef,?Cls)
%
% true if Cls is a class in the signature of CDef
cdef_class_in_sig(cdef(G,_),G) :- nonvar(G).
cdef_class_in_sig(cdef(_,Ds),D) :- member(_=D,Ds),nonvar(D).

cdef_relation_in_sig(cdef(_,Ds),R) :- member(R=_,Ds),nonvar(R).

% HACK
cdef_relation_in_sig(_,R) :- entity_partition(R,logical_definition_view_relation).



% ========================================
% OBOL: Text -> Pattern
% ========================================

lexmatch_class(P,Cls,CDef) :-
        lexmatch_class(P,Cls,CDef,_).
lexmatch_class(P,Cls,CDef,CombinedScope) :-
        entity_label_scope(Cls,Label,_Scope),
        lexmatch_term(P,Label,CDef,CombinedScope).


lexmatch_term(P,Label,cdef(G,Ds)) :-
        lexmatch_term(P,Label,cdef(G,Ds),_).
lexmatch_term(P,Label,cdef(G,Ds),CombinedScope) :-
        pattern(P,G,Ds,Fields,PrologVars,Lexs,_),
        nth0(_Pos,Lexs,Lex),
        lexical_to_list(Lex,TargetToks),
                                % optimization step
        forall((member(Tok,TargetToks),atom(Tok)),
               sub_atom(Label,_,_,_,Tok)),
        findall(Var,member(v(Var),TargetToks),Vars),
        maplist(bind_field(PrologVars,Vars),Fields,Bindings),
        tokens_match(TargetToks,Label,Bindings,Scopes),
        combine_scopes(Scopes,CombinedScope).

combine_scopes(L,R) :-
        combine_scopes(L,label,R).
combine_scopes([],R,R) :- !.
combine_scopes([S|T],In,Out) :-
        combine_scope_pair(S,In,Combined),
        combine_scopes(T,Combined,Out).

combine_scope_pair(S1,S2,R) :-
        scope_rank(S1,R1),
        scope_rank(S2,R2),
        R is max(R1,R2).

scope_rank(label,1).
scope_rank(exact,2).
scope_rank(related,3).
scope_rank(broad,4).
scope_rank(narrow,4).


%% bind_field(+PrologVars:list, +Fields:list, ?Field, ?PrologVar:var) is det
bind_field(PrologVars,Fields,Field,PrologVar) :-
        nth0(Pos,Fields,Field),
        nth0(Pos,PrologVars,PrologVar).

tokens_match([],'',[],[]).
tokens_match(T,Label,Bindings,Scopes) :-
        atom_concat(' ',Rest,Label),
        !,
        tokens_match(T,Rest,Bindings,Scopes).
tokens_match([v(_Type)|T],Label,[C|Bindings],[Scope|Scopes]) :-
        !,
        % true if C has label that matches the start of Label
        token_match_cls(C,Label,Rest,Scope),
        tokens_match(T,Rest,Bindings,Scopes).
tokens_match([H|T],Label,Bindings,Scopes) :-
        !,
        atom_concat(H,Rest,Label),
        tokens_match(T,Rest,Bindings,Scopes).

token_match_cls(C,Text,Rest,Scope) :-
        entity_label_scope(C,Base,Scope),
        atom_concat(Base,Rest,Text).

ix_labels :-
        materialize_index(entity_split_label_scope(+,+,+,+)).
        

