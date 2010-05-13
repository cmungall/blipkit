/* -*- Mode: Prolog -*- */

:- module(obolog_bridge_to_ontol,[
                  ]).

:- use_module(bio(obolog_db)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(obolog_writer_kif),[formula_atom/2]).

% metadata
metadata_db:entity_label(X,L):- label(X,L).

obo_builtin(instance_of).
obo_builtin(is_a).

% types
ontol_db:class(X):- type(X).

% relations
ontol_db:property(X):- relation(X), \+ obo_builtin(X), \+ ((label(X,N),obo_builtin(N))).

ontol_db:is_transitive(X):- obolog_db:transitive(X).
ontol_db:is_reflexive(X):- reflexive(X).
ontol_db:is_symmetric(X):- symmetric(X).
ontol_db:is_asymmetric(X):- asymmetric(X).
ontol_db:is_anti_symmetric(X):- anti_symmetric(X).

ontol_db:logicalformula(X,FA,'KIF'):-
        relation_axiom_direct(X,_,F),
        formula_atom(F,FA).


ontol_db:inverse_of(X,Y):- inverse_of(X,Y).
ontol_db:property_relationship(X,disjoint_over,Y):- disjoint_over(X,Y).
ontol_db:property_relationship(X,all_some_all_times,Y):- all_some_all_times(X,Y).
ontol_db:property_relationship(X,all_some_tr,Y):- all_some_tr(X,Y).
%ontol_db:property_relationship(X,all_some_in_reference_context,Y):- all_some_in_reference_context(X,Y,_).

ontol_db:subclass(X,Y):- subrelation(X,Y).  % legacy : subclass in ontol_db
ontol_db:subclass(X,Y):- formula(is_a(X,Y)).
ontol_db:holds_over_chain(R,[R1,R2]):- obolog_db:holds_over_chain(R,R1,R2).

ontol_db:property_domain(X,Y):- domain(X,Y).
ontol_db:property_range(X,Y):- range(X,Y).
ontol_db:id_axiom(X,Y):- relation(X),formula(Y),Y=..[_,X|_].

metadata_db:entity_resource(X,relationship):- relation(X). % TODO

metadata_db:synonym_type_desc('formal_name','EXACT','an exact synonym that conforms to a formal naming style that may not be appropriate for the human-friendly name').
metadata_db:entity_synonym_scope(X,S,exact):- formal_label(X,S).
metadata_db:entity_synonym_type(X,S,'formal_name'):- formal_label(X,S).
metadata_db:entity_synonym(X,S):- formal_label(X,S).

metadata_db:entity_synonym_scope(X,S,exact):- exact_synonym(X,S).
metadata_db:entity_synonym_scope(X,S,broad):- broad_synonym(X,S).
metadata_db:entity_synonym_scope(X,S,narrow):- narrow_synonym(X,S).
metadata_db:entity_synonym_scope(X,S,related):- related_synonym(X,S).

metadata_db:entity_synonym(X,S):- synonym(X,S).

metadata_db:entity_xref(X,S):- xref(X,S).
metadata_db:entity_xref(X,S):- image_xref(X,S).

% doesn't work if filtered..
metadata_db:entity_comment(E,X):-
        autocomment(E,Toks,[]),
        concat_atom(Toks,X).
metadata_db:entity_example(E,XA):-
	example(E,XT),
        autoexample(XT,Toks,[]),
        concat_atom(Toks,XA).
metadata_db:entity_example(E,XA):-
	example(E,XT,Src),
        autoexample(XT,Toks,[]),
        concat_atom(Toks,XA1),
        sformat(XA,'~w (source: ~w)',[XA1,Src]).



ontol_db:def_xref(E,X):-
        text_definition_xref(E,X).

ontol_db:def(E,X):-
        text_definition(E,D1),
        Ds1=[D1],
        (   class_instance_relation_pair(E,EI),
            ontol_db:def(EI,D2)
        ->  Ds2=['[Instance level: ',D2,'] Between types: '|Ds1]
        ;   Ds2=Ds1),
        Ds2 \= [],
        concat_atom(Ds2,X).
        
autocomment(R) --> {class_instance_relation_pair(R,PR)},!,['[Instance level: '],autocomment2(PR),['] Between types: '],autocomment2(R).
autocomment(R) --> autocomment2(R).
autocomment2(R) --> autocomment_c(R),autocomment_e1(R),autocomment_e2(R).

autocomment_c(R) --> {findall(X,obolog_db:comment(R,X),Xs)},autocomments(Xs).
autocomment_e1(R) --> {findall(X,example(R,X,_Src),Xs)},examples(Xs).
autocomment_e2(R) --> {findall(X,example(R,X,_Src),Xs)},examples(Xs).

autocomments([]) --> !,[].
autocomments([H|T]) --> [H],autocomments(T).

examples([]) --> !,[].
examples([H|T]) --> !,[' Example: '],autoexample(H),examples(T).
autoexample(X) --> {X=..[R,A,B]},!,[A,' '],label(R),[' ',B],['. '].
autoexample(X) --> {X=..[R,A,B,C]},!,[A,' '],label(R),[' ',B,' ',C],['. '].
autoexample(X) --> {X=..L,concat_atom(L,' ',A)},[A],['. '].
label(X) --> {label(X,L)},!,[L].
label(X) --> [X].
