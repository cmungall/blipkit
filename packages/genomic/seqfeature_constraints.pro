:- module(seqfeature_bridge_from_simple_graph,
          [
           structural_axiom/3
          ]).

:- use_module(bio(seqfeature_db)).

structural_axiom(any_exon_must_be_directly_nested_in_a_transcript,
                 '',
                 forall(implied_feature_type(X,exon),
                        (   feature_relationship(X,Y),
                            implied_feature_type(Y,transcript)))).
structural_axiom(any_transcript_must_be_directly_nested_in_a_gene,
                 '',
                 forall(implied_feature_type(X,transcript),
                        (   feature_relationship(X,Y),
                            implied_feature_type(Y,gene)))).


:- begin_tests(axioms).

:- use_module(bio(seqfeature_sqlmap_chado)).
:- use_module(bio(sql_compiler)).


axiom2sql(forall(Pre,Post),Sql):-
        plterm_to_sqlterm(Pre,(Pre,not(Post)),SqlTerm),
        sqlterm2atom(SqlTerm,Sql).

test(foo):-
        writeln(foo).
test(axioms):-
        forall(structural_axiom(Name,Desc,Axiom),
               (   writeln(structural_axiom(Name,Desc,Axiom)),
                   axiom2sql(Axiom,Sql),
                   writeln(Sql))).

:- end_tests(axioms).


