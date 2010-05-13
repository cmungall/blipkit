:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

% blip -r cell -r ego -u query_obol findall anon_class_replacement/3
anon_class_replacement(Anon,NewClass,Name):-
        is_anonymous(Anon),
        class(Anon,Name),
        class(NewClass,Name),
        NewClass \= Anon.

class_name_error(Class,Name1,Name2):-
        class(Class,Name1),
        class(Class,Name2),
        Name1 \= Name2.

anon_class_refcount(AnonOnt,Anon,AnonName,NumGenusRefs,NumDiffRefs):-
        is_anonymous(Anon),
        belongs(Anon,AnonOnt),
        class(Anon,AnonName),
        setof_count(X,genus(X,Anon),NumGenusRefs),
        setof_count(X,differentium(X,_,Anon),NumDiffRefs).

genus_relation_defcount(RelationName,Genus,GenusName,DefCount):-
        count_by(Genus1,genus(_,Genus1),GenusCounts),
        member(Genus-_,GenusCounts),
        class_or_id(Genus,GenusName),
        count_by(Relation1,(genus(Def,Genus),differentium(Def,Relation1,_)),RelCounts),
        member(Relation-DefCount,RelCounts),
        class_or_id(Relation,RelationName).

class_or_id(Class,Name):- class(Class,Name),!.
class_or_id(Class,Name):- property(Class,Name),!.
class_or_id(Class,Class).

anon_class_definition(Ont,AnonName,Class,ClassName,Def):-
        is_anonymous(Anon),
        class(Anon,AnonName),
        belongs(Anon,Ont),
        differentium(Class,_,Anon),
        class(Class,ClassName),
        (   def(Class,Def)->true;Def='').

