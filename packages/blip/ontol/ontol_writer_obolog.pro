/* -*- Mode: Prolog -*- */

% TODO: write this as a bridge rather than writer

% DEPRECATED?: use ontol_bridge_to_obolog

:- module(ontol_writer_obolog, [
                                   write_ontology/2,
                                   write_header/1,
                                   write_class/2,
                                   write_property/2
                                  ]).

:- use_module(bio(mode)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

io:redirect_stdout(obolog).

io:write_all(obolog,_,_):-
        write_header(obolog),
        %dsetof(O,ID^N^(belongs(ID,O),class(ID,N)),Os),
        % collect all ontology IDs first
        dsetof(O,ID^entity_resource(ID,O),Os),
        % check for inclusion/exclusion specified by user
        forall((member(O,Os),io:in_include_list(O,ontology)),
               write_ontology(obolog,O)),
        %forall_distinct(inst_of(ID,_),
        %write_inst(obolog,ID)),
        % write entities that dont belong to an ontology
        solutions(ID,(has_info(ID),\+entity_resource(ID,_)),IDs),
        maplist(write_entity(obolog),IDs).


% TODO - DRY - writer_obo
has_info(ID):- class(ID).
has_info(ID):- property(ID).
has_info(ID):- inst(ID).
has_info(ID):- subclass(ID,_).
has_info(ID):- restriction(ID,_,_).

write_header(obolog):-
        wsxpr(obolog_version(dq('0.01'))),
        forall(idspace(Local,Global),
               wsxpr(idspace(Local,dq(Global)))),
        nl.

:- mode write_ontology(+,+) is det.
write_ontology(obolog,O):-
        forall_distinct(belongs(ID,O),
                        write_entity(obolog,ID)).

write_entity(obolog,Class):-
        class(Class),
        debug(obolog,'writing class ~w',[Class]),
        write_class(obolog,Class).
write_entity(obolog,Class):-
        property(Class),
        debug(obolog,'writing property ~w',[Class]),
        write_property(obolog,Class),
        write_type_level_property(obolog,Class).
write_entity(obolog,Inst):-
        inst(Inst),
        debug(obolog,'writing inst~w',[Inst]),
        write_instance(obolog,Inst).
write_entity(obolog,X):-
        debug(obolog,'unknown: ~w',[X]).

write_instance(obolog,Class):-
        wsxpr(instance(id(Class))),
        forall(instance(Class,Name),
               wsxpr(label(id(Class),dq(Name)))).

write_class(obolog,Class):-
        wsxpr(type(id(Class))),
        forall(class(Class,Name),
               wsxpr(label(id(Class),dq(Name)))),
        forall(subclass(Class,Parent),wsxpr(is_a(id(Class),id(Parent)))),
        forall(genus(Class,Parent),wsxpr(genus(id(Class),id(Parent)))),
        forall(restriction(Class,Relation,ToClass),
               (   Term=..[Relation,
                           id(Class),
                           id(ToClass)],
                   wsxpr(Term))),
        forall(differentium(Class,Relation,ToClass),
               (   Term=..[differentium,
                           Relation,
                           id(Class),
                           id(ToClass)],
                   wsxpr(Term))),
        forall(class_disjoint_union_list(Class,XL),
               (   findall(id(X),
                           member(X,XL),
                           XL2),
                   %Term=..[equivalent_to,id(Class)|XL2],
                   Term=..[disjoint_union|XL2],
                   wsxpr(equivalent_to(id(Class),Term)))),
        forall((inst_sv(Class,Slot,Val,_Type),\+ builtin_annotation_property(Slot)),
               wsxpr(class_metadata(id(Class),id(Slot),dq(Val)))),
        write_entity_generic(Class),
        nl.

write_property(obolog,Rel):-
        wsxpr(relation(id(Rel))),
        forall(property(Rel,Name),
               wsxpr(label(id(Rel),dq(Name)))),
        forall(subclass(Rel,Parent),
               wsxpr(subrelation(id(Rel),
                                 id(Parent)))),
        forall(property_domain(Rel,Domain),
               wsxpr(domain(id(Rel),
                            id(Domain)))),
        forall(property_range(Rel,Range),
               wsxpr(range(id(Rel),
                           id(Range)))),
        forall(inverse_of(Rel,Inverse),
               wsxpr(inverse_of(id(Rel),
                                id(Inverse)))),
        forall((property_relationship(P,R,F),H=..[R,id(P),id(F)]),
               wsxpr(H)),
        forall((property_intersection_elements(Rel,L),findall(id(X),member(X,L),L2)),
               wsxpr(equivalent_to(id(Rel),[intersection_of|L2]))),
        forall((property_union_elements(Rel,L),findall(id(X),member(X,L),L2)),
               wsxpr(equivalent_to(id(Rel),[union_of|L2]))),
        forall(transitive_over(Rel,Over),
               wsxpr(transitive_over(id(Rel),
                                     id(Over)))),
        forall(holds_over_chain(Rel,[R1,R2]),
               wsxpr(holds_over_chain(id(Rel),
                                      id(R1),
                                      id(R2)))),
        forall(holds_temporally_between(Rel,Sub,Ob),
               wsxpr(holds_temporally_between(id(Sub),
                                              id(Ob)))),
        forall(holds_atemporally_between(Rel,Sub,Ob),
               wsxpr(holds_atemporally_between(id(Sub),
                                               id(Ob)))),
        forall(all_some(Rel),wsxpr(all_some(id(Rel)))),
        forall(holds_for_all_times(Rel),wsxpr(holds_for_all_times(id(Rel)))),
        forall(is_proper(Rel),wsxpr(proper(id(Rel)))),
        forall(is_transitive(Rel),wsxpr(transitive(id(Rel)))),
        forall(is_symmetric(Rel),wsxpr(symmetric(id(Rel)))),
        forall(is_anti_symmetric(Rel),wsxpr(anti_symmetric(id(Rel)))),
        forall(is_reflexive(Rel),wsxpr(reflexive(id(Rel)))),
        forall(is_cyclic(Rel),wsxpr(cyclic(id(Rel)))),
        forall((inst_sv(Class,Slot,Val,_Type),\+ builtin_annotation_property(Slot)),
               wsxpr(class_metadata(id(Class),id(Slot),dq(Val)))),
        write_entity_generic(Rel),
        nl.

rel_typelevel_localid(Rel,TypeRel):-
        property(Rel,Name),
        atom_concat(Name,'_some',TypeRel).

rel_typelevel_id(Rel,TypeRel):-
        concat_atom([DB,Local],':',Rel),
        atom_concat(DB,'_C',TDB),
        concat_atom([TDB,Local],':',TypeRel).

        
write_type_level_property(obolog,Rel):-
        rel_typelevel_localid(Rel,TypeRel),
        wsxpr(relation(id(TypeRel))),
        wsxpr(type_type(id(TypeRel))),
        wsxpr(all_some_all_times(id(TypeRel),id(Rel))),
        forall(property(Rel,Name),
               (   atom_concat(Name,'_some',TypeName),wsxpr(label(id(TypeRel),dq(TypeName))))),
        forall(subclass(Rel,Parent),
               (   rel_typelevel_localid(Parent,TypeParent),
                   wsxpr(subrelation(id(TypeRel),
                                     id(TypeParent))))),
        forall(is_transitive(Rel),wsxpr(transitive(id(Rel)))),
        forall(is_symmetric(Rel),wsxpr(symmetric(id(Rel)))),
        forall(is_anti_symmetric(Rel),wsxpr(anti_symmetric(id(Rel)))),
        forall(is_reflexive(Rel),wsxpr(reflexive(id(Rel)))),
        forall(is_cyclic(Rel),wsxpr(cyclic(id(Rel)))),
        %forall(holds_over_chain(R,R1,R2),wsxpr(holds_over_chain(id(R),id(R1),id(R2)))),
        %forall(transitive_over(R,R1),wsxpr(transitive_over(id(R),id(R1)))),
        forall((inst_sv(Class,Slot,Val,_Type),\+ builtin_annotation_property(Slot)),
               wsxpr(class_metadata(id(Class),id(Slot),dq(Val)))),
        forall((entity_alternate_identifier(Rel,X),idprivileged_local(X,_Local)),
               (   rel_typelevel_id(X,ID),
                   wsxpr(exported_identifier(id(TypeRel),dq(ID))))),
        nl.

write_entity_generic(E):-
        forall(entity_comment(E,X),
               wsxpr(comment(id(E),dq(X)))),
        forall(entity_synonym_scope(E,X,related),
               wsxpr(related_synonym(id(E),dq(X)))),
        forall(entity_synonym_scope(E,X,exact),
               wsxpr(exact_synonym(id(E),dq(X)))),
        forall(entity_synonym_scope(E,X,narrow),
               wsxpr(narrow_synonym(id(E),dq(X)))),
        forall(entity_synonym_scope(E,X,broad),
               wsxpr(broad_synonym(id(E),dq(X)))),
        forall(entity_alternate_identifier(E,X),
               wsxpr(alternate_identifier(id(E),dq(X)))),
        forall((entity_alternate_identifier(E,X),idprivileged_local(X,_)), % for original RO
               wsxpr(exported_identifier(id(E),dq(X)))),
        forall(idprivileged_local(E,_), % for BFO
               wsxpr(exported_identifier(id(E),dq(E)))),
        forall(def(E,X),
               wsxpr(text_definition(id(E),dq(X)))),
        forall(def_xref(E,X),
               wsxpr(text_definition_xref(id(E),dq(X)))).


builtin_annotation_property('rdfs:comment').
        
wsxpr(Term):-
        sxpr(Term,Tokens,[]),
        maplist(write,Tokens).

sxpr(_-id(X)) --> {idprivileged_local(X,X2),!},[X2].
sxpr(_-id(X)) --> !,{concat_atom(L,':',X),concat_atom(L,'__',X2)},[X2].
sxpr(_-dq(X)) --> !,{concat_atom(L,'"',X),concat_atom(L,'\\"',X2)},['"',X2,'"'].
%sxpr(_-dq(X)) --> !,{concat_atom(L,'x',X),concat_atom(L,'xx',X2)},[z].
sxpr(D-L) --> {is_list(L),!,D2 is D+1},['('],sxpr_list(D2-L),[')','\n'].
sxpr(D-T) --> {compound(T),!,T=..L},sxpr(D-L).
%sxpr(_-X) --> !,[X,' '].
sxpr(_-X) --> !,[X].
sxpr(X)   --> sxpr(0-X).
sxpr_list(D-[H]) --> !,sxpr(D-H).
sxpr_list(D-[H|T]) --> !,sxpr(D-H),[' '],sxpr_list(D-T).
sxpr_list(_-[]) --> [].

idprivileged_local(X,X2):- concat_atom([DB,X2],':',X),privileged(DB).
privileged('OBO_REL').
privileged('span').
privileged('snap').
privileged('bfo').

