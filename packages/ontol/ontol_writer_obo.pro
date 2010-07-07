/* -*- Mode: Prolog -*- */

:- module(ontol_writer_obo, [
                             write_ontology/2,
                             write_header/1,
%                             write_class/2,
                             write_entity/2,
                             write_property/2,
                             write_inst/2,
			     write_axiom/2,
			     write_retract_axiom/2,
                             write_cdef/2,
                             write_cdef/3,
                             obo_format_option/1
                             ]).

:- multifile ontol_writer:write_class/2.

:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(serval)).
:- use_module(library('semweb/rdf_db'),[rdf_global_id/2]).

:- op(800,xfy,forall_unfiltered).

:- multifile obo_format_option/1.
%:- dynamic obo_format_option/1.

forall_unfiltered(Template,Goal) =>
  forall_unique(Template,(Goal,\+ suppress_fact(Goal))).

% advanced obo1.3 option
% note: this is overridden in curation_writer_obo
obo_format_option(pheno_syntax):- fail.

obo_format_option(remap_identifiers):- fail.

% semi-hack: indicates that this writes to stdout
io:redirect_stdout(obo).
        
io:write_all(obo,_,_Filter):-
        write_header(obo),
        %dsetof(O,ID^N^(belongs(ID,O),class(ID,N)),Os),
        % collect all ontology IDs first
        dsetof(O,ID^entity_resource(ID,O),Os),
        % check for inclusion/exclusion specified by user
        forall((member(O,Os),io:in_include_list(O,ontology)),
               write_ontology(obo,O)),
        % write entities that dont belong to an ontology
        solutions(ID,(has_info(ID),\+entity_resource(ID,_)),IDs),
        maplist(write_entity(obo),IDs),
        write_undeclared_properties.

has_info(ID):- class(ID).
has_info(ID):- property(ID).
has_info(ID):- inst(ID).
has_info(ID):- entity_obsolete(ID,_).
%has_info(ID):- subclass(ID,_).
%has_info(ID):- restriction(ID,_,_).

is_class(X):- class(X).
%is_class(X):- subclass(X,_).
is_class(X):- restriction(X,_,_).

% tests if class is referenced
% (this is a hack to deal with orphan expressions from old owl conversion)
is_referenced(X) :- inst_of(_,X).
is_referenced(X) :- restriction(_,_,X).
is_referenced(X) :- subclass(_,X).
is_referenced(X) :- genus(_,X).
is_referenced(X) :- differentium(_,_,X).


:- mode write_ontology(+,+) is det.
write_ontology(obo,O):-
        forall_distinct(entity_resource(ID,O),
                        write_entity(obo,ID)).
        
:- mode write_header(+) is det.
write_header(obo):-
        format('format-version: 1.2~n'),
        forall_distinct(idspace(Local,Global),
                        format('idspace: ~w ~w ""~n',[Local,Global])),
        forall_distinct((partition(SubSet),entity_label(SubSet,Label)),
                        format('subsetdef: ~w "~w"~n',[SubSet,Label])),
        forall_distinct(synonym_type_desc(S,T,D),
                        format('synonymtypedef: ~w "~w" ~w~n',[S,D,T])),
       % hack to unify part_of etc
%        solutions(RN-R,(   (   restriction(_,RN,_)
%                           ;   differentium(_,RN,_)),
%                           property(R,RN),
%                           R\=RN),
%                  RMaps),
%        maplist(write_idmap,RMaps),
        nl.

write_idmap(ID-To):-
        format('id-mapping: ~w ~w~n',[ID,To]).

:- mode write_class(+,+) is det.
ontol_writer:write_class(obo,ID,_):-
        is_class(ID),
        !,
        (   is_anonymous(ID),
            \+ is_referenced(ID)
        ->  true                % suppress anonymous orphans
        ;   write_stanza(obo,ID,'Term')).

ontol_writer:write_class(obo,ID,_):-
        %throw(cannot_write_class(ID)).
	write_stanza(obo,ID,'Term').

ontol_writer:write_class(obo_make_disjoint,ID,_):-
        class(ID),
        !,
        forall(subclass(CID,ID),
               write_disjoint_term(CID,ID)).


:- mode write_property(+,+) is det.
write_property(obo,ID):-
        property(ID),
        write_stanza(obo,ID,'Typedef').

write_undeclared_properties :-
        solutions(P,(restriction(_,P,_),\+ property(P)),Ps),
        maplist(write_undeclared_property(obo),Ps).

:- mode write_undeclared_property(+,+) is det.
write_undeclared_property(obo,ID):-
        \+ property(ID),
        write_stanza(obo,ID,'Typedef').

:- mode write_entity(+,+) is det.
write_entity(obo,ID):-
        debug(ontol_detail,'writing ~w',ID),
        (   property(ID)
        ->  write_property(obo,ID)
        ;   is_class(ID)
        ->  write_class(obo,ID)
        ;   entity_obsolete(ID,class)
        ->  write_stanza(obo,ID,'Term')
        ;   entity_obsolete(ID,property)
        ->  write_stanza(obo,ID,'Typedef')
        ;   entity_obsolete(ID,instance)
        ->  write_stanza(obo,ID,'Instance')
        ;   inst(ID)
        ->  write_inst(obo,ID)
        ;   format(user_error,'Unknown: ~w~n',[ID])). % no longer assume it's a nameless inst

:- mode write_cdef(+,+) is det.
write_cdef(obo,CDef):-
        CDef=cdef(ID,_),
        write_cdef(obo,CDef,ID).

:- mode write_cdef(+,+) is det.
write_cdef(obo,ID,CDef):-
        !,
        %user:ensure_loaded(bio(ontol_db)), % serval runs in user space...
        %user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        write_sterm([],xml([]),cdef(ID,CDef)),
        !.



% bit of a hack..
% useful for making disjointness axioms
write_disjoint_term(ID,PID):-
        class(ID,N),
        format('[Term]~nid: ~w  ! ~w~n',[ID,N]),
        forall((subclass(SID,PID),SID\=ID),
               write_disjoint_tag(SID)),
        nl.
write_disjoint_tag(ID):-
        class(ID,N),
        format('disjoint_from: ~w ! ~w~n',[ID,N]).

% (+,?)
% many relation IDs are without prefix (in which case it matches the name)
% map this to the real ID
%property_real_id(R,R2):-  THIS IS DANGEROUS!!!
%        property(R2,R),!.
property_real_id(R,R).

:- mode write_stanza(+,+,+,+) is det.
write_stanza(obo,ID,StanzaType):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        % do not write anon terms in phenosyntax mode - they will always be referred to by expression
        (   is_anonymous(ID),
            ontol_writer_obo:obo_format_option(pheno_syntax)
        ->  true
        ;   write_sterm([],xml([]),stanza(ID,StanzaType))),
        !.
%        ;   write_sterm([[exclude_dangling=1]],xml([]),stanza(ID,StanzaType))).

write_stanza(obo,ID,StanzaType):-
        throw(cannot_write(StanzaType-ID)).

:- mode write_inst(+,+) is det.
write_inst(obo,ID):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        write_sterm([],xml([]),inst(ID)).

write_axiom(obo,Ax):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        write_sterm([],xml([]),axiom(Ax)).

write_retract_axiom(obo,Ax):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        write_sterm([],xml([]),retract_axiom(Ax)).

synonym_scope_upcase(Class,Syn,Scope):-
        entity_synonym_scope(Class,Syn,Scope0),
        upcase_atom(Scope0,Scope).

newline => NL where atom_codes(NL,"\n").
tag(Tag) => Tag,': '.
tvpair(Tag,Rel-Val) => tag(Tag),property(Rel),' ',escape_val(Val).
tvpair(Tag,Val) => tag(Tag),escape_val(Val).
tvpairnl(Tag,Val) => tvpair(Tag,Val),newline.

escape_val(Val) => noesc(Esc) where ontol_writer_obo:obo_escape(Val,Esc).

quoted(X) => '"',escape_val(X),'"'.

tagnodenl(S,Tag,Node) => tagnode(S,Tag,Node),newline.
tagnodenl(S,Tag,Rel,Node) => tagnode(S,Tag,Rel,Node),newline.

tagnode(S,Tag,Node) =>
 getparam(exclude_dangling,ExcludeDangling,0 ), % todo
 if((ExcludeDangling=0 ; is_nondangler(Node)),
    [tag(Tag),identifier(Node),' ',
     entailment(S),
     id_info_as_comment(Node)]).
% obo_comment(Name) where id_comment(Node,Name).
tagnode(S,Tag,Rel,Node) =>
 tag(Tag),property(Rel),' ',identifier(Node),' ',
 entailment(S),
 id_info_as_comment(Node).
% obo_comment(Name) where id_comment(Node,Name).

entailment(S) => qualifiers([implied=true,implication_rule=R]) where entailed_by(S,R).
%entailment(S) => qualifiers([implied=true]) where entailed_by(S,_).

identifier(Node) =>
  if( (is_anonymous(Node),
       ontol_writer_obo:obo_format_option(pheno_syntax),
       genus(Node,Genus)),
      then:
    genus_diff_id(Node,Genus),
      else:
    if(ontol_writer_obo:obo_format_option(remap_identifiers),  % performance hit? perhaps map outside?
       then: mapid(Node),
%       else: data(Node))).
       else: escape_identifier(Node))).

escape_identifier(intersectionOf(L)) => jmap('^',X,identifier(X),L).
escape_identifier(unionOf(L)) => jmap('|',X,identifier(X),L).
escape_identifier(someValuesFrom(R,Y)) => identifier(R),'(',identifier(Y),')'.
escape_identifier(complementOf(C)) => '-',identifier(C).
% TODO: cardinality
escape_identifier(X) => escape_val(X).


genus_diff_id(Node,Genus)=>
  identifier(Genus),
  differentium_idpart(R,X) forall differentium(Node,R,X).

differentium_idpart(R,To)=>
  '^',
  property(R),
  '(',
  identifier(To),
  ')'.

mapid(ID) =>
  if(alt_id(RealID,ID),
    then: data(RealID),
    else: data(ID)).

% many relation IDs are without prefix (in which case it matches the name)
% map this to the real ID -- DEPRECATED
%property(Rel) => if(property(ActualID,Rel),
%                    then: data(ActualID),
%                    else: data(Rel)).
property(Rel) => data(Rel).

booltag(Tag) => tag(Tag),true,newline.

id_info_as_comment(ID)=>
 if(entity_label(ID,N),
    then:obo_comment(N),
    else:if(entity_alternate_identifier(NewID,ID),
            then: obo_comment(['alt_id_for ',NewID, ' ',quoted(NN) where entity_label(NewID,NN)]))),
 if(((ID=intersectionOf(_)->true;true),entity_obsolete(ID,_)),
    then: [obo_comment([obsolete,
                       [   ' consider: ', AltID, ' ',quoted(AltN) where entity_label(AltID,AltN)] forall entity_consider(ID,AltID),
                       [   ' replaced_by: ', AltID, ' ',quoted(AltN) where entity_label(AltID,AltN)] forall entity_replaced_by(ID,AltID)])
          ],
    else: []).


obo_comment(X) => [' ! ',X].

qualifiers([]) => ''.
qualifiers(L) => ' {',qualifiers1(L),'}'.
qualifiers1([X]) => qualifier(X).
qualifiers1([X|L]) => qualifier(X),',',qualifiers1(L).
qualifier(X=Y) => [X,'=',Y].
qualifier(X) => [X].


%xrefs(L) => ' [',map(X,xref(X),L),']'.   % TODO - comma separated
xrefs(L) => ' [',xrefs1(L),']'.   % TODO - comma separated
xrefs1([]) => ''.
xrefs1([X]) => xref(X).
xrefs1([X|L]) => xref(X),', ',xrefs1(L).
%xrefs(L) => ' [',join(', ',map(X,xref(X),L)),']'.   % TODO - comma separated
xref(X) => escape_val(X).

xsd(DataType) => if(concat_atom([xsd,_],':',DataType),
                    then: data(DataType),
                    else: ['xsd:',data(DataType)]).

def(Class,Def)=>
  call(findall(X,def_xref(Class,X),Xs)),
  tag(def),quoted(Def),xrefs(Xs),
  newline.
% sometimes we have a def_xref but no def text (e.g. wikipedia)
emptydef(Class)=>
  call(findall(X,def_xref(Class,X),Xs)),
  tag(def),quoted('.'),xrefs(Xs),
  newline.
synonym(Class,Syn)=>
  tag(synonym),quoted(Syn),
  [' ',Scope] where ontol_writer_obo:synonym_scope_upcase(Class,Syn,Scope),
  [' ',Type] where entity_synonym_type(Class,Syn,Type),
  call(findall(X,entity_synonym_xref(Class,Syn,X),Xs)),
  xrefs(Xs),
  newline.
inst_sv(ID,R,V,DataType)=>
  tag(property_value),property(R),' ',quoted(V),' ',xsd(DataType),
  qualifiers([id=BNode]) where reification(BNode,inst_sv(ID,R,V)),
  newline.
inst_rel(ID,R,V)=>
  tag(property_value),property(R),' ',identifier(V),
  qualifiers([id=BNode]) where reification(BNode,inst_rel(ID,R,V)),
  newline.
logicalformula(_ID,Formula,Lang)=>
  tag(formula),quoted(Formula),
  ' ',Lang,
  xrefs([]),
  newline.

relationship_tag(Tag,_ID,card(Rel,Min,Max),X)=>
  tag(Tag),property(Rel),' ',identifier(X),
  if(Min=Max,
     then: qualifiers([cardinality=Min]),
     else: qualifiers([min_cardinality=Min,max_cardinality=Max])),
  id_info_as_comment(X),
  newline.
relationship_tag(Tag,_ID,card(Rel,Q),X)=>
  tag(Tag),property(Rel),' ',identifier(X),
  qualifiers([cardinality=Q]),
  id_info_as_comment(X),
  newline.
relationship_tag(Tag,ID,Rel,X)=>
  tag(Tag),property(Rel),' ',identifier(X),
  qualifiers([id=BNode]) where reification(BNode,restriction(ID,Rel,X)),
  qualifiers([implied=true,implication_rule=S]) where entailed_by(restriction(ID,Rel,X),S),
  id_info_as_comment(X),
  newline.
relationship_tag(Tag,ID,Rel,X,A3)=>
  tag(Tag),property(Rel),' ',identifier(X),' ',A3,
  qualifiers([id=BNode]) where reification(BNode,restriction(ID,Rel,X,A3)),
  qualifiers([implied=true,implication_rule=S]) where entailed_by(restriction(ID,Rel,X,A3),S),
  id_info_as_comment(X),' ',
  id_info_as_comment(A3),
  newline.
relationship_tag(Tag,ID,Rel,identifier(X),A3,A4)=>
  tag(Tag),property(Rel),' ',identifier(X),' ',A3,' ',A4,
  qualifiers([id=BNode]) where reification(BNode,restriction(ID,Rel,X,A3,A4)),
  qualifiers([implied=true,implication_rule=S]) where entailed_by(restriction(ID,Rel,X,A3,A4),S),
  id_info_as_comment(X),' ',
  id_info_as_comment(A3),
  id_info_as_comment(A4),
  newline.


openstanza(StanzaType) =>
  '[',StanzaType,']',newline.

stanza(ID,StanzaType) =>
   openstanza(StanzaType),
   tvpairnl(id,ID),
   tvpairnl(name,N) where entity_label(ID,N),
   tvpairnl(namespace,X) where belongs(ID,X),
   tvpairnl(alt_id,X) forall_unfiltered entity_alternate_identifier(ID,X),
   def(ID,X) where def(ID,X),
   emptydef(ID) where (def_xref(ID,_),!,\+def(ID,_)),
   booltag(is_obsolete) where entity_obsolete(ID,_),
   tagnodenl(replaced_by(ID,X),replaced_by,X) forall_unfiltered entity_replaced_by(ID,X),
   tagnodenl(consider(ID,X),consider,X) forall_unfiltered entity_consider(ID,X),
   booltag(is_anonymous) where is_anonymous(ID),
   booltag(is_metadata_tag) where is_metadata_tag(ID),
   booltag(is_class_level) where is_class_level(ID),
   booltag(is_transitive) where is_transitive(ID),
   booltag(is_reflexive) where is_reflexive(ID),
   booltag(is_symmetric) where is_symmetric(ID),
   booltag(is_functional) where is_functional(ID),
   booltag(is_inverse_functional) where is_inverse_functional(ID),
   booltag(is_anti_symmetric) where is_anti_symmetric(ID),
   booltag(is_cyclic) where is_cyclic(ID),
   tagnodenl(inverse_of(ID,X),inverse_of,X) where inverse_of(ID,X),
   tagnodenl(inverse_of_on_instance_level(ID,X),inverse_of_on_instance_level,X) where inverse_of_on_instance_level(ID,X),
   tagnodenl(complement_of(ID,X),complement_of,X) where complement_of(ID,X),
   tagnodenl(cyclic_over(ID,X),cyclic_over,X) where cyclic_over(ID,X),
   tagnodenl(transitive_form_of(ID,X),transitive_form_of,X) where transitive_form_of(ID,X),
   tagnodenl(proper_form_of(ID,X),proper_form_of,X) where proper_form_of(ID,X),
   tvpairnl(lexical_category,X) where lexical_category(ID,X),
   tvpairnl(comment,X) forall_unfiltered class_comment(ID,X),
   tvpairnl(example,X) forall_unfiltered entity_example(ID,X), % DEPRECATED/UNSUPPORTED?
   tagnodenl(true,subset,X) forall_unfiltered entity_partition(ID,X),
   synonym(ID,X) forall_unfiltered entity_synonym(ID,X),
   tvpairnl(xref,X) forall_unfiltered entity_xref(ID,X),
   tagnodenl(subclass(ID,X),is_a,X) forall_unfiltered subclass(ID,X),
   relationship_tag(relationship,ID,T,X) forall_unfiltered restriction(ID,T,X),
   relationship_tag(relationship,ID,T,X,A3) forall_unfiltered restriction(ID,T,X,A3),
   relationship_tag(relationship,ID,T,X,A3,A4) forall_unfiltered restriction(ID,T,X,A3,A4),
   tagnodenl(true,R,X) forall_unfiltered property_relationship(ID,R,X), % ontol_db reifies the relation
   tagnodenl(true,intersection_of,X) forall_unfiltered genus(ID,X),
   relationship_tag(intersection_of,ID,T,X) forall_unfiltered differentium(ID,T,X),
   %tagnodenl(true,intersection_of,R,X) forall_unfiltered differentium(ID,R,X),
   tagnodenl(true,intersection_of,X) forall_unfiltered (class_intersection_element(ID,X),X\=(_=_)),
   relationship_tag(intersection_of,ID,T,X) forall_unfiltered class_intersection_element(ID,T=X),
   tagnodenl(true,intersection_of,X) forall_unfiltered (property_intersection_element(ID,X)),
   tagnodenl(true,union_of,X) forall_unfiltered (property_union_element(ID,X)),
   tagnodenl(true,disjoint_from,X) forall_unfiltered disjoint_from(ID,X),
   tagnodenl(true,union_of,X) forall_unfiltered class_union_element(ID,X),
   tagnodenl(true,domain,X) forall_unfiltered property_domain(ID,X),
   tagnodenl(true,range,X) forall_unfiltered property_range(ID,X),
   tagnodenl(true,transitive_over,R1) forall_unfiltered transitive_over(ID,R1),
   tagnodenl(true,holds_over_chain,R1,R2) forall_unfiltered holds_over_chain(ID,[R1,R2]),
   inst_rel(ID,R,X) forall inst_rel(ID,R,X), % annotation object properties
   inst_sv(ID,R,V,DT) forall_unfiltered inst_sv(ID,R,V,DT), % annotation datatype properties
   logicalformula(ID,Formula,Lang) forall_unfiltered logicalformula(ID,Formula,Lang), % annotation datatype properties
   tvpairnl(created_by,X) where entity_created_by(ID,X),
   tvpairnl(creation_date,X) where entity_creation_date(ID,X),
   newline.

axiom(subclass(A,B)) =>
  openstanza('Term'),
  tagnodenl(id(A),id,A),
  tagnodenl(subclass(A,B),is_a,B),
  newline.

retract_axiom(subclass(A,B)) =>
  openstanza('Term'),
  tagnodenl(id(A),id,A),
  '-',
  tagnodenl(subclass(A,B),is_a,B),
  newline.
  


inst(ID) =>
  openstanza('Instance'),
  tvpairnl(id,ID),
  tvpairnl(name,N) where inst(ID,N),
  %tvpairnl(instance_of,X) where inst_of(ID,X),
  tagnodenl(instance_of(ID,X),instance_of,X) forall_unfiltered inst_of(ID,X),
  tvpairnl(namespace,X) where entity_resource(ID,X),
  tvpairnl(comment,X) forall_unfiltered entity_comment(ID,X),
  synonym(ID,X) forall_unfiltered entity_synonym(ID,X),
  tvpairnl(xref,X) forall_unfiltered entity_xref(ID,X),
  call(findall(X-R-Class,
               (   inst_rel_anon(ID,R,Class),
                   gensym('_:',X)),
               BNodes)),
  tagnodenl(true,property_value,R,X) forall member(X-R-_,BNodes),
  %tagnodenl(true,property_value,R,X) forall inst_rel(ID,R,X),
  relationship_tag(relationship,ID,T,X) forall_unfiltered inst_rel(ID,T,X),
  relationship_tag(relationship,ID,T,X,A3) forall_unfiltered inst_rel(ID,T,X,A3),
  relationship_tag(relationship,ID,T,X,A3,A4) forall_unfiltered inst_rel(ID,T,X,A3,A4),
  relationship_tag(relationship,ID,T,X,A3,A4,A5) forall_unfiltered inst_rel(ID,T,X,A3,A4,A5),
  %inst_rel(ID,R,X) forall inst_rel(ID,R,X),
  inst_sv(ID,R,X,DT) forall inst_sv(ID,R,X,DT),
  booltag(is_anonymous) where is_anonymous(ID),
  newline,
  anon_inst(X-Class) forall member(X-_-Class,BNodes).

cdef(ID,cdef(Genus,Diffs)) =>
   openstanza('Term'),
   tagnodenl(id(ID),id,ID),
   %tvpair(id,ID),
   tagnodenl(true,intersection_of,Genus),
   tagnodenl(true,intersection_of,R,X) forall_unfiltered member(R=X,Diffs),
   newline.

anon_inst(ID,Class) =>
  openstanza('Instance'),
  tvpairnl(id,ID),
  tvpairnl(instance_of,Class) where nonvar(Class),
  booltag(is_anonymous).

:- mode obo_escape(+,?) is det.
obo_escape(In,Out):-
        rdf_global_id(NS:Local,In),
        !,
        % lingering URI IDs - convert now
        concat_atom([NS,Local],':',Out).
obo_escape(In,Out):-
        \+ compound(In),
        !,
        atom_chars(In,InChars),
        obo_escape_chars(InChars,OutChars),
        atom_chars(Out,OutChars).
obo_escape(In,Out):- % compound
        is_list(In),
        !,
        maplist(obo_escape,In,L2),
        concat_atom(L2,'__',X),
        obo_escape(X,Out1),
        atom_concat('_:',Out1,Out).
obo_escape(In,Out):- % compound
        debug(ontol_detail,' escaping ~w',[In]),
        In=..L,
        !,
        maplist(obo_escape,L,L2),
        concat_atom(L2,'__',X),
        obo_escape(X,Out1),
        atom_concat('_:',Out1,Out).

obo_escape_chars([],[]).
obo_escape_chars([H|T],['\\',H|T2]):-
        must_be_escaped(H),
        !,
        obo_escape_chars(T,T2).
obo_escape_chars(['\n'|T],['\\',n|T2]):-
        !,
        obo_escape_chars(T,T2).
obo_escape_chars(['\r'|T],T2):-
        !,
        obo_escape_chars(T,T2).
%obo_escape_chars([H|T],[H2|T2]):-
%        obo_replace_char(H,H2),
%        !,
%        obo_escape_chars(T,T2).
obo_escape_chars([H|T],[H|T2]):-
        !,
        obo_escape_chars(T,T2).

%obo_replace_char('\n','\\n').
%obo_replace_char(X,X).

must_be_escaped('"').
must_be_escaped('\\').
%must_be_escaped('\'').
must_be_escaped('{').
must_be_escaped('}').
must_be_escaped('!').

        

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.14 $
  @date  $Date: 2006/03/27 17:57:51 $
  @license LGPL

  ---+ Name
  ---++ ontol_writer_obo
- generates obo format from ontol_db

  ---+ Synopsis

  ==
  

  ==

  ---+ Description

**/
