/* -*- Mode: Prolog -*- */



:- module(ontol_writer_intermine,[
                                ]).

:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).
%:- use_module(bio(curation_bridge_to_ontol)).
:- use_module(bio(serval)).

io:redirect_stdout(intermine).
io:write_all(intermine,_,_):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        user:ensure_loaded(bio(curation_db)),   % serval runs in user space... TODO; inherit this?
        write_sterm([],xml([]),model).

user:make_id(T,T):- atom(T),!.
user:make_id(T,ID):- sformat(ID,'~q',[T]).

user:node_link_metatype(Su,R,Ob,'org.obd.InstanceLevelLink'):-
        inst_rel(Su,R,Ob).
user:node_link_metatype(Su,R,Ob,'org.obd.PropertyValue'):-
        inst_sv(Su,R,Ob,_).
user:node_link_metatype(Su,'OBO_REL:instance_of',Ob,'org.obd.InstanceOfLink'):-
        inst_of(Su,Ob).
user:node_link_metatype(Su,R,Ob,'org.obd.AllSomeLink'):-
        restriction(Su,R,Ob).
user:node_link_metatype(Su,'OBO_REL:is_a',Ob,'org.obd.IsALink'):-
        subclass(Su,Ob).
user:node_link_metatype(Su,'OBO_REL:is_a',Ob,'org.obd.GenusLink'):-
        genus(Su,Ob).
user:node_link_metatype(Su,R,Ob,'org.obd.DifferentiumLink'):-
        differentium(Su,R,Ob).
           
model =>
 xml:model(xmlns='http://www.obd.org/model/intermine#',
           node(X,'org.obd.RelationNode') forall property(X),
           node(X,'org.obd.ClassNode') forall class(X),
           node(X,'org.obd.InstanceNode') forall inst(X),
           curation(X) forall curation(X)).

node(X,T) =>
  xml:item(id=X,
           class=T,
           xml:attribute(name=source,value=S) forall entity_resource(X,S),
           xml:attribute(name=label,value=N) forall entity_label(X,N),
           xml:collection(name=aliasSet,
                          ref(alias(X,V)) forall entity_synonym(X,V))),
  alias(X,V) forall entity_synonym(X,V),
  node_links(X).


node_links(Su) =>
 xml:item(class=MT,
          xml:attribute(name=node,value=Su),
          xml:attribute(name=predicate,value=Rel),
          xml:attribute(name=object,value=Ob)) forall user:node_link_metatype(Su,Rel,Ob,MT).

ref(T) =>
 call(make_id(T,ID)),
 xml:reference(ref_id=ID).
               
alias(X,A) =>
 call(make_id(alias(X,A),ID)),
 xml:item(id=ID,
          class='org.obd.Alias',
          xml:attribute(name=label,value=A),
          xml:attribute(name=scope,value=Scope) forall entity_synonym_scope(X,A,Scope),
          xml:attribute(name=category,value=Cat) forall entity_synonym_type(X,A,Cat),
          xml:collection(name=xrefSet,
                     data(Xref)) forall entity_synonym_xref(X,A,Xref)).

curation(X) =>
 if(curation_statement(X,Su,Rel,Ob),
    then:
   [
   xml:item(class='org.obd.AnnotationNode',
            xml:collection(name=evidenceSet,
                           ref(E) forall curation_evidence(X,E)),
            xml:collection(name=provenanceSet,
                           ref(P) forall curation_source(X,P)),
            xml:collection(name=assignedBySet,
                         ref(S) forall curation_publisher(X,S)),
            xml:attribute(name=node,value=Su),
            xml:attribute(name=predicate,value=Rel),
            xml:attribute(name=object,value=Ob)),
    evidence(E) forall curation_evidence(X,E),
    curation_statement(X,Su,R,Ob) forall curation_statement(X,Su,R,Ob)]).


evidence(E) =>
 xml:item(id=E,
          class='org.obd.InstanceNode').

curation_statement(X,Su,R,Ob) =>
 call(make_id(curation_statement(X,Su,R,Ob),ID)),
 xml:item(id=ID,
          class=MT,
          xml:attribute(name=node,value=Su),
          xml:attribute(name=predicate,value=Rel),
          xml:attribute(name=object,value=Ob)) forall user:node_link_metatype(Su,Rel,Ob,MT).
