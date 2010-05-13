:- multifile
	system:term_expansion/2.

% INTERNAL
link(Link,Reif,Node,Pred,Obj,Implied,Comb) <- link(Link,Reif,Node,Pred,Obj,Implied,Comb,_,_). % base 
link(Node,Pred,Obj,Implied)  <- link(_,_,Node,Pred,Obj,Implied,''). 
link_asserted(Node,Pred,Obj) <- link(_,_,Node,Pred,Obj,f,''). % asserted
intersection_link(Node,Pred,Obj)  <- link(_,_,Node,Pred,Obj,_,'I'). 
node(IID,ID,N,T) <- node(IID,ID,N,T,_,_,'f',_,_,_).
%% relax this: ontol_db:parent0(X,R,Y) <- node(XI,X,_,'C'),link_asserted(XI,RI,YI),node(RI,R,_,_),node(YI,Y,_,'C').
%ontol_db:intersection0(X,R,Y) <- node(XI,X,_,'C'),intersection_link(XI,RI,YI),node(RI,R,_,_),node(YI,Y,_,'C').
ontol_db:intersection0(X,R,Y) <- node(XI,X,_,_),intersection_link(XI,RI,YI),node(RI,R,_,_),node(YI,Y,_,_).
node_desc(ID,ScopeIID,TypeIID,Label) <- node(IID,ID,_,_,_,_,_,_,_,_),description(_,IID,ScopeIID,TypeIID,Label).

% METADATA
metadata_db:entity_label(ID,N) <- node(_,ID,N,_),not_null(N).
metadata_db:entity_synonym(ID,N) <- node(IID,ID,_,_),alias(IID,_,_,N).
metadata_db:entity_resource(ID,NS) <- node(_IID,ID,_,_,_,_,_,_,SIID,_),node(SIID,NS,_,_).
metadata_db:entity_xref(ID,X) <- parent0(ID,'oboMetaModel:xref',X). % todo: or node_xref?

% ONTOL
ontol_db:class(ID) <- node(_,ID,_,'C').
ontol_db:class(ID,N) <- node(_,ID,N,'C').
ontol_db:is_anonymous(ID) <- node(_,ID,_,_,t,_,_,_,_,_).

metadata_db:entity_resource(ID,Source) <- node(_,ID,_,_,_,_,_,_,SourceIID,_),node(SourceIID,Source,_,_).

ontol_db:property(ID) <- node(_,ID,_,'R').
ontol_db:property(ID,N) <- node(_,ID,N,'R').
ontol_db:inst(ID) <- node(_,ID,_,'I').
ontol_db:inst(ID,N) <- node(_,ID,N,'I').
ontol_db:entity_uri(ID,URI) <- node(_,ID,_,_,_,_,_,_,_,URI).

ontol_db:subclass(X,Y) <- parent0(X,'OBO_REL:is_a',Y).
ontol_db:restriction(X,R,Y) <- parent0(X,R,Y),not(R='OBO_REL:is_a').
ontol_db:inst_of(X,Y) <- parent0(X,'OBO_REL:instance_of',Y).
ontol_db:subclassT(X,Y) <- node(XI,X,_,_),link(XI,RI,YI,_),node(YI,Y,_,_),node(RI,'OBO_REL:is_a',_,_),\+ (XI=YI).
ontol_db:subclassRT(X,Y) <- node(XI,X,_,_),link(XI,RI,YI,_),node(YI,Y,_,_),node(RI,'OBO_REL:is_a',_,_).
ontol_db:parentT(X,Y) <- node(XI,X,_,_),link(XI,_,YI,_),node(YI,Y,_,_).


ontol_db:inst_rel(X,R,Y) <- parent0(X,R,Y),not(R='OBO_REL:is_a'). % todo: inst only

% todo: check
%:- abolish(ontol_db:genus/2).
ontol_db:genus(X,Y) <- intersection0(X,'OBO_REL:is_a',Y).
%:- abolish(ontol_db:differentium/3).
ontol_db:differentium(X,R,Y) <- intersection0(X,R,Y),not(R='OBO_REL:is_a').

ontol_db:class_intersection_element(X,R=Y) <- intersection0(X,R,Y).
ontol_db:class_intersection_element(X,R,Y) <- intersection0(X,R,Y).


ontol_db:def(X,Label) <- node_desc(X,_,TypeIID,Label),node(TypeIID,definition,_,_).

% todo: should this be moved to a curation_db mapping? as it is now, curation_bridge_to_ontol should be used
%:- abolish(curation_db:curation_statement/4).
curation_db:curation_statement(Cu,S,R,O) <- node(SI,S,_,_),link(_Link,CuI,SI,RI,OI,_,_),node(OI,O,_,_),node(RI,R,_,_),node(CuI,Cu,_,_).

curation_db:curation_statementT(Cu,S,R,InferredO,O) <-
  node(SI,S,_,_),link(_Link,CuI,SI,RI,OI,_,_),link(OI,_,InferredOI,_),node(OI,O,_,_),node(InferredOI,InferredO,_,_),node(RI,R,_,_),node(CuI,Cu,_,_).

%lookup_class(search(S,_),ID):-
%        foo.

:- dynamic rdb_handle/1.
getrdb(Rdb):-
        %nb_getval(rdb,Rdb).
        sql_compiler:sqlschema_connection(obd,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

% (+,+,?,+)
% todo: use functions
sql_store(class,Rdb,ID,IID):-
        (   rdb_query(Rdb,IID,node(IID,ID,_,_))
        ->  sql_update(class,Rdb,ID,IID)
        ;   sql_insert(class,Rdb,ID,IID)),
        forall(restriction(ID,R,To),
               sql_store(Rdb,restriction(ID,R,To))).

sql_store(class,Rdb,ID,IID):-
        (   rdb_query(Rdb,IID,node(IID,ID,_,_))
        ->  sql_update(class,Rdb,ID,IID)
        ;   sql_insert(class,Rdb,ID,IID)).

              
% (+,+,?,+)
sql_insert(class,Rdb,ID,IID):-
        findall(TagVal,sqlmap_col(class,node,ID,TagVal),TagVals),
        rdb_insert(Rdb,
                   node,
                   TagVals,
                   IID).
% (+,+,+,+)
sql_insert(class,Rdb,ID,IID):-
        findall(TagVal,sqlmap_col(class,node,ID,TagVal),TagVals),
        rdb_update(Rdb,
                   node,
                   TagVals,
                   IID).


% todo
sqlmap_col(class,node,ID,uid=ID).
sqlmap_col(class,node,_,metatype='C').
%sqlmap_col(class,tagval,ID,x).
sqlmap_col(_,node,ID,label=Label):- entity_label(ID,Label).
sqlmap_col(_,node,ID,is_obsolete=t):- entity_obsolete(ID,_).
