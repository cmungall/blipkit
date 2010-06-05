:- module(ontol_sqlmap_go_exposed_ids,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(curation_db)). % separate module?

:- load_schema_defs(bio('sql_schema/schema_go')).

:- multifile
	system:term_expansion/2.

% views
%  term0
%  product0
%  etc
:- [ontol_sqlmap_go_util].


% METADATA
metadata_db:entity_label(ID,N) <- term0(ID,_,N,_).
metadata_db:entity_resource(ID,NS) <- term0(ID,_,_,NS).
metadata_db:entity_synonym(ID,N) <- term_synonym(ID,N,_,_).

is_class_or_property_or_inst(ID) <- term0(ID,_,_,_).

%metadata_db:entity_source(ID,DB) <- dbxrefd(_IID,_Acc,_,DB,_,ID).


% ONTOL
ontol_db:class(ID) <- term0(ID,_,_,_).

ontol_db:parent(X,Y) <- link_n(X,_,Y).

ontol_db:parentT(X,R,Y) <- link_implied(X,R,Y).
ontol_db:parentT(X,Y) <- link_implied(X,_,Y). % todo - nonrefl
ontol_db:parentRT(X,Y) <- link_implied(X,_,Y).

ontol_db:subclass(X,Y) <- parent0(X,R,Y),term0(R,_,is_a,_).
ontol_db:restriction(X,R,Y) <- parent(X,R,Y),term0(R,_,RN,_),not(RN=is_a).

%:- abolish(ontol_db:noparent/1).
ontol_db:noparent(X) <- class(X),not(parent(X,_)).

ontol_db:def(X,Label) <- term_definition(X,Label,_,_,_).

%lookup_class(search(S,_),ID):-
%        foo.

% TODO: split this into curation package?
curation_db:curation(X) <- association(X,_,_,_,_,_,_).
curation_db:curation_statement(X,S,has_role,O) <- association(X,O,S,0,_,_,_).
curation_db:negative_curation_statement(X,S,has_role,O) <- association(X,O,S,1,_,_,_).
curation_db:curation_evidence(C,E) <- evidence(E,_,C,_,_).
curation_db:evidence_type(E,T) <- evidence(E,T,_,_,_).
curation_db:evidence_with(E,X) <- evidence_dbxref(E,XI),dbxref0(XI,X).
curation_db:evidence_source(E,S) <- evidence(E,_,_,SI,_),dbxref0(SI,S).

