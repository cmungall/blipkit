:- module(ontol_sqlmap_obd_optimized,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(curation_db),[]). % move?
:- use_module(bio(ontol_sqlmap_obd)).

% extends ontol_sqlmap_obd with optimized versions of ontol_db predicates
% note: will NOT work in concert with ontol_db over in-memory predicates

:- abolish(ontol_db:parent/2).
ontol_db:parent(X,Y) <- node(XI,X,_,'C'),link_asserted(XI,_,YI),node(YI,Y,_,'C').

:- abolish(ontol_db:parent/3). % map to internal
ontol_db:parent(X,subclass,Y):- ontol_db:parent0(X,'OBO_REL:is_a',Y).
ontol_db:parent(X,R,Y):- ontol_db:parent0(X,R,Y),not(R='OBO_REL:is_a').

:- abolish(ontol_db:parentT/3). % TODO is_a/subclass
ontol_db:parentT(X,R,Y) <- node(XI,X,_,'C'),link(XI,RI,YI),node(RI,R,_,'C'),node(YI,Y,_,'C').

:- abolish(ontol_db:subclassT/2).
ontol_db:subclassT(X,Y) <- node(XI,X,_,'C'),link(XI,RI,YI,_),node(RI,'OBO_REL:is_a',_,_),node(YI,Y,_,'C').
:- abolish(ontol_db:noparent/1).
ontol_db:noparent(X) <- class(X),not(parent0(X,_)).
%ontol_db:noparent(X,O) <- class(X),entity_resource(X,O),not(parent0(X,_)).


%:- abolish(ontol_db:subclass/2).

