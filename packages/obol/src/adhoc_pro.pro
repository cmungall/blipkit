
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(bio(tabling)).
:- use_module(bio(io)).
:- use_module(library(porter_stem),[]).

class_from_protein(N,AN) :-	% 
	atom_concat(AN,' secretion',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' production',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' transport',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' binding',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' nuclear translocation',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' biosynthetic process',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' catabolic process',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' metabolic process',N).
class_from_protein(N,AN) :-	% 
	atom_concat(AN,' signal transduction',N).

%% use BP and BP-XP
newpro_write(BP,AN) :-
	setof(BP-N,(class(BP,N),id_idspace(BP,'GO')),BPNs),
	member(BP-N,BPNs),
	subclassRT(BP,X),
	differentium(X,_,PRO),
	id_idspace(PRO,'PRO'),
	debug(newpro,'potential: ~w (under ~w/~w)',[BP,X,PRO]),
	\+ genus(BP,_),		% undefined
	debug(newpro,'pc: ~w ~w',[BP,N]),
	class_from_protein(N,AN).

newpro_write_defs :-
	setof(BP-N,(class(BP,N),id_idspace(BP,'BP')),BPNs),
	member(BP-N,BPNs),
	atom_concat('abnormal ',N1,N),
	atom_concat(AN,' morphology',N1),
	entity_label_or_synonym(A,AN),
	belongs(A,uberon),
	\+ def(A,_),
	mpxp_def(BP,Def),
	Def\='.',
        writeln('[Term]'),
        format('id: ~w~n',[A]),
        format('def: "~w" [~w]~n',[Def,BP]),
	nl,
	fail.
newpro_write_defs.


mpxp_def(BP,Def) :-
	def(BP,D1),
	concat_atom([_,Def],'anomaly of ',D1),
	!.
mpxp_def(BP,Def) :-
	def(BP,D1),
	concat_atom([_,Def],'anomaly in ',D1),
	!.
mpxp_def(BP,Def) :-
	def(BP,D1),
	concat_atom([_,Def],'location of ',D1),
	!.
mpxp_def(BP,Def) :-
	def(BP,D1),
	concat_atom([_,Def],'presence of ',D1),
	!.
mpxp_def(BP,Def) :-
	def(BP,D1),
	concat_atom([_,Def],'structure of ',D1),
	!.
mpxp_def(_,'.').

goxp_def(BP,Def) :-
	def(BP,D1),
	concat_atom(L,'.',D1),
	reverse(L,[_,Def,_|_]),
	!.
goxp_def(_,'.').


write_mpxp_parents(A) :-
	subclass(A,B),
	differentium(B,_,Y),
	entity_xref(X,Y),
	belongs(X,uberon),
	(   class(X,XN)
	->  true
	;   XN = '?'),
	format('relationship: part_of ~w ! ~w~n',[X,XN]),
	fail.
write_mpxp_parents(_) :- !.

write_goxp_parents(A) :-
	parent(A,B),
	differentium(B,_,X),
	belongs(X,uberon),
	(   class(X,XN)
	->  true
	;   XN = '?'),
	format('relationship: part_of ~w ! ~w~n',[X,XN]),
	fail.
write_goxp_parents(_) :- !.


write_xrefs_for_name(N) :-
	entity_label_or_synonym(X,N),
	class(X,XN),
	format('xref: ~w ! ~w~n',[X,XN]),
	fail.
write_xrefs_for_name(_).
