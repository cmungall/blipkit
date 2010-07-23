:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).

pure_role(ID-N):-
        class(Root,'ChEBI ontology'),
        class(Role,'biological role'),
        findall(SubRoot,(parent(SubRoot,Root),SubRoot\=Role),SubRoots),
        subclassT(ID,Role),
        \+ (subclassT(ID,SubRoot),
            member(SubRoot,SubRoots)),
        class(ID,N).

sub_smiles(C1,C2) :-
	class_smiles(C1,A1),
	class_smiles(C2,A2),
	A1\=A2,
	sub_atom(A1,_,_,_,A2).

guess_has_part_nr(C1,C2) :-
	sub_smiles(C1,C2),
	\+restriction(C1,has_part,C2),
	\+((subclassRT(C1,C1X),
	    restriction(C1X,has_part,C2X),
	    subclassRT(C2X,C2))).


class_smiles(C,X) :-
	entity_synonym_type(C,X,'SMILES').


class_inchi(C,X) :-
	entity_synonym(C,S),atom_concat('InChI=',X,S).

class_inchi_atomlayer(C,X,A) :-
	class_inchi(C,X),
	concat_atom([_,_,A1|_],/,X),
	atom_concat(c,A,A1).

gdr_for_inverse_entail('metabolic process','OBO_REL:has_participant').
gdr_for_inverse_entail('biosynthetic process','OBO_REL:has_output').
gdr_for_inverse_entail('biosynthetic process','OBO_REL:results_in_formation_of').
gdr_for_inverse_entail('catabolic process','OBO_REL:results_in_breakdown_of').
gdr_for_inverse_entail('secretion','OBO_REL:results_in_transport_of').
gdr_for_inverse_entail('transport','OBO_REL:results_in_transport_of').
gdr_for_inverse_entail('binding','UCDHSC:results_in_joining_of').
gdr_for_inverse_entail('catabolic process','UCDHSC:results_in_division_of').

inverse_entail(XA,XB) :-
	gdr_for_inverse_entail(GN,R),
	class(G,GN),
	genus(A,G),
	debug(ie,'testing=~w',[A]),
	setof(XA,differentium(A,R,XA),[XA]),
	subclass(A,B), % pre-reasoned
	A\=B,
	genus(B,G),
	setof(XB,differentium(B,R,XB),[XB]),
	XA\=XB.

inverse_entail_nr(XA,XB) :-
	inverse_entail(XA,XB),
	\+ ((inverse_entail(XA,XZ),
	     inverse_entail(XZ,XB))).

abduce_inference_over(AX,RX,BX,A,B,R) :-
        differentium(A,R,AX),
        id_idspace(AX,'CHEBI'),
        subclass(A,B),
        differentium(B,R,BX),
        id_idspace(BX,'CHEBI'),
        restriction(AX,RX,BX).

neg_abduce_inference_over(AX,RX,BX,A,B,R) :-
        differentium(A,R,AX),
        id_idspace(AX,'CHEBI'),
        restriction(AX,RX,BX),
        propagate_over_rel(RX),
        differentium(B,R,BX),
        id_idspace(BX,'CHEBI'),
        \+ subclassRT(A,B).

propagate_over_rel(is_conjugate_base_of).
propagate_over_rel(has_role).
propagate_over_rel(has_functional_parent).


class_from_bp(N,AN) :-
	atom_concat(AN,' metabolic process',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' transport',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' biosynthesic process',N).
class_from_bp(N,AN) :-
	atom_concat(AN,' catabolic process',N).

goxp_def(MP,Def) :-
	def(MP,D1),
        concat_atom([_|L],'. ',D1),
	%concat_atom(L,'. ',D1),
	%reverse(L,[_,Def,_|_]),
        concat_atom(L,'. ',Def),
        Def\='',
        Def\='. ',
        Def\='.',
	!.

nodef(X) :- \+ def(X,_).
nodef(X) :- def(X,'.').
nodef(X) :- def(X,'x.').

:- dynamic done/1.
chebi_def_from_go :-
	solutions(GO-N,(entity_label_scope(GO,N,Sc),
                        (   Sc=exact;Sc=label),
                        belongs(GO,biological_process),
                        subclassT(GO,'GO:0008152')),
                  GONs),
	member(GO-N,GONs),
	class_from_bp(N,AN),
	entity_label_or_synonym(A,AN),
        nodef(A),
        \+done(A),
        entity_label(A,CN),
	goxp_def(GO,Def),
        assert(done(A)),
        writeln('[Term]'),
        format('id: ~w~n',[A]),
        format('name: ~w~n',[CN]),
        format('def: "~w" [~w]~n',[Def,GO]),
        (   def(A,Def1)
        ->  format('! was: ~w~n',[Def1])
        ;   true),
	nl,
	fail.
uberon_goxp_write :- !.

is_exact(exact).
is_exact(label).

term_ends_with_inexact(A,B,C,D,E,F) :-
        term_ends_with(A,B,C,D,E,F),
        (   \+ is_exact(E)
        ;   \+ is_exact(F)),
        \+ ((   term_ends_with(A,B2,C2,_,E2,F2),
                (   B2\=B
                ;   C2\=C
                ;   E2\=E
                ;   F2\=F),
                is_exact(E2),
                is_exact(F2))).

go_new_via_propagation(X,Y,C,R,E) :-
        genus_label(Tail),
        term_ends_with(X,C,_,Tail,SX1,SX2),
        \+id_idspace(C,'GO'),
        is_exact(SX1),
        is_exact(SX2),
        restriction(C,R,D),
        subclassRT(D,E),
        term_ends_with(Y,E,_,Tail,SY1,SY2),
        is_exact(SY1),
        is_exact(SY2),
        \+ subclassRT(X,Y).

guess_conj_acid(Acid,Conj) :-
        entity_label(Acid,AcidN),
        atom_concat(Prefix,'ic acid',AcidN),
        atom_concat(Prefix,'ate',ConjN),
        entity_label(Conj,ConjN).

go_oxoacid_ref(X,C,Conj,GuessConj) :-
        genus_label(Tail),
        term_ends_with(X,C,_,Tail,SX1,SX2),
        \+id_idspace(C,'GO'),
        is_exact(SX1),
        is_exact(SX2),
        subclassRT(C,'CHEBI:24833'), % oxoacid
        (   restriction(C,is_conjugate_acid_of,Conj)
        ->  GuessConj='-'
        ;   Conj='HAS_NO_CONJUGATE_ACID',
            (   guess_conj_acid(C,GuessConj)
            ->  true
            ;   GuessConj='CANNOT_GUESS_CONJUGATE')).


