:- use_module(bio(bioprolog_util)).
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

	
	



            

        
