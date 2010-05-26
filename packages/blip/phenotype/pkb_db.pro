:- module(pkb_db,
          [
           organism/1,
           organism_label/2,
           organism_type/2,
           organism_species/2,
           organism_role/2,
           organism_description/2,
           organism_phenotype/2,
           organism_phenotype_inst/2,
           organism_variant_gene/2,
	   organism_disease_variant_gene/3,
           phenotype_class/2,

           phenotype_cdef/2,
           phenotype_quad/2,
           phenotype_quad_cdef/3,
	   phenotype_differentium/3,
           organism_phenotype_quad/3,
           organism_phenotype_quad/2,
	   phenotype_property_value/3,
	   class_quad_aspect/3,
           phenotype_description/2,
           organism_inferred_type/2,
           organism_role_disease/3,
           organism_disease/2,
           inferred_organism_role_disease/3,
	   inferred_organism_role_disease_species/5,
           species/1,
           species_label/2,
           disease/1,
           disease_label/2,
           disease_description/2,
           disease_gene_variant/3,
           disease_gene/2,

           disease_phenotype/2,

	   entity_phenotype/2
           ]).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_basic_reasoner'),
              [entailed/1       % for organism_inferred_type/2
              ]).
:- use_module(bio(bioprolog_util),[solutions/3]).

:- use_module(phenotype_db).
:- use_module(bio(dbmeta)).

%% organism(?Org)
:- extensional(organism/1).

%% organism_label(?Org,?Label)
:- extensional(organism_label/2).

%% organism_type(?Org,?Type)
:- extensional(organism_type/2).

%% organism_role(?Org,?Role)
% TODO - make intensional, derive from organism_role_disease/3
:- extensional(organism_role/2).

%% organism_role_disease(?Org,?Role,?Disease)
:- extensional(organism_role_disease/3).

organism_disease(O,D) :- organism_role_disease(O,_,D).


%% organism_role_disease(?Org,?Role,?Disease,?Species)
:- extensional(organism_role_disease_species/4).

%% organism_species(?Org,?Species)
:- extensional(organism_species/2).

%% organism_variant_gene(?Org,?Gene)
% true if Org has some variant version of Gene.
:- extensional(organism_variant_gene/2).

%% organism_disease_variant_gene(?Org,?D,?Gene)
% inferred from organism_role_disease/3 and disease_variant_gene
organism_disease_variant_gene(O,D,G) :-
	organism_role_disease(O,_,D),
	disease_gene_variant(D,G,_).

%% organism_description(?Org,?Desc)
:- extensional(organism_description/2).

%% organism_phenotype_inst(?Org,?PhenoInst)
:- extensional(organism_phenotype_inst/2).

%% organism_phenotype(?Org,?Phenotype)
% Phenotype = (E,Q,D,W)
:- extensional(organism_phenotype/2).

%:- extensional(phenotype_class/2).
phenotype_class(P,P) :-
        class(P).

%% species(?Sp)
:- extensional(species/1).

%% species_label(?Sp,?Label)
:- extensional(species_label/2).

%% disease(?D)
:- extensional(disease/1).

%% disease_label(?D,?Label)
:- extensional(disease_label/2).

%% disease_description(?D,?Desc)
:- extensional(disease_description/2).

%% disease_gene_variant(?D,?G,?V)
:- extensional(disease_gene_variant/3).

%% disease_gene(?D,?G)
% true if G has some variant V that is implicated in D
disease_gene(D,G) :- disease_gene_variant(D,G,_).


%% inferred_organism_role_disease(?Model,?Role,?Disease)
inferred_organism_role_disease(Model,model,D) :-
        solutions(MaxIC-Model,
                  (   organism_role_disease(Patient,patient,D),
                      method_feature_pair_phenosim(postcomposed,Patient,Model,Results),
		      member(max_ic(MaxIC),Results),
                      \+ organism_role_disease(Model,patient,_)
                  ),
                  L),           % TODO
        member(MaxIC-Model,L),
        \+ ((member(MaxIC2-_,L),
             MaxIC2 > MaxIC)).

%% inferred_organism_role_disease_species(?Org,?Role,?Disease,?Sp,IsReciprocal)
% Org inferred to play role Role in Disease, and Org is of species Sp
inferred_organism_role_disease_species(Org,model,D,S,IsReciprocal) :-
        species(S), % force S to be ground
	% find all score-organism pairs for a given species
        solutions(Sc-Org,
		  organism_disease_species_score(Org,D,S,Sc),
                  L),           % TODO
        member(Sc-Org,L),
        \+ ((member(Sc2-_,L),
             Sc2 > Sc)),
	% must be reciprocal; i.e. no other disease D2 that for which
	% Org-D2 is a better match
	(   organism_disease_species_score(Org,D2,S,Sc2),
	    D2\=D,
	    Sc2 > Sc
	->  IsReciprocal=false
	;   IsReciprocal=true).



organism_disease_species_score(Org,D,S,Sc) :-
	organism_role_disease(Patient,patient,D),
	organism_pair_combined_score(Patient,Org,Sc),
	Sc > 2.0,		% arbitrary for now...
	organism_species(Org,S),
	\+ organism_role_disease(Org,patient,_).

%% organism_pair_combined_score(?O1,?O2,?S)
% ad-hoc combination of similarity scores between two organisms, based on method_feature_pair_phenosim/6
organism_pair_combined_score(O1,O2,S) :-
        method_feature_pair_phenosim(postcomposed,O1,O2,Results),
	member(max_ic(MaxIC),Results),
	member(avg_ic_bestmatches(AvgIC),Results),
        S is MaxIC + AvgIC/5.

%% organism_inferred_type(?Org,?Type)
% combination of organism_type/2 and inferred (reflexive) subClassOf/2 using entailed/1
organism_inferred_type(Org,Type) :-
        organism_type(Org,Type1),
        entailed(subClassOfReflexive(Type1,Type)).

%% disease_phenotype(?D,?P)
% true if disease is associated with phenotype, by virtue of some organism with the disease carrying the phenotype
disease_phenotype(D,P) :-
        organism_role_disease(O,_,D),
        organism_phenotype(O,P).

%% entity_phenotype(?E,?P)
% union of organism_phenotype/2 and disease_phenotype/2.
% TODO - this should replace feature_phenotype/2 in phenotype_db
entity_phenotype(E,P) :- organism_phenotype(E,P).
entity_phenotype(E,P) :- disease_phenotype(E,P).

%% phenotype_property_value(?P,?Prop,?V)
phenotype_property_value(P,Prop,V) :-
	phenotype_quad(P,Q),
	class_quad_aspect(V,Q,Prop).


%% phenotype_quad(?P,?Quad)
% Quad = (E,Q,D,W)
:- extensional(phenotype_quad/2).

%% organism_phenotype_quad(?O,?P,?PQ)
% joins organism_phenotype/2 and phenotype_quad/2 to directly access E-Q-D-W quad from organism
organism_phenotype_quad(O,P,PQ) :- organism_phenotype(O,P),phenotype_quad(P,PQ).
organism_phenotype_quad(O,PQ) :- organism_phenotype_quad(O,_,PQ).

class_quad_aspect(C,(C,_,_,_),e).
class_quad_aspect(C,(_,C,_,_),q).
class_quad_aspect(C,(_,_,C,_),d).
class_quad_aspect(C,(_,_,_,C),w).


%% phenotype_description(?P,?Description)
:- extensional(phenotype_description/2).

phenotype_cdef(P,D) :-
        phenotype_quad_cdef(P,_,D).

phenotype_quad_cdef(P,PQ,cdef(Q,DL)) :-
        phenotype_quad(P,PQ),
        PQ=(_,Q1,_,_),
        uri_to_obo(Q1,Q),
        findall(R=X,phenotype_differentium(PQ,R,X),DL).

phenotype_differentium((E1,_,_,_),inheres_in,E) :-         uri_to_obo(E1,E).
phenotype_differentium((_,_,_,W1),inheres_in_part_of,W) :- uri_to_obo(W1,W).
phenotype_differentium((_,_,D1,_),towards,D) :-            uri_to_obo(D1,D).

uri_to_obo(U,ID) :-
        rdf_global_id(NS:Obj,U),
        !,
        concat_atom([NS,Obj],':',ID).
uri_to_obo(U,U) :-
        U\='-',
        U\=''.

:- multifile dbmeta:fact_chain_hook/2.
dbmeta:fact_chain_hook(organism(O),
	   [organism_phenotype(O,_),
	    organism_phenotype_inst(O,_),
	    organism_species(O,_),
	    organism_label(O,_),
	    organism_type(O,_),
	    organism_species(O,_),
	    organism_role(O,_),
	    organism_role_disease(O,_,_),
	    organism_description(O,_),
	    organism_phenotype(O,_),
	    organism_phenotype_inst(O,_),
	    organism_variant_gene(O,_),
	    organism_variant_gene(O,_)]).

dbmeta:fact_chain_hook(organism_phenotype(_,S),
		       [phenotype_quad(S,_),
			phenotype_description(S,_)
		       ]).

dbmeta:fact_chain_hook(organism_species(_,S),
		       [species(S)]).
dbmeta:fact_chain_hook(species(S),[species_label(S,_)]).

dbmeta:fact_chain_hook(organism_role_disease(_,_,S),
		       [disease(S)]).
dbmeta:fact_chain_hook(disease(S),
		       [disease_label(S,_),
			disease_description(S,_),
			disease_gene_variant(S,_,_)
		       ]).

dbmeta:fact_chain_hook(organism_variant_gene(_,G),
		       [genome_db:gene(G)]).


	

%translate_pkb_to_obo_ids:-
        

/** <module> model for NIF neurodegenerative diseases phenotype knowledgebase

  ---+ Synopsis

==
:- use_module(bio(pkb_db)).

% 
demo:-
  nl.
  

==

---+ Details

---++ Requirements

* Thea2 - uses subClassOf/2 and entailed/1


*/