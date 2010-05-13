:- module(biopax1_bridge_from_biopax2,[]).

:-use_module(biopax1_db,[]).
:-use_module(bio(owl_util)).
:-use_module(semweb(rdf_db)).
:-use_module(semweb(rdfs)).

:- initialization(rdf_load('http://www.biopax.org/release/biopax-level2.owl'),
		  after_load).


:-op(300, xfy, xref).
:-op(300, xfy, participants).
:-op(300, xfy, sequence_interval_end).
:-op(300, xfy, sequence_interval_begin).
:-op(300, xfy, feature_type).
:-op(300, xfy, step_interactions).
:-op(300, xfy, organism).
:-op(300, xfy, controlled).
:-op(300, xfy, controller).
:-op(300, xfy, participant).
:-op(300, xfy, experimental_form_type).
:-op(300, xfy, cellular_location).
:-op(300, xfy, physical_entity).
:-op(300, xfy, confidence).
:-op(300, xfy, evidence_code).
:-op(300, xfy, experimental_form).
:-op(300, xfy, tissue).
:-op(300, xfy, celltype).
:-op(300, xfy, taxon_xref).
:-op(300, xfy, pathway_components).
:-op(300, xfy, evidence).
:-op(300, xfy, right).
:-op(300, xfy, interaction_type).
:-op(300, xfy, delta_g).
:-op(300, xfy, structure).
:-op(300, xfy, sequence_feature_list).
:-op(300, xfy, left).
:-op(300, xfy, components).
:-op(300, xfy, cofactor).
:-op(300, xfy, next_step).
:-op(300, xfy, feature_location).
:-op(300, xfy, keq).
:-op(300, xfy, data_source).
:-op(300, xfy, db).
:-op(300, xfy, id).
:-op(300, xfy, relationship_type).
:-op(300, xfy, molecular_weight).
:-op(300, xfy, chemical_formula).
:-op(300, xfy, sequence_position).
:-op(300, xfy, position_status).
:-op(300, xfy, year).
:-op(300, xfy, title).
:-op(300, xfy, biopax_name).
:-op(300, xfy, short_name).
:-op(300, xfy, confidence_value).
:-op(300, xfy, sequence).
:-op(300, xfy, db_version).
:-op(300, xfy, id_version).
:-op(300, xfy, control_type).
:-op(300, xfy, ph).
:-op(300, xfy, temperature).
:-op(300, xfy, ionic_strength).
:-op(300, xfy, k_prime).
:-op(300, xfy, pmg).
:-op(300, xfy, stoichiometric_coefficient).
:-op(300, xfy, delta_g_prime_o).
:-op(300, xfy, direction).
:-op(300, xfy, spontaneous).
:-op(300, xfy, structure_data).
:-op(300, xfy, structure_format).
:-op(300, xfy, authors).
:-op(300, xfy, availability).
:-op(300, xfy, synonyms).
:-op(300, xfy, delta_s).
:-op(300, xfy, source).
:-op(300, xfy, delta_h).
:-op(300, xfy, biopax_comment).
:-op(300, xfy, url).
:-op(300, xfy, term).
:-op(300, xfy, ec_number).

biopax1_db:dataSource(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#dataSource').
biopax1_db:openControlledVocabulary(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#openControlledVocabulary').
biopax1_db:xref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#xref').
biopax1_db:bioSource(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#bioSource').
biopax1_db:externalReferenceUtilityClass(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#externalReferenceUtilityClass').
biopax1_db:publicationXref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#publicationXref').
biopax1_db:unificationXref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#unificationXref').
biopax1_db:sequenceParticipant(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#sequenceParticipant').
biopax1_db:physicalEntityParticipant(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#physicalEntityParticipant').
biopax1_db:transportWithBiochemicalReaction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#transportWithBiochemicalReaction').
biopax1_db:biochemicalReaction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#biochemicalReaction').
biopax1_db:transport(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#transport').
biopax1_db:complexAssembly(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#complexAssembly').
biopax1_db:conversion(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#conversion').
biopax1_db:physicalEntity(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#physicalEntity').
biopax1_db:interaction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#interaction').
biopax1_db:entity(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#entity').
biopax1_db:pathway(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#pathway').
biopax1_db:relationshipXref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#relationshipXref').
biopax1_db:physicalInteraction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#physicalInteraction').
biopax1_db:smallMolecule(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#smallMolecule').
biopax1_db:protein(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#protein').
biopax1_db:rna(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#rna').
biopax1_db:dna(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#dna').
biopax1_db:complex(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#complex').
biopax1_db:sequenceLocation(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#sequenceLocation').
biopax1_db:kPrime(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#kPrime').
biopax1_db:deltaGprimeO(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#deltaGprimeO').
biopax1_db:chemicalStructure(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#chemicalStructure').
biopax1_db:experimentalForm(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#experimentalForm').
biopax1_db:confidence(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#confidence').
biopax1_db:evidence(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#evidence').
biopax1_db:utilityClass(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#utilityClass').
biopax1_db:pathwayStep(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#pathwayStep').
biopax1_db:sequenceFeature(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#sequenceFeature').
biopax1_db:sequenceInterval(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#sequenceInterval').
biopax1_db:sequenceSite(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#sequenceSite').
biopax1_db:modulation(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#modulation').
biopax1_db:catalysis(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#catalysis').
biopax1_db:control(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level2.owl#control').
biopax1_db:xref(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#XREF', G6584).
biopax1_db:participants(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#PARTICIPANTS', G6584).
biopax1_db:sequence_interval_end(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-INTERVAL-END', G6584).
biopax1_db:sequence_interval_begin(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-INTERVAL-BEGIN', G6584).
biopax1_db:feature_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#FEATURE-TYPE', G6584).
biopax1_db:step_interactions(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#STEP-INTERACTIONS', G6584).
biopax1_db:organism(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#ORGANISM', G6584).
biopax1_db:controlled(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CONTROLLED', G6584).
biopax1_db:controller(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CONTROLLER', G6584).
biopax1_db:participant(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#PARTICIPANT', G6584).
biopax1_db:experimental_form_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#EXPERIMENTAL-FORM-TYPE', G6584).
biopax1_db:cellular_location(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CELLULAR-LOCATION', G6584).
biopax1_db:physical_entity(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#PHYSICAL-ENTITY', G6584).
biopax1_db:confidence(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CONFIDENCE', G6584).
biopax1_db:evidence_code(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#EVIDENCE-CODE', G6584).
biopax1_db:experimental_form(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#EXPERIMENTAL-FORM', G6584).
biopax1_db:tissue(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#TISSUE', G6584).
biopax1_db:celltype(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CELLTYPE', G6584).
biopax1_db:taxon_xref(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#TAXON-XREF', G6584).
biopax1_db:pathway_components(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#PATHWAY-COMPONENTS', G6584).
biopax1_db:evidence(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#EVIDENCE', G6584).
biopax1_db:right(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#RIGHT', G6584).
biopax1_db:interaction_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#INTERACTION-TYPE', G6584).
biopax1_db:delta_g(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DELTA-G', G6584).
biopax1_db:structure(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#STRUCTURE', G6584).
biopax1_db:sequence_feature_list(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-FEATURE-LIST', G6584).
biopax1_db:left(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#LEFT', G6584).
biopax1_db:components(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#COMPONENTS', G6584).
biopax1_db:cofactor(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#COFACTOR', G6584).
biopax1_db:next_step(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#NEXT-STEP', G6584).
biopax1_db:feature_location(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#FEATURE-LOCATION', G6584).
biopax1_db:keq(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#KEQ', G6584).
biopax1_db:data_source(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DATA-SOURCE', G6584).
biopax1_db:db(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DB', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:id(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#ID', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:relationship_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#RELATIONSHIP-TYPE', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:molecular_weight(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#MOLECULAR-WEIGHT', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:chemical_formula(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CHEMICAL-FORMULA', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:sequence_position(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-POSITION', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:position_status(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#POSITION-STATUS', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:year(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#YEAR', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:title(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#TITLE', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:biopax_name(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#NAME', G6644), rdf_literal_to_native(G6644, G6584).
biopax1_db:short_name(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SHORT-NAME', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:confidence_value(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CONFIDENCE-VALUE', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:sequence(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SEQUENCE', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:db_version(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DB-VERSION', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:id_version(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#ID-VERSION', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:control_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#CONTROL-TYPE', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:ph(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#PH', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:temperature(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#TEMPERATURE', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:ionic_strength(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#IONIC-STRENGTH', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:k_prime(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#K-PRIME', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:pmg(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#PMG', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:stoichiometric_coefficient(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#STOICHIOMETRIC-COEFFICIENT', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:delta_g_prime_o(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DELTA-G-PRIME-O', G6627), rdf_literal_to_native(G6627, G6584).
biopax1_db:direction(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DIRECTION', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:spontaneous(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SPONTANEOUS', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:structure_data(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#STRUCTURE-DATA', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:structure_format(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#STRUCTURE-FORMAT', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:authors(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#AUTHORS', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:availability(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#AVAILABILITY', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:synonyms(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SYNONYMS', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:delta_s(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DELTA-S', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:source(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#SOURCE', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:delta_h(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#DELTA-H', G6621), rdf_literal_to_native(G6621, G6584).
biopax1_db:biopax_comment(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#COMMENT', G6644), rdf_literal_to_native(G6644, G6584).
biopax1_db:url(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#URL', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:term(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#TERM', G6618), rdf_literal_to_native(G6618, G6584).
biopax1_db:ec_number(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level2.owl#EC-NUMBER', G6621), rdf_literal_to_native(G6621, G6584).



/** <module> 

  ---+ Synopsis

==
:- use_module(bio(biopax2_bridge_from_biopax1)).

% 
demo:-
  nl.
  

==

---+ Details

TODO - generares a lot of warnings

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
