:- module(biopax2_bridge_from_biopax1,[]).

:-use_module(biopax2_db,[]).
:-use_module(bio(owl_util)).
:-use_module(semweb(rdf_db)).
:-use_module(semweb(rdfs)).

:- initialization(rdf_load('http://www.biopax.org/release/biopax-level1.owl'),
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
biopax2_db:dataSource(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#dataSource').
biopax2_db:openControlledVocabulary(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#openControlledVocabulary').
biopax2_db:xref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#xref').
biopax2_db:bioSource(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#bioSource').
biopax2_db:externalReferenceUtilityClass(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#externalReferenceUtilityClass').
biopax2_db:publicationXref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#publicationXref').
biopax2_db:unificationXref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#unificationXref').
biopax2_db:sequenceParticipant(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#sequenceParticipant').
biopax2_db:physicalEntityParticipant(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#physicalEntityParticipant').
biopax2_db:transportWithBiochemicalReaction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#transportWithBiochemicalReaction').
biopax2_db:biochemicalReaction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#biochemicalReaction').
biopax2_db:transport(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#transport').
biopax2_db:complexAssembly(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#complexAssembly').
biopax2_db:conversion(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#conversion').
biopax2_db:physicalEntity(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#physicalEntity').
biopax2_db:interaction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#interaction').
biopax2_db:entity(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#entity').
biopax2_db:pathway(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#pathway').
biopax2_db:relationshipXref(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#relationshipXref').
biopax2_db:physicalInteraction(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#physicalInteraction').
biopax2_db:smallMolecule(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#smallMolecule').
biopax2_db:protein(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#protein').
biopax2_db:rna(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#rna').
biopax2_db:dna(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#dna').
biopax2_db:complex(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#complex').
biopax2_db:sequenceLocation(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#sequenceLocation').
biopax2_db:kPrime(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#kPrime').
biopax2_db:deltaGprimeO(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#deltaGprimeO').
biopax2_db:chemicalStructure(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#chemicalStructure').
biopax2_db:experimentalForm(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#experimentalForm').
biopax2_db:confidence(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#confidence').
biopax2_db:evidence(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#evidence').
biopax2_db:utilityClass(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#utilityClass').
biopax2_db:pathwayStep(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#pathwayStep').
biopax2_db:sequenceFeature(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#sequenceFeature').
biopax2_db:sequenceInterval(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#sequenceInterval').
biopax2_db:sequenceSite(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#sequenceSite').
biopax2_db:modulation(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#modulation').
biopax2_db:catalysis(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#catalysis').
biopax2_db:control(G6581):-rdfs_individual_of(G6581, 'http://www.biopax.org/release/biopax-level1.owl#control').
biopax2_db:xref(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#XREF', G6584).
biopax2_db:participants(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#PARTICIPANTS', G6584).
biopax2_db:sequence_interval_end(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SEQUENCE-INTERVAL-END', G6584).
biopax2_db:sequence_interval_begin(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SEQUENCE-INTERVAL-BEGIN', G6584).
biopax2_db:feature_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#FEATURE-TYPE', G6584).
biopax2_db:step_interactions(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#STEP-INTERACTIONS', G6584).
biopax2_db:organism(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#ORGANISM', G6584).
biopax2_db:controlled(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CONTROLLED', G6584).
biopax2_db:controller(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CONTROLLER', G6584).
biopax2_db:participant(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#PARTICIPANT', G6584).
biopax2_db:experimental_form_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#EXPERIMENTAL-FORM-TYPE', G6584).
biopax2_db:cellular_location(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CELLULAR-LOCATION', G6584).
biopax2_db:physical_entity(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#PHYSICAL-ENTITY', G6584).
biopax2_db:confidence(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CONFIDENCE', G6584).
biopax2_db:evidence_code(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#EVIDENCE-CODE', G6584).
biopax2_db:experimental_form(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#EXPERIMENTAL-FORM', G6584).
biopax2_db:tissue(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#TISSUE', G6584).
biopax2_db:celltype(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CELLTYPE', G6584).
biopax2_db:taxon_xref(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#TAXON-XREF', G6584).
biopax2_db:pathway_components(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#PATHWAY-COMPONENTS', G6584).
biopax2_db:evidence(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#EVIDENCE', G6584).
biopax2_db:right(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#RIGHT', G6584).
biopax2_db:interaction_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#INTERACTION-TYPE', G6584).
biopax2_db:delta_g(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DELTA-G', G6584).
biopax2_db:structure(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#STRUCTURE', G6584).
biopax2_db:sequence_feature_list(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SEQUENCE-FEATURE-LIST', G6584).
biopax2_db:left(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#LEFT', G6584).
biopax2_db:components(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#COMPONENTS', G6584).
biopax2_db:cofactor(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#COFACTOR', G6584).
biopax2_db:next_step(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#NEXT-STEP', G6584).
biopax2_db:feature_location(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#FEATURE-LOCATION', G6584).
biopax2_db:keq(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#KEQ', G6584).
biopax2_db:data_source(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DATA-SOURCE', G6584).
biopax2_db:db(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DB', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:id(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#ID', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:relationship_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#RELATIONSHIP-TYPE', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:molecular_weight(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#MOLECULAR-WEIGHT', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:chemical_formula(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CHEMICAL-FORMULA', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:sequence_position(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SEQUENCE-POSITION', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:position_status(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#POSITION-STATUS', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:year(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#YEAR', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:title(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#TITLE', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:biopax_name(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#NAME', G6644), rdf_literal_to_native(G6644, G6584).
biopax2_db:short_name(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SHORT-NAME', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:confidence_value(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CONFIDENCE-VALUE', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:sequence(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SEQUENCE', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:db_version(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DB-VERSION', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:id_version(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#ID-VERSION', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:control_type(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#CONTROL-TYPE', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:ph(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#PH', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:temperature(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#TEMPERATURE', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:ionic_strength(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#IONIC-STRENGTH', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:k_prime(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#K-PRIME', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:pmg(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#PMG', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:stoichiometric_coefficient(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#STOICHIOMETRIC-COEFFICIENT', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:delta_g_prime_o(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DELTA-G-PRIME-O', G6627), rdf_literal_to_native(G6627, G6584).
biopax2_db:direction(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DIRECTION', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:spontaneous(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SPONTANEOUS', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:structure_data(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#STRUCTURE-DATA', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:structure_format(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#STRUCTURE-FORMAT', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:authors(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#AUTHORS', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:availability(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#AVAILABILITY', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:synonyms(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SYNONYMS', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:delta_s(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DELTA-S', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:source(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#SOURCE', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:delta_h(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#DELTA-H', G6621), rdf_literal_to_native(G6621, G6584).
biopax2_db:biopax_comment(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#COMMENT', G6644), rdf_literal_to_native(G6644, G6584).
biopax2_db:url(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#URL', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:term(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#TERM', G6618), rdf_literal_to_native(G6618, G6584).
biopax2_db:ec_number(G6581, G6584):-rdf_has(G6581, 'http://www.biopax.org/release/biopax-level1.owl#EC-NUMBER', G6621), rdf_literal_to_native(G6621, G6584).


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
