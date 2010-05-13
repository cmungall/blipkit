:-module(biopax3_bridge_from_rdf, []).

:-use_module(bio(owl_util)).
:-use_module(semweb(rdf_db)).
:-use_module(semweb(rdfs)).

:- initialization(rdf_load('http://www.biopax.org/release/biopax-level3.owl'),
		  after_load).

biopax3_db:rna(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Rna').
biopax3_db:protein(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Protein').
biopax3_db:dna(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Dna').
biopax3_db:physicalEntity(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PhysicalEntity').
biopax3_db:rnaReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RnaReference').
biopax3_db:publicationXref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PublicationXref').
biopax3_db:xref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Xref').
biopax3_db:conversion(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Conversion').
biopax3_db:interaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Interaction').
biopax3_db:geneticInteraction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#GeneticInteraction').
biopax3_db:catalysis(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Catalysis').
biopax3_db:control(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Control').
biopax3_db:complex(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Complex').
biopax3_db:rnaRegionReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RnaRegionReference').
biopax3_db:entityReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EntityReference').
biopax3_db:smallMoleculeReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SmallMoleculeReference').
biopax3_db:cellularLocationVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#CellularLocationVocabulary').
biopax3_db:controlledVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ControlledVocabulary').
biopax3_db:biochemicalPathwayStep(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalPathwayStep').
biopax3_db:pathwayStep(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PathwayStep').
biopax3_db:molecularInteraction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#MolecularInteraction').
biopax3_db:dnaRegionReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DnaRegionReference').
biopax3_db:relationshipTypeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RelationshipTypeVocabulary').
biopax3_db:fragmentFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#FragmentFeature').
biopax3_db:entityFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EntityFeature').
biopax3_db:bioSource(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BioSource').
biopax3_db:utilityClass(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#UtilityClass').
biopax3_db:complexAssembly(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ComplexAssembly').
biopax3_db:gene(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Gene').
biopax3_db:templateReaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TemplateReaction').
biopax3_db:degradation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Degradation').
biopax3_db:sequenceModificationVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceModificationVocabulary').
biopax3_db:dnaRegion(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DnaRegion').
biopax3_db:provenance(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Provenance').
biopax3_db:unificationXref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#UnificationXref').
biopax3_db:experimentalFormVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ExperimentalFormVocabulary').
biopax3_db:tissueVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TissueVocabulary').
biopax3_db:modulation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Modulation').
biopax3_db:evidence(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Evidence').
biopax3_db:cellVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#CellVocabulary').
biopax3_db:bindingFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BindingFeature').
biopax3_db:deltaG(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DeltaG').
biopax3_db:sequenceLocation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceLocation').
biopax3_db:smallMolecule(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SmallMolecule').
biopax3_db:transport(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Transport').
biopax3_db:stoichiometry(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Stoichiometry').
biopax3_db:score(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Score').
biopax3_db:sequenceSite(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceSite').
biopax3_db:evidenceCodeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EvidenceCodeVocabulary').
biopax3_db:pathway(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Pathway').
biopax3_db:entity(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Entity').
biopax3_db:chemicalStructure(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ChemicalStructure').
biopax3_db:proteinReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ProteinReference').
biopax3_db:kPrime(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#KPrime').
biopax3_db:biochemicalReaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction').
biopax3_db:dnaReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DnaReference').
biopax3_db:covalentBindingFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#CovalentBindingFeature').
biopax3_db:modificationFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ModificationFeature').
biopax3_db:rnaRegion(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RnaRegion').
biopax3_db:experimentalForm(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ExperimentalForm').
biopax3_db:transportWithBiochemicalReaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction').
biopax3_db:entityReferenceTypeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EntityReferenceTypeVocabulary').
biopax3_db:phenotypeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PhenotypeVocabulary').
biopax3_db:relationshipXref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RelationshipXref').
biopax3_db:sequenceRegionVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceRegionVocabulary').
biopax3_db:templateReactionRegulation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TemplateReactionRegulation').
biopax3_db:interactionVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#InteractionVocabulary').
biopax3_db:sequenceInterval(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceInterval').
biopax3_db:participant(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#participant', B).
biopax3_db:controller(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#controller', B).
biopax3_db:stepProcess(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stepProcess', B).
biopax3_db:interactionType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#interactionType', B).
biopax3_db:xref(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#xref', B).
biopax3_db:confidence(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#confidence', B).
biopax3_db:evidenceCode(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#evidenceCode', B).
biopax3_db:experimentalForm(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalForm', B).
biopax3_db:notFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#notFeature', B).
biopax3_db:sequenceFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#feature', B). % renamed
biopax3_db:experimentalFormDescription(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalFormDescription', B).
biopax3_db:componentStoichiometry(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#componentStoichiometry', B).
biopax3_db:featureLocation(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#featureLocation', B).
biopax3_db:dataSource(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#dataSource', B).
biopax3_db:pathwayComponent(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#pathwayComponent', B).
biopax3_db:left(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#left', B).
biopax3_db:pathwayOrder(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#pathwayOrder', B).
biopax3_db:product(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#product', B).
biopax3_db:kEQ(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#kEQ', B).
biopax3_db:relationshipType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#relationshipType', B).
biopax3_db:right(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#right', B).
biopax3_db:deltaG(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaG', B).
biopax3_db:interactionScore(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#interactionScore', B).
biopax3_db:experimentalFormEntity(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalFormEntity', B).
biopax3_db:experimentalFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalFeature', B).
biopax3_db:nextStep(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#nextStep', B).
biopax3_db:featureLocationType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#featureLocationType', B).
biopax3_db:participantStoichiometry(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#participantStoichiometry', B).
biopax3_db:evidence(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#evidence', B).
biopax3_db:cofactor(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#cofactor', B).
biopax3_db:memberFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#memberFeature', B).
biopax3_db:memberPhysicalEntity(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#memberPhysicalEntity', B).
biopax3_db:subRegion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#subRegion', B).
biopax3_db:memberEntityReference(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#memberEntityReference', B).
biopax3_db:template(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#template', B).
biopax3_db:regionType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#regionType', B).
biopax3_db:tissue(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#tissue', B).
biopax3_db:entityReferenceType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#entityReferenceType', B).
biopax3_db:sequenceIntervalBegin(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequenceIntervalBegin', B).
biopax3_db:phenotype(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#phenotype', B).
biopax3_db:cellularLocation(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#cellularLocation', B).
biopax3_db:scoreSource(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#scoreSource', B).
biopax3_db:absoluteRegion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#absoluteRegion', B).
biopax3_db:structure(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#structure', B).
biopax3_db:physicalEntity(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#physicalEntity', B).
biopax3_db:controlled(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#controlled', B).
biopax3_db:organism(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#organism', B).
biopax3_db:cellType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#cellType', B).
biopax3_db:taxonXref(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#taxonXref', B).
biopax3_db:modificationType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#modificationType', B).
biopax3_db:stepConversion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stepConversion', B).
biopax3_db:entityReference(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#entityReference', B).
biopax3_db:sequenceIntervalEnd(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequenceIntervalEnd', B).
biopax3_db:bindsTo(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#bindsTo', B).
biopax3_db:entityFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#entityFeature', B).
biopax3_db:component(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#component', B).
biopax3_db:url(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#url', X), rdf_literal_to_native(X, B).
biopax3_db:deltaS(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaS', X), rdf_literal_to_native(X, B).
biopax3_db:eCNumber(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#eCNumber', X), rdf_literal_to_native(X, B).
biopax3_db:availability(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#availability', X), rdf_literal_to_native(X, B).
biopax3_db:term(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#term', X), rdf_literal_to_native(X, B).
biopax3_db:biopax_comment(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#comment', Y), rdf_literal_to_native(Y, B).
biopax3_db:deltaH(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaH', X), rdf_literal_to_native(X, B).
biopax3_db:author(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#author', X), rdf_literal_to_native(X, B).
biopax3_db:biopax_name(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#name', Y), rdf_literal_to_native(Y, B).
biopax3_db:source(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#source', X), rdf_literal_to_native(X, B).
biopax3_db:conversionDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#conversionDirection', X), rdf_literal_to_native(X, B).
biopax3_db:value(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#value', X), rdf_literal_to_native(X, B).
biopax3_db:patoData(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#patoData', X), rdf_literal_to_native(X, B).
biopax3_db:structureFormat(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#structureFormat', X), rdf_literal_to_native(X, B).
biopax3_db:id(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#id', X), rdf_literal_to_native(X, B).
biopax3_db:dbVersion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#dbVersion', X), rdf_literal_to_native(X, B).
biopax3_db:catalysisDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#catalysisDirection', X), rdf_literal_to_native(X, B).
biopax3_db:stoichiometricCoefficient(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stoichiometricCoefficient', X), rdf_literal_to_native(X, B).
biopax3_db:stepDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stepDirection', X), rdf_literal_to_native(X, B).
biopax3_db:structureData(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#structureData', X), rdf_literal_to_native(X, B).
biopax3_db:pMg(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#pMg', X), rdf_literal_to_native(X, B).
biopax3_db:deltaGPrime0(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaGPrime0', X), rdf_literal_to_native(X, B).
biopax3_db:templateDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#templateDirection', X), rdf_literal_to_native(X, B).
biopax3_db:standardName(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#standardName', X), rdf_literal_to_native(X, B).
biopax3_db:temperature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#temperature', X), rdf_literal_to_native(X, B).
biopax3_db:controlType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#controlType', X), rdf_literal_to_native(X, B).
biopax3_db:intraMolecular(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#intraMolecular', X), rdf_literal_to_native(X, B).
biopax3_db:molecularWeight(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#molecularWeight', X), rdf_literal_to_native(X, B).
biopax3_db:displayName(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#displayName', X), rdf_literal_to_native(X, B).
biopax3_db:ph(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#ph', X), rdf_literal_to_native(X, B).
biopax3_db:positionStatus(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#positionStatus', X), rdf_literal_to_native(X, B).
biopax3_db:sequence(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequence', X), rdf_literal_to_native(X, B).
biopax3_db:kPrime(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#kPrime', X), rdf_literal_to_native(X, B).
biopax3_db:chemicalFormula(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#chemicalFormula', X), rdf_literal_to_native(X, B).
biopax3_db:year(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#year', X), rdf_literal_to_native(X, B).
biopax3_db:idVersion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#idVersion', X), rdf_literal_to_native(X, B).
biopax3_db:sequencePosition(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequencePosition', X), rdf_literal_to_native(X, B).
biopax3_db:spontaneous(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#spontaneous', X), rdf_literal_to_native(X, B).
biopax3_db:title(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#title', X), rdf_literal_to_native(X, B).
biopax3_db:db(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#db', X), rdf_literal_to_native(X, B).
biopax3_db:ionicStrength(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#ionicStrength', X), rdf_literal_to_native(X, B).
