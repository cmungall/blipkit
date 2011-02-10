:- use_module(library('semweb/rdf_db')).

% to generate:
% blip -i http://obo.cvs.sourceforge.net/viewvc/*checkout*/obo/obo/website/cgi-bin/ontologies.txt -f 'tagval(obo_metadata)' ontol-generate-rdf-register-ns


% TODO: move
/*
:- rdf_register_ns('MA','http://purl.org/obo/owl/MA#MA_').
:- rdf_register_ns('AAO','http://purl.org/obo/owl/AAO#AAO_').
:- rdf_register_ns('ADW','http://purl.org/obo/owl/ADW#ADW_').
:- rdf_register_ns('PATO','http://purl.org/obo/owl/PATO#PATO_').
:- rdf_register_ns('BILA','http://purl.org/obo/owl/BILA#BILA_').
:- rdf_register_ns('GO','http://purl.org/obo/owl/GO#GO_').
:- rdf_register_ns('BTO','http://purl.org/obo/owl/BTO#BTO_').
:- rdf_register_ns('CARO','http://purl.org/obo/owl/CARO#CARO_').
:- rdf_register_ns('BSPO','http://purl.org/obo/owl/BSPO#BSPO_').
:- rdf_register_ns('SPD','http://purl.org/obo/owl/SPD#SPD_').
:- rdf_register_ns('CL','http://purl.org/obo/owl/CL#CL_').
:- rdf_register_ns('GO','http://purl.org/obo/owl/GO#GO_').
:- rdf_register_ns('CHEBI','http://purl.org/obo/owl/CHEBI#CHEBI_').
:- rdf_register_ns('RESID','http://purl.org/obo/owl/RESID#RESID_').
:- rdf_register_ns('DDANAT','http://purl.org/obo/owl/DDANAT#DDANAT_').
:- rdf_register_ns('DOID','http://purl.org/obo/owl/DOID#DOID_').
:- rdf_register_ns('IDO','http://purl.org/obo/owl/IDO#IDO_').
:- rdf_register_ns('EMAP','http://purl.org/obo/owl/EMAP#EMAP_').
:- rdf_register_ns('ENVO','http://purl.org/obo/owl/ENVO#ENVO_').
:- rdf_register_ns('IEV','http://purl.org/obo/owl/IEV#IEV_').
:- rdf_register_ns('ECO','http://purl.org/obo/owl/ECO#ECO_').
:- rdf_register_ns('EV','http://purl.org/obo/owl/EV#EV_').
:- rdf_register_ns('FIX','http://purl.org/obo/owl/FIX#FIX_').
:- rdf_register_ns('FBbt','http://purl.org/obo/owl/FBbt#FBbt_').
:- rdf_register_ns('FBdv','http://purl.org/obo/owl/FBdv#FBdv_').
:- rdf_register_ns('FBsp','http://purl.org/obo/owl/FBsp#FBsp_').
:- rdf_register_ns('FBcv','http://purl.org/obo/owl/FBcv#FBcv_').
:- rdf_register_ns('FMA','http://purl.org/obo/owl/FMA#FMA_').
:- rdf_register_ns('FAO','http://purl.org/obo/owl/FAO#FAO_').
:- rdf_register_ns('YPO','http://purl.org/obo/owl/YPO#YPO_').
:- rdf_register_ns('BOOTStrep','http://purl.org/obo/owl/BOOTStrep#BOOTStrep_').
:- rdf_register_ns('EHDAA','http://purl.org/obo/owl/EHDAA#EHDAA_').
:- rdf_register_ns('EHDA','http://purl.org/obo/owl/EHDA#EHDA_').
:- rdf_register_ns('HP','http://purl.org/obo/owl/HP#HP_').
:- rdf_register_ns('FBbi','http://purl.org/obo/owl/FBbi#FBbi_').
:- rdf_register_ns('IPR','http://purl.org/obo/owl/IPR#IPR_').
:- rdf_register_ns('MP','http://purl.org/obo/owl/MP#MP_').
:- rdf_register_ns('RO','http://purl.org/obo/owl/RO#RO_').
:- rdf_register_ns('MFO','http://purl.org/obo/owl/MFO#MFO_').
:- rdf_register_ns('MO','http://purl.org/obo/owl/MO#MO_').
:- rdf_register_ns('GO','http://purl.org/obo/owl/GO#GO_').
:- rdf_register_ns('IMR','http://purl.org/obo/owl/IMR#IMR_').
:- rdf_register_ns('TGMA','http://purl.org/obo/owl/TGMA#TGMA_').
:- rdf_register_ns('MIRO','http://purl.org/obo/owl/MIRO#MIRO_').
:- rdf_register_ns('MPATH','http://purl.org/obo/owl/MPATH#MPATH_').
:- rdf_register_ns('NCBITaxon','http://purl.org/obo/owl/NCBITaxon#NCBITaxon_').
:- rdf_register_ns('NCIt','http://purl.org/obo/owl/NCIt#NCIt_').
:- rdf_register_ns('NEWT','http://purl.org/obo/owl/NEWT#NEWT_').
:- rdf_register_ns('NMR','http://purl.org/obo/owl/NMR#NMR_').
:- rdf_register_ns('PW','http://purl.org/obo/owl/PW#PW_').
:- rdf_register_ns('EO','http://purl.org/obo/owl/EO#EO_').
:- rdf_register_ns('TO','http://purl.org/obo/owl/TO#TO_').
:- rdf_register_ns('PLO','http://purl.org/obo/owl/PLO#PLO_').
:- rdf_register_ns('PO','http://purl.org/obo/owl/PO#PO_').
:- rdf_register_ns('PO','http://purl.org/obo/owl/PO#PO_').
:- rdf_register_ns('PRO','http://purl.org/obo/owl/PRO#PRO_').
:- rdf_register_ns('ProPreO','http://purl.org/obo/owl/ProPreO#ProPreO_').
:- rdf_register_ns('MI','http://purl.org/obo/owl/MI#MI_').
:- rdf_register_ns('MOD','http://purl.org/obo/owl/MOD#MOD_').
:- rdf_register_ns('MS','http://purl.org/obo/owl/MS#MS_').
%:- rdf_register_ns('OBO_REL','http://purl.org/obo/owl/OBO_REL#').
:- rdf_register_ns('REX','http://purl.org/obo/owl/REX#REX_').
:- rdf_register_ns('SEP','http://purl.org/obo/owl/SEP#SEP_').
:- rdf_register_ns('SO','http://purl.org/obo/owl/SO#SO_').
:- rdf_register_ns('SBO','http://purl.org/obo/owl/SBO#SBO_').
:- rdf_register_ns('SOPHARM','http://purl.org/obo/owl/SOPHARM#SOPHARM_').
:- rdf_register_ns('TAO','http://purl.org/obo/owl/TAO#TAO_').
:- rdf_register_ns('TTO','http://purl.org/obo/owl/TTO#TTO_').
:- rdf_register_ns('TRANS','http://purl.org/obo/owl/TRANS#TRANS_').
:- rdf_register_ns('GRO','http://purl.org/obo/owl/GRO#GRO_').
:- rdf_register_ns('GRO','http://purl.org/obo/owl/GRO#GRO_').
:- rdf_register_ns('TADS','http://purl.org/obo/owl/TADS#TADS_').
:- rdf_register_ns('UBERON','http://purl.org/obo/owl/UBERON#UBERON_').
:- rdf_register_ns('UO','http://purl.org/obo/owl/UO#UO_').
:- rdf_register_ns('WBbt','http://purl.org/obo/owl/WBbt#WBbt_').
:- rdf_register_ns('WBls','http://purl.org/obo/owl/WBls#WBls_').
:- rdf_register_ns('WBPhenotype','http://purl.org/obo/owl/WBPhenotype#WBPhenotype_').
:- rdf_register_ns('XAO','http://purl.org/obo/owl/XAO#XAO_').
:- rdf_register_ns('ZEA','http://purl.org/obo/owl/ZEA#ZEA_').
:- rdf_register_ns('ZFA','http://purl.org/obo/owl/ZFA#ZFA_').
*/

:- rdf_register_ns(obd,'http://www.bioontology.org/obd/obd-ontology#').
:- rdf_register_ns(oboInOwl,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_ns(biopax1,'http://www.biopax.org/release/biopax-level1.owl#').
:- rdf_register_ns(mged,'http://mged.sourceforge.net/ontologies/MGEDOntology.owl#').

:- rdf_register_ns(oban,'http://www.berkeleybop.org/ontologies/oban/alpha#').
:- rdf_register_ns(galen,'http://www.co-ode.org/ontologies/galen#').
:- rdf_register_ns(propreo,'http://lsdis.cs.uga.edu/projects/glycomics/propreo#').
:- rdf_register_ns(reactome,'http://www.reactome.org/biopax#').

:- rdf_register_ns(sao,'http://ccdb.ucsd.edu/SAO/1.2#').
:- rdf_register_ns('SAO_HumanDPO','http://ccdb.ucsd.edu/SAO/HumanDPO.owl#').
:- rdf_register_ns('BIRN_PDPO','http://ccdb.ucsd.edu/PDPhenotypeOntology/1.0#'). % animal
:- rdf_register_ns('BIRN_ImagePhenotype','http://ccdb.ucsd.edu/SAO/DPO/1.0/ImagePhenotype.owl').
:- rdf_register_ns('BIRN_PDStage','http://ccdb.ucsd.edu/PDStageOntology/1.0/').
:- rdf_register_ns('BIRN_SA','http://ccdb.ucsd.edu/smart_atlas_ontology/sa.owl#').
:- rdf_register_ns(birnlex_anatomy,'http://purl.org/nbirn/birnlex/ontology/BIRNLex-Anatomy.owl#').
:- rdf_register_ns(birnlex_disease,'http://purl.org/nbirn/birnlex/ontology/BIRNLex-Disease.owl#').
:- rdf_register_ns(birnlex_tax,'http://purl.org/nbirn/birnlex/ontology/BIRNLex-OrganismalTaxonomy.owl#').
%:- rdf_register_ns(birnlex_ubo2,'http://birnlex.nbirn.net/ontology/BIRNLex-OBO-UBO.owl#').
%:- rdf_register_ns(birnlex_ubo,'http://purl.org/nbirn/birnlex/ontology/BIRNLex-OBO-UBO.owl#').
%:- rdf_register_ns('NIF_Molecule','http://purl.org/nif/ontology/NIF-Molecule.owl#').
%:- rdf_register_ns('PKB','http://ccdb.ucsd.edu/SAO/DPO/2.0/DPO.owl#').
:- rdf_register_ns('PKB','http://ccdb.ucsd.edu/PKB/1.0/PKB.owl#').
:- rdf_register_ns('NDPO','http://ccdb.ucsd.edu/SAO/Disease/1.0/NDPO.owl#').
%:- rdf_register_ns('snap','http://ccdb.ucsd.edu/SAO/Disease/1.0/NDPO.owl#').
:- rdf_register_ns(bfo,'http://www.ifomis.org/bfo/1.1#',[force(true)]).
:- rdf_register_ns(snap,'http://www.ifomis.org/bfo/1.1/snap#',[force(true)]).
:- rdf_register_ns(span,'http://www.ifomis.org/bfo/1.1/span#',[force(true)]).


