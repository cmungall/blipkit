% assoc_rel(id, from_id, to_id, relationship_type_id)
% association(id, term_id, gene_product_id, is_not, role_group, assocdate, source_db_id)
unique(association,id).
view(association(TID,PID,Not),
     (term(TUID,_,_,TID,_,_),association(_,TUID,PID,Not,_,_,_))).
% association_qualifier(id, association_id, term_id, value)
% db(id, name, fullname, datatype, generic_url, url_syntax)
% dbxref(id, xref_key, xref_keytype, xref_dbname, xref_desc)
% evidence(id, code, association_id, dbxref_id, seq_acc)
% evidence_dbxref(evidence_id, dbxref_id)
% gene_product(id, symbol, dbxref_id, species_id, secondary_species_id, type_id, full_name)
view(gene_product(ID,Sym),gene_product(ID,Sym,_,_,_,_,_)).
view(gene_product(ID,Sym,Type),(gene_product(ID,Sym,_,_,_,TUID,_),
                                term(TUID,Type,_,_,_,_))).
% gene_product_count(term_id, code, speciesdbname, product_count)
% gene_product_property(gene_product_id, property_key, property_val)
% gene_product_seq(gene_product_id, seq_id, is_primary_seq)
% gene_product_synonym(gene_product_id, product_synonym)
% graph_path(id, term1_id, term2_id, distance)
% graph_path2term(graph_path_id, term_id, rank)
% instance_data(release_name, release_type, release_notes)
% seq(id, display_id, description, seq, seq_len, md5checksum, moltype, timestamp)
% seq_dbxref(seq_id, dbxref_id)
% seq_property(id, seq_id, property_key, property_val)
% source_audit(source_path, source_type, source_md5, source_mtime)
% species(id, ncbi_taxa_id, common_name, lineage_string, genus, species)
% term(id, name, term_type, acc, is_obsolete, is_root)
view(class(ID,N),term(_,N,_,ID,_,_)).
view(belongs(ID,O),term(_,_,O,ID,_,_)).
unique(term,acc).
unique(term,id).
% term2term(id, relationship_type_id, term1_id, term2_id, complete)
view(parent(ID,PID),(term(UID,_,_,ID,_,_),
                     term(PUID,_,_,PID,_,_),
                     term2term(_,_,PUID,UID,_))).
view(parent(ID,TID,PID),(term(UID,_,_,ID,_,_),
                         term(PUID,_,_,PID,_,_),
                         term(TUID,_,_,TID,_,_),
                         term2term(_,TUID,PUID,UID,_))).
view(subclass(ID,PID),(parent(ID,is_a,PID))).
% term_audit(term_id, term_loadtime)
% term_dbxref(term_id, dbxref_id, is_for_definition)
% term_definition(term_id, term_definition, dbxref_id, term_comment, reference)
% term_synonym(term_id, term_synonym, acc_synonym, synonym_type_id)


relation(assoc_rel, 4).
relation(association, 7).
relation(association_qualifier, 4).
relation(db, 6).
relation(dbxref, 5).
relation(evidence, 5).
relation(evidence_dbxref, 2).
relation(gene_product, 7).
relation(gene_product_count, 4).
relation(gene_product_property, 3).
relation(gene_product_seq, 3).
relation(gene_product_synonym, 2).
relation(graph_path, 4).
relation(graph_path2term, 3).
relation(instance_data, 3).
relation(seq, 8).
relation(seq_dbxref, 2).
relation(seq_property, 4).
relation(source_audit, 4).
relation(species, 6).
relation(term, 6).
relation(term2term, 5).
relation(term_audit, 2).
relation(term_dbxref, 3).
relation(term_definition, 5).
relation(term_synonym, 4).
attribute(1, assoc_rel, id, integer).
attribute(1, association, id, integer).
attribute(1, association_qualifier, id, integer).
attribute(1, db, id, integer).
attribute(1, dbxref, id, integer).
attribute(1, evidence, id, integer).
attribute(1, evidence_dbxref, evidence_id, integer).
attribute(1, gene_product, id, integer).
attribute(1, gene_product_count, term_id, integer).
attribute(1, gene_product_property, gene_product_id, integer).
attribute(1, gene_product_seq, gene_product_id, integer).
attribute(1, gene_product_synonym, gene_product_id, integer).
attribute(1, graph_path, id, integer).
attribute(1, graph_path2term, graph_path_id, integer).
attribute(1, instance_data, release_name, string).
attribute(1, seq, id, integer).
attribute(1, seq_dbxref, seq_id, integer).
attribute(1, seq_property, id, integer).
attribute(1, source_audit, source_path, string).
attribute(1, species, id, integer).
attribute(1, term, id, integer).
attribute(1, term2term, id, integer).
attribute(1, term_audit, term_id, integer).
attribute(1, term_dbxref, term_id, integer).
attribute(1, term_definition, term_id, integer).
attribute(1, term_synonym, term_id, integer).
attribute(2, assoc_rel, from_id, integer).
attribute(2, association, term_id, integer).
attribute(2, association_qualifier, association_id, integer).
attribute(2, db, name, string).
attribute(2, dbxref, xref_key, string).
attribute(2, evidence, code, string).
attribute(2, evidence_dbxref, dbxref_id, integer).
attribute(2, gene_product, symbol, string).
attribute(2, gene_product_count, code, string).
attribute(2, gene_product_property, property_key, string).
attribute(2, gene_product_seq, seq_id, integer).
attribute(2, gene_product_synonym, product_synonym, string).
attribute(2, graph_path, term1_id, integer).
attribute(2, graph_path2term, term_id, integer).
attribute(2, instance_data, release_type, string).
attribute(2, seq, display_id, string).
attribute(2, seq_dbxref, dbxref_id, integer).
attribute(2, seq_property, seq_id, integer).
attribute(2, source_audit, source_type, string).
attribute(2, species, ncbi_taxa_id, integer).
attribute(2, term, name, string).
attribute(2, term2term, relationship_type_id, integer).
attribute(2, term_audit, term_loadtime, integer).
attribute(2, term_dbxref, dbxref_id, integer).
attribute(2, term_definition, term_definition, string).
attribute(2, term_synonym, term_synonym, string).
attribute(3, assoc_rel, to_id, integer).
attribute(3, association, gene_product_id, integer).
attribute(3, association_qualifier, term_id, integer).
attribute(3, db, fullname, string).
attribute(3, dbxref, xref_keytype, string).
attribute(3, evidence, association_id, integer).
attribute(3, gene_product, dbxref_id, integer).
attribute(3, gene_product_count, speciesdbname, string).
attribute(3, gene_product_property, property_val, string).
attribute(3, gene_product_seq, is_primary_seq, integer).
attribute(3, graph_path, term2_id, integer).
attribute(3, graph_path2term, rank, integer).
attribute(3, instance_data, release_notes, string).
attribute(3, seq, description, string).
attribute(3, seq_property, property_key, string).
attribute(3, source_audit, source_md5, string).
attribute(3, species, common_name, string).
attribute(3, term, term_type, string).
attribute(3, term2term, term1_id, integer).
attribute(3, term_dbxref, is_for_definition, integer).
attribute(3, term_definition, dbxref_id, integer).
attribute(3, term_synonym, acc_synonym, string).
attribute(4, assoc_rel, relationship_type_id, integer).
attribute(4, association, is_not, integer).
attribute(4, association_qualifier, value, string).
attribute(4, db, datatype, string).
attribute(4, dbxref, xref_dbname, string).
attribute(4, evidence, dbxref_id, integer).
attribute(4, gene_product, species_id, integer).
attribute(4, gene_product_count, product_count, integer).
attribute(4, graph_path, distance, integer).
attribute(4, seq, seq, string).
attribute(4, seq_property, property_val, string).
attribute(4, source_audit, source_mtime, integer).
attribute(4, species, lineage_string, string).
attribute(4, term, acc, string).
attribute(4, term2term, term2_id, integer).
attribute(4, term_definition, term_comment, string).
attribute(4, term_synonym, synonym_type_id, integer).
attribute(5, association, role_group, integer).
attribute(5, db, generic_url, string).
attribute(5, dbxref, xref_desc, string).
attribute(5, evidence, seq_acc, string).
attribute(5, gene_product, secondary_species_id, integer).
attribute(5, seq, seq_len, integer).
attribute(5, species, genus, string).
attribute(5, term, is_obsolete, integer).
attribute(5, term2term, complete, integer).
attribute(5, term_definition, reference, string).
attribute(6, association, assocdate, integer).
attribute(6, db, url_syntax, string).
attribute(6, gene_product, type_id, integer).
attribute(6, seq, md5checksum, string).
attribute(6, species, species, string).
attribute(6, term, is_root, integer).
attribute(7, association, source_db_id, integer).
attribute(7, gene_product, full_name, string).
attribute(7, seq, moltype, string).
attribute(8, seq, timestamp, integer).


