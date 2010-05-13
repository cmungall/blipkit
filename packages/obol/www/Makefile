BASE=http://www.berkeleybop.org/obol

all: index.html

obo-all/ontology_index.xml: obol_metadata.txt
	$(HOME)/obo-utils/obo-downloader.pl --skip-format owl-classified-by-pellet --skip-format pellet-report --baseuri $(BASE) obol_metadata.txt || rm $@
.PRECIOUS: obo-all/ontology_index.xml

index.html:  obo-all/ontology_index.xml create-index.xsl obo-all/ontology_index.rdf
	xsltproc create-index.xsl $< > $@.tmp && mv $@.tmp $@

obo-all/ontology_index.rdf:  obo-all/ontology_index.xml
	xsltproc metadata-to-rdf.xsl $< > $@.tmp && mv $@.tmp $@

update:
	rsync -avz index.html css/ stylesheet.css obo-all sos.lbl.gov:public_html/obol ;
	rsync -avz -L css/* sos.lbl.gov:public_html/obol/css/
