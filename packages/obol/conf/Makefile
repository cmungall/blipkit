
gosubset_prok.obo:
	blip -r go ontol-query -query "entity_partition(ID,gosubset_prok)" -to obo > $@

pato_rate.obo:
	blip -r pato ontol-query -query "subclassRT(ID,'PATO:0000161')" -to obo > $@

mireot_omp-ontol_db.pro:  gosubset_prok.obo pato_rate.obo
	blip -i gosubset_prok.obo -i pato_rate.obo io-convert -to ontol_db:pro > $@
