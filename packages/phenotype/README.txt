
OWL/Thea based modules:

* pkb_db  
** pkb_web

ontol_db based:

* phenotype_db - subclass/2 and restriction/2 (pre-reasoned). override context_relation/2
** phenoblast (indirectly)

ontol_db modules are only used for computing subsumption and LCA. Should we replace with Thea throughout?

bridging

* pkb_from_owl - actually from NIF model..
* pkb_from_obo - gene.obo files and phenotype_db:feature_phenotype/2
* pkb_to_phenotype - manifests subclass/2 and restriction/2 from OWL

---+ TODO

remove feature_phenotype/2
