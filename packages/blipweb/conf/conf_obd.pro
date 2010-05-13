app_title('OBD-Phenotype').
%amigo_component(amigo_feature).
amigo_component(amigo_mutant).
amigo_port(8100).
use_bioresource(xrf).
use_bioresource(go).
use_bioresource(cell).
use_bioresource(pato).
use_bioresource(fma).
%use_bioresource(pato_orig).
use_bioresource(mammalian_phenotype).
use_bioresource(obol_plant_trait).
use_bioresource(fly_anatomy).
use_bioresource(plant_anatomy).
use_bioresource(zebrafish_anatomy).

use_bioresource(obd_phenotype_annotations).

%use_bioresource(omim_pheno_data).
%use_bioresource(fly_pheno_data).
%use_bioresource(zfin_pheno_data).

use_bioresource(obd_genotypes).
%use_bioresource(obd_genes).


%use_bioresource(fly_ga).

%use_bioresource(zfinB_pheno_data).
%use_bioresource(zfin_gene_data).

%use_bioresource(zfin_fly_ortho_data).
%use_bioresource(organism).

amigo_param(app_name,obd).
amigo_param(about,
            [h2('Open Bio-Database'),
             p('This is a prototype of a browser for the OBD'),
             p('Currently the OBD is populated with EXAMPLE data ',
               'in the following categories:',
               ul(li(b(ontologies),
                     p('GO (wild type function), anatomical and cell')),
                  li(b(features),
                     p('wild-type genes and gene products annotated to GO')),
                  li(b(mutants),
                     p('mutant alleles, and their associated phenotypes')),
                  li(b(phenotypes),
                     p('the actual phenotypic descriptions themselves')))),
             p('For more info, see',
               a(href='http://www.fruitfly.org/~cjm/obd','OBD Info')),
             h3('coming soon...'),
             p('complex queries')]).
