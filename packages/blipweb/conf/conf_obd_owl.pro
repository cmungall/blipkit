app_title('OBD Demo: OWL Data').
amigo_component(amigo_feature).
amigo_component(amigo_mutant).
use_bioresource(testobd).
app_param(show_ids,false).
amigo_param(app_name,obdowl).
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
             h3('coming soon...'),
             p('complex queries')]).
