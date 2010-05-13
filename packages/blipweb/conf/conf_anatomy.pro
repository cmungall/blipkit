app_title('OBO Anatomy ontologies DEMO').
amigo_port(8108).
use_bioresource(cell).
use_bioresource(fly_anatomy).
use_bioresource(mosquito_anatomy).
use_bioresource(zebrafish_anatomy).
use_bioresource(medaka_anatomy).
use_bioresource(mouse_anatomy).
use_bioresource(plant_anatomy).
use_bioresource(fma_pro).
amigo_component(amigo_xp).
app_param(show_ids,false).

:- multifile ontol_db:is_transitive/1.
ontol_db:is_transitive('http://nlm.nih.gov/ontology/FMAInOWL#part_of').
ontol_db:is_transitive('http://nlm.nih.gov/ontology/FMAInOWL#regional_part_of').
%ontol_db:is_transitive('http://nlm.nih.gov/ontology/FMAInOWL#part').
%ontol_db:is_transitive('http://nlm.nih.gov/ontology/FMAInOWL#regional_part').

amigo_param(app_name,anatomy).
amigo_param(about,
            [
             p('This is a demo application illustrating navigation of OBO anatomy ontologies.')
            ]).

bench_call(test_serval([data_class='term',
                        id='http://nlm.nih.gov/ontology/FMAInOWL#Wall_of_biatrial_part_of_heart',
                        page=detail_page]),
           []).
                       

