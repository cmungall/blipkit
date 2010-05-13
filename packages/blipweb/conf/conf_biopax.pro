app_title('BioPax Demo').
amigo_component(amigo_instance).
amigo_port(8106).
app_param(show_ids,false).
use_bioresource(biopax1).
use_bioresource(biopax_glycolysis). % hardcoded test data
use_bioresource(owl).
use_bioresource(rdfs).
:- use_module(bio(sb_bridge_from_biopax)).

amigo_param(app_name,biopax).
amigo_param(about,
            [h2('Biopax Demo'),
             p('generic class and instance viewer over biopax pathway data')]).
