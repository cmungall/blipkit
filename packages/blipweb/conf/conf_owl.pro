app_title('OWL Demo').
amigo_component(amigo_instance).
app_param(show_ids,false).
amigo_param(app_name,owlex).
data_class(property).
use_bioresource(rdfs).
use_bioresource(owl).
:- use_module(bio(ontol_bridge_from_owl)).
