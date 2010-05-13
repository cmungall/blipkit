app_title('OWL Demo: Wine and Food ontology: Generic Views').
amigo_port(8101).
amigo_component(amigo_instance).
app_param(show_ids,false).
bioresource(wine).
bioresource(food).

amigo_param(app_name,wine).
amigo_param(about,
            p(foo)).
