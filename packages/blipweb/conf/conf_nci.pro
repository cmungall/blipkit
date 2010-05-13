app_title('OWL Demo: NCI Thesaurus: Generic Views').
amigo_port(8104).
amigo_component(amigo_instance).
app_param(show_ids,false).
bioresource(nci).
bioresource(owl).

amigo_param(app_name,nci).
amigo_param(about,
            p('This is a generic interface to the NCI Thesaurus')).
