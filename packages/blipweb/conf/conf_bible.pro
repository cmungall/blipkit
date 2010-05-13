app_title('OWL Demo: Bible ontology: Generic Views').
amigo_port(8103).
amigo_component(amigo_instance).
app_param(show_ids,false).
use_bioresource(bible).
use_bioresource(owl).
use_bioresource(rdfs).

amigo_param(app_name,bible).
amigo_param(about,
            [p('This is a generic interface to the Semantic Bible, an ',
               'interesting resource I found at ',
               a(href='http://protege.stanford.edu/plugins/owl/ontologies.html',
                 'http://protege.stanford.edu/plugins/owl/ontologies.html')),
             br,
             p('This ontology is intended purely to demonstrate the use of ',
               'AmiGO-NG as a generic ontology and instance browser')]).
               
