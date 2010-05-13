app_title('Upper-Bio-Ontology DEMO').
amigo_port(8102).
use_bioresource(ubo).
use_bioresource(obo).
init_session_state([[open_ids,IDL]]):-
        setof(ID,belongs(ID,upper_bio_ontology),IDL).
amigo_param(app_name,ubo).

amigo_param(about, [

p('This is a demo application illustrating how existing OBO ontologies
can be integrated into an upper-mid level ontology. This can be
extremely useful for reasoning and querying'),

p('The upper ontology used here is very ad-hoc, mixing some BFO with
some fairly arbitrary upper-level biological concepts.'),

p('Note that this is NOT necessarily the view a biologist would get
when browsing OBO - the is_a hierarchy is not necessarily bets for
browsing. It is possible to have application subsets arranged in a more intuitive fashion')

]).
