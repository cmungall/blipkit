app_title('OBD-CT Demo').
amigo_port(8109).
amigo_component(amigo_property).
amigo_component(amigo_instance).

amigo_param(app_name,ct).
data_class(property).

use_bioresource(ct_test_data).

app_param(show_ids,false).
amigo_param(about,
            [
             p('This is a demo application illustrating a generic via over the UCSF clinical trial data and class schema. this will be later extended to have non-generic views.')
            ]).

