app_title('OBD Demo').
:- use_module(bio(ontol_bridge_from_gosql)).
:- ontol_bridge_from_gosql:set_bridge_resource(go).

amigo_param(app_name,go).
amigo_param(about,
            [h2('GO Demo'),
             p('runs off of GO MySQL db')]).
