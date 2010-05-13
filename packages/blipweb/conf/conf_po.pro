app_title('AmiGO-2-prototype: BASIC : Plant').
amigo_port(8105).
use_bioresource(plant).
use_bioresource(poc('associations/maize_pha.pro')).
use_bioresource(poc('associations/maize_stock.pro')).
use_bioresource(poc('associations/gramene_pha.pro')).

amigo_param(app_name,po).
amigo_param(about,
            [h2('Amigo-NG, Plant Anatomical Structures'),
             p('This is a prototype of a new AmiGO browser')]).
