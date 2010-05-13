app_title('Text alignment demo').
use_bioresource(cell).
use_bioresource(go).

amigo_param(about, [

                    p('test textual alignment between cell and go')
                   ]).

amigo_component('skins/ontol_text_align_view').

% define a new mapping between a tabular format and a module:predicate
io:file_to_prolog_cmd(class_align,'tbl2p -p class_align').
io:format_module(class_align,ontol_db).
