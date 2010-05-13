app_title('AmiGO-2-prototype: Mouse cross-products').
amigo_component(amigo_feature).
%bioresource(uniprot_ga).
bioresource(go).
bioresource(cell).
bioresource(mouse_anatomy).
bioresource('EMAP').
bioresource(mgi_ga).
bench_call(amigo_query(feature,class('GO:0005515'),L),L). % protein binding

% protein binding in spermatid
bench_call(amigo_query(feature,class('GO:0005515',
                                     [in:class('CL:0000018')]),L),L). 
