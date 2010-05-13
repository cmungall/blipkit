amigo_component(amigo_xp).
app_title('AmiGO-2-prototype: CROSS PRODUCTS: Plant').
use_bioresource(dl_plant).
% anat
use_bioresource(poc('associations/maize_aa_xp.pro'),pro,ontol).
use_bioresource(poc('associations/tair_aa_xp.pro'),pro,ontol).
% pheno
use_bioresource(poc('associations/maize_pha_xp.pro'),pro,ontol).
use_bioresource(poc('associations/gramene_pha_xp.pro'),pro,ontol).

% test
bench_call(lfeature_query(class('PO:0006081'),L),L). % flight behavior
