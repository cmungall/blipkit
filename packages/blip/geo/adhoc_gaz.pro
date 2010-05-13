:- use_module(library(semweb/rdf_db)).
:- use_module(bio(metadata_db)).

:- rdf_register_ns(georss,'http://www.georss.org/georss/').
:- rdf_register_ns(dbpedia,'http://dbpedia.org/resource/').
:- rdf_register_ns(wikipedia,'http://en.wikipedia.org/wiki/').
:- rdf_register_ns(pos,'http://www.w3.org/2003/01/geo/wgs84_pos#').

% blip-findall -i geo_en.nt -f turtle -r gaz -u adhoc_gaz gaz_wikipedia_lat_long/5 
gaz_wikipedia_lat_long(Gaz,WP,Lat,Long,Uniq) :-
        rdf_has(Geo,pos:lat,literal(type(_,ALat))),
        rdf_has(Geo,pos:long,literal(type(_,ALong))),
        atom_number(ALat,Lat),
        atom_number(ALong,Long),
        uri_encoded(path,DecodedURI,Geo),
        rdf_global_id(_:Name, DecodedURI),
        concat_atom(Toks,'_',Name),
        concat_atom(Toks,' ',Name2),
        setof(Gaz,entity_label_or_synonym(Gaz,Name2),GazL),
        (   GazL=[Gaz]
        ->  Uniq=true
        ;   Uniq=false,
            member(Gaz,GazL)),
        rdf_global_id(_:Local, Geo),
        rdf_global_id(wikipedia:Local, WP).




