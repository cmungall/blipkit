:- module(geo_db,[]).

:- use_module(bio(dbmeta)).


%% point_latitude(?Loc,?Lat:number)
% Φ is point of a place on Earth (or other planetary body) north or
% south of the equator. Lines of Latitude are the horizontal lines
% shown running east-to-west on maps (particularly so in the Mercator
% projection). Technically, latitude is an angular measurement in
% degrees (marked with °) ranging from 0° at the equator (low
% latitude) to 90° at the poles (90° N or +90° for the North Pole and
% 90° S or −90° for the South Pole). The complementary angle of a
% latitude is called the colatitude.
:- extensional(point_latitude/2).


%% point_longitude(?Loc,?Long:number)
% λ is the geographic coordinate most commonly used in cartography and
% global navigation for east-west measurement. A line of longitude is
% a north-south meridian and half of a great circle.
:- extensional(point_longitude/2).


% e.g. point_lat_long('South_Pole',-90,0)
point_lat_long(Loc,Lat,Long) :-
        point_latitude(Loc,Lat),
        point_longitude(Loc,Long).

/*
  TODO: use sparql_client to map to
  
  PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
SELECT ?subject ?label ?lat ?long WHERE {
<http://dbpedia.org/resource/Eiffel_Tower> geo:lat ?eiffelLat.
<http://dbpedia.org/resource/Eiffel_Tower> geo:long ?eiffelLong.
?subject geo:lat ?lat.
?subject geo:long ?long.
?subject rdfs:label ?label.
FILTER(xsd:float(?lat) - xsd:float(?eiffelLat) <= 0.05 && xsd:float(?eiffelLat) - xsd:float(?lat) <= 0.05 &&
xsd:float(?long) - xsd:float(?eiffelLong) <= 0.05 && xsd:float(?eiffelLong) - xsd:float(?long) <= 0.05 &&
lang(?label) = "en"
).
} LIMIT 20

http://dbpedia.org/snorql/?query=PREFIX+geo%3A+%3Chttp%3A%2F%2Fwww.w3.org%2F2003%2F01%2Fgeo%2Fwgs84_pos%23%3E%0D%0ASELECT+%3Fsubject+%3Flabel+%3Flat+%3Flong+WHERE+{%0D%0A%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FEiffel_Tower%3E+geo%3Alat+%3FeiffelLat.%0D%0A%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FEiffel_Tower%3E+geo%3Along+%3FeiffelLong.%0D%0A%3Fsubject+geo%3Alat+%3Flat.%0D%0A%3Fsubject+geo%3Along+%3Flong.%0D%0A%3Fsubject+rdfs%3Alabel+%3Flabel.%0D%0AFILTER(xsd%3Afloat(%3Flat)+-+xsd%3Afloat(%3FeiffelLat)+%3C%3D+0.05+%26%26+xsd%3Afloat(%3FeiffelLat)+-+xsd%3Afloat(%3Flat)+%3C%3D+0.05+%26%26%0D%0Axsd%3Afloat(%3Flong)+-+xsd%3Afloat(%3FeiffelLong)+%3C%3D+0.05+%26%26+xsd%3Afloat(%3FeiffelLong)+-+xsd%3Afloat(%3Flong)+%3C%3D+0.05+%26%26%0D%0Alang(%3Flabel)+%3D+%22en%22%0D%0A).%0D%0A}+LIMIT+20
  */
  
point_bounded_by(Loc,Lat1,Long1,Lat2,Long2) :-
        point_lat_range(Loc,Lat1,Lat2),
        point_long_range(Loc,Long1,Long2).

point_lat_range(Loc,S,E) :-
        point_latitude(Loc,Lat),
        Lat >= Lat1,
        Lat =< Lat2.

% http://mathforum.org/library/drmath/results.html?textsearch=haversine&textsearch_bool_type=and&textsearch_whole_words=no

/** <module> 

  ---+ Synopsis

==
:- use_module(bio(geo_db)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org



*/
