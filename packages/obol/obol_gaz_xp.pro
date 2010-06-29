:- [bio(obol_obo_xp)].

:- multifile term_label/3.
term_label(P) --> geolocation(P).

% populated place, Scotland
geolocation(F that located_in(I)) --> geographic_feature(F),[','],geoinstance(I).

geographic_feature(F) --> any_kind_of(F,'geographic feature').
geoinstance(I) --> terminal(I),{belongs(I,'GAZ')}.
