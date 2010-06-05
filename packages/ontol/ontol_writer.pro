:- module(ontol_writer,
          [
           write_class/2,
           write_class/3
          ]).

:- use_module(bio(mode)).

:- multifile
    write_class/2,
    write_class/3.

:- mode write_class(+,+) is det.
write_class(Fmt,ID):- 
    write_class(Fmt,ID,[]).

:- mode write_class(+,+,+) is det.
write_class(_,_,_):- fail.

