/* -*- Mode: Prolog -*- */

:- module(gsl,
          [
           gsl_sf_bessel_J0/2
          ]
         ).

:- initialization load_foreign_library(gsl4pl).

t:-
        gsl_sf_bessel_J0(0.5,Y),
        writeln(Y).

/** <module> wrapper for GNU Scientific Library

---+ Synopsis

==
:- use_module(bio(gsl)).

% 
demo:-
  nl.
  

==

---+ Details

http://www.gnu.org/software/gsl/

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
