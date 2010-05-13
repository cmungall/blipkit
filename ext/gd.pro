/* -*- Mode: Prolog -*- */

:- module(gd,
          [
           gdImageCreate/3,
           gdImagePng/2
          ]
         ).

:- initialization load_foreign_library(gd4pl).

t:-
        gdImageCreate(Im,64,64),
        writeln(Im).

