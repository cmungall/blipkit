:- module(gxa,[]).

:- use_module(library(http/json)).

:- json_object gene(name, id).


gene_info(G,Info) :-
        sformat(URL,'http://www.ebi.ac.uk/gxa/api?geneIs=~w&format=json',[G]),
        http_get(URL,R,[]).


/** <module> wrapper for GXA

  ---+ Synopsis

==
:- use_module(bio(gxa)).

% 
demo:-
  nl.
==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/

