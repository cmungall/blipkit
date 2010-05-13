:- module(ontol_sqlmap_obd_inf,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(curation_db),[]). % move?

:- load_schema_defs(bio('sql_schema/schema_obd')).

ontol_db:parent0(X,R,Y) <- node(XI,X,_,_),link(XI,RI,YI,_),node(RI,R,_,_),node(YI,Y,_,_).
ontol_db:parent0(X,Y) <- node(XI,X,_,_),link(XI,_,YI,_),node(YI,Y,_,_).

:- [ontol_sqlmap_obd_core].

/** <module> maps inferred obd database to ontol_db

  ---+ Synopsis

==
:- use_module(bio(ontol_sqlmap_obd_inf)).

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
