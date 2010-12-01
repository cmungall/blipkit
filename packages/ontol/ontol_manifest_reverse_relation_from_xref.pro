
:- module(ontol_manifest_reverse_relation_from_xref,[]).

:- use_module(bio(ontol_db)).

%:- multifile ontol_db:parent_over_nr/3.
%ontol_db:parent_over_nr(xrefed_by,Y,X):- class_xref(X,Y),class(Y).
%:- multifile ontol_db:parent_over/3.
%ontol_db:parent_over(xrefed_by,Y,X):- class_xref(X,Y),class(Y).
:- multifile ontol_db:restriction/3.
ontol_db:restriction(Y,xrefed_by,X):- class_xref(X,Y),class(Y).


        
