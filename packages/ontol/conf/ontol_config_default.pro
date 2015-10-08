:- multifile user:graphviz_ontol_param/2.

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).

:- multifile user:focus_node/1.
:- dynamic user:focus_node/1.


:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(edge(_,_,is_a,_),arrowhead=empty).
user:graphviz_ontol_param(edge(_,_,is_a,_),color=green).
user:graphviz_ontol_param(edge(_,_,po,_),color=blue).
user:graphviz_ontol_param(edge(_,_,'part_of',_),color=blue).
user:graphviz_ontol_param(edge(_,_,'part_of',_),label='PO').
user:graphviz_ontol_param(edge(_,_,'OBO_REL:part_of',_),color=blue).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:part_of',_),label='PO').
user:graphviz_ontol_param(edge(_,_,'OBO_REL:inheres_in',_),color=blue).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:inheres_in',_),label='inh').
user:graphviz_ontol_param(edge(_,_,integral_part_of,_),arrowtail=diamond).
user:graphviz_ontol_param(edge(_,_,integral_part_of,_),color=blue).
user:graphviz_ontol_param(edge(_,_,only_part_of,_),arrowhead=empty).
user:graphviz_ontol_param(edge(_,_,only_part_of,_),arrowtail=diamond).
user:graphviz_ontol_param(edge(_,_,only_part_of,_),color=blue).
user:graphviz_ontol_param(edge(_,_,has_part,_),arrowhead=diamond).
user:graphviz_ontol_param(edge(_,_,has_part,_),color=blue).
user:graphviz_ontol_param(edge(_,_,df,_),arrowhead=open).
user:graphviz_ontol_param(edge(_,_,df,_),color=red).
user:graphviz_ontol_param(edge(_,_,followed_by,_),color=red).
%user:graphviz_ontol_param(edge(_,_,xref,_),color=grey).
user:graphviz_ontol_param(edge(_,_,xref,_),weight=1000).
%user:graphviz_ontol_param(edge(_,_,xref,_),arrowhead=open).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:results_in_complete_development_of',_),color=grey).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:homologous_to',_),color=grey).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:homologous_to',_),weight=1000).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:homologous_to',_),penwidth=4).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:homologous_to',_),arrowhead=empty).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:homologous_to',_),label='H').


user:graphviz_ontol_param(edge(_,_,'BFO:0000050',_),override(label='PO')).
user:graphviz_ontol_param(edge(_,_,'BFO:0000050',_),color=blue).

