:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(node(_),style=filled).
user:graphviz_ontol_param(node(X),fillcolor=lemonchiffon):- ontol_db:inst_sv(X,has_rank,'NCBITaxon:order').
user:graphviz_ontol_param(node(X),fillcolor=green):- ontol_db:inst_sv(X,has_rank,'NCBITaxon:phylum').
user:graphviz_ontol_param(node(X),fillcolor=peru):- ontol_db:inst_sv(X,has_rank,'NCBITaxon:class').
user:graphviz_ontol_param(node(X),fillcolor=yellow):- ontol_db:inst_sv(X,has_rank,'NCBITaxon:kingdom').
user:graphviz_ontol_param(node(X),fillcolor=hotpink):- ontol_db:inst_sv(X,has_rank,'NCBITaxon:species').
user:graphviz_ontol_param(node(X),fillcolor=steelblue2):- ontol_db:inst_sv(X,has_rank,'NCBITaxon:superkingdom').

