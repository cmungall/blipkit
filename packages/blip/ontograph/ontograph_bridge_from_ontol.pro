:- module(ontograph_bridge_from_ontol,[]).

:- use_module(bio(ontol_db)).

ontograph_db:node(Node):- class(Node,_) ; property(Node,_) ; inst_of(Node,_).
ontograph_db:node_label(Node,Label):- ( class(Node,Label) ; property(Node,Label) ; inst_of(Node,Label) ), Label \= ''.
ontograph_db:node_metatype(Node,class):- class(Node,_).
ontograph_db:node_metatype(Node,relation):- property(Node,_).
ontograph_db:node_metatype(Node,instance):- inst_of(Node,_).
ontograph_db:node_source(Node,Source):- belongs(Node,Source).

ontograph_db:link(Term,Node,Relation,Target,true,):- Term=restriction(Node,Relation,Target),Term.
ontograph_db:link(Node,is_a,Target):- subclass(Node,Target).
ontograph_db:link(Node,Relation,Target):- inst_rel(Node,Relation,Target).
ontograph_db:link(Node,xref,Target):- class_xref(Node,Target). % rdf:seeAlso

ontograph_db:node_tagval(Node,Relation,Target):- inst_sv(Node,Relation,Target).

ontograph_db:node_tagval(Node,synonym,Label):- synonym(Node,_,Label).
ontograph_db:node_tagval(node_tagval(Node,synonym,Label),scope,Scope):- synonym(Node,Scope,Label).

ontograph_db:node_tagval(Node,comment,Label):- class_comment(Node,Label).

ontograph_db:node_tagval(Node,definition,Label):- def(Node,Label).
ontograph_db:node_source(node_tagval(Node,definition,Label),Xref):- def(Node,Label),def_xref(Node,Xref).

ontograph_db:node_metaproperty(Node,reflexive):- is_reflexive(Node).
ontograph_db:node_metaproperty(Node,transitive):- is_transitive(Node).
ontograph_db:node_metaproperty(Node,symmetric):- is_symmetric(Node).
ontograph_db:node_metaproperty(Node,anti_symmetric):- is_anti_symmetric(Node).
ontograph_db:node_metaproperty(Node,anonymous):- is_anonymous(Node).
ontograph_db:node_metaproperty(Node,obsolete):- obsolete(Node,_).
ontograph_db:node_metaproperty(Node,obsolete):- obsolete_class(Node,_,_).






