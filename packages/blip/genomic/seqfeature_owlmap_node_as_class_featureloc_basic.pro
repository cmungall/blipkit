:- module(seqfeature_owlmap_node_as_class_featureloc_basic,
        []).

:- use_module(bio(seqfeature_db)).
:- multifile seqfeature_owlmap_node_as_individual:fact/2.

owl_parser:equivalentSet([F,Expr]):- 
        featureloc(F,Src,Beg,End,Strand),
        Expr =
        intersectionOf(['test:Region',
                        restriction('test:relative_to',someValuesFrom(Src)),
                        restriction('test:has_start',value(literal(type('http://www.w3.org/2001/XMLSchema#integer',Beg)))),
                        restriction('test:has_end',value(literal(type('http://www.w3.org/2001/XMLSchema#integer',End)))),
                        restriction('test:has_strand',value(literal(type('http://www.w3.org/2001/XMLSchema#string',Strand))))]).



