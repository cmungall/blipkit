:- module(seqfeature_owlmap_node_as_individual_featureloc_basic,
        []).

:- use_module(bio(seqfeature_db)).
:- multifile seqfeature_owlmap_node_as_individual:fact/2.
seqfeature_owlmap_node_as_individual:fact(F,Fact):-
        featureloc(F,Src,Beg,End,Strand),
        Facts=[
               value('test:relative_to',Src),
               value('test:has_start',literal(type('http://www.w3.org/2001/XMLSchema#int',Beg))),
               value('test:has_end',literal(type('http://www.w3.org/2001/XMLSchema#int',End))),
               value('test:has_strand',literal(Strand))],
        member(Fact,Facts).

