:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).

gt2ncbigene(G,NcbiGeneID):-
        inst_of(G,'SO:0001027'),
        atom_concat('O',G1,G),
        concat_atom([OmimID|_],'.',G1),
        inst_rel(NcbiGeneID,'oboMetaModel:xref',OmimID).
        
