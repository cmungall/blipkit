:- module(aminoacid_chebi,[aminoacid_chebi/3]).
aminoacid_chebi('A', 'CHEBI:16977', 'L-alanine').
aminoacid_chebi('C', 'CHEBI:17561', 'L-cysteine').
aminoacid_chebi('D', 'CHEBI:17053', 'L-aspartic acid').
aminoacid_chebi('E', 'CHEBI:16015', 'L-glutamic acid').
aminoacid_chebi('F', 'CHEBI:17295', 'L-phenylalanine').
aminoacid_chebi('G', 'CHEBI:15428', 'glycine').
aminoacid_chebi('H', 'CHEBI:15971', 'L-histidine').
aminoacid_chebi('I', 'CHEBI:17191', 'L-isoleucine').
aminoacid_chebi('K', 'CHEBI:18019', 'L-lysine').
aminoacid_chebi('L', 'CHEBI:15603', 'L-leucine').
aminoacid_chebi('M', 'CHEBI:16643', 'L-methionine').
aminoacid_chebi('N', 'CHEBI:17196', 'L-asparagine').
aminoacid_chebi('P', 'CHEBI:17203', 'L-proline').
aminoacid_chebi('Q', 'CHEBI:18050', 'L-glutamine').
aminoacid_chebi('R', 'CHEBI:16467', 'L-arginine').
aminoacid_chebi('S', 'CHEBI:17115', 'L-serine').
aminoacid_chebi('T', 'CHEBI:16857', 'L-threonine').
aminoacid_chebi('V', 'CHEBI:16414', 'L-valine').
aminoacid_chebi('W', 'CHEBI:16828', 'L-tryptophan').
aminoacid_chebi('Y', 'CHEBI:17895', 'L-tyrosine').

/*
solutions(AA,codon_aa(_,_,AA),AAs),solutions(s(AA,ID,N),(member(AA,AAs),synonym(ID,_ST,AA),class(ID,N),class(RID,'amino acids'),subclassT(ID,RID)),Sols),member(s(AA,ID,N),Sols),writeq(aminoacid_chebi(AA,ID,N)),writeln('.'),fail.

*/
