:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(bioprolog_util)).

% blip -r go -r worm_phenotype -f gzip/go_assoc -i ~/cvs/go/gene-associations/gene_association.wb.gz -u query_worm findall go2ph/2

init:-
        load_bioresource(worm_phenotype),
        load_bioresource(go).

go2ph(Go-GoN,Ph-PhN,ET):-
        evidence_with(E,W),
        sub_atom(W,_,_,_,'WB:WBPh'),
        atom_concat('WB:',Ph,W),
        class(Ph,PhN),
        curation_evidence(A,E),
        curation_statement(A,_,_,Go),
        class(Go,GoN),
        evidence_type(E,ET).

% blip -r worm_phenotype_xp -r go -r worm_phenotype -f gzip/go_assoc -i ~/cvs/go/gene-associations/gene_association.wb.gz -u query_worm findall go2ph_xp/3
% blip -r pato -r worm_anatomy -r worm_phenotype_xp -r go -r worm_phenotype -f gzip/go_assoc -i ~/cvs/go/gene-associations/gene_association.wb.gz -u query_worm findall go2ph_xp/3 
go2ph_xp(Go-GoN,Ph-PhN,XPNs):-
        evidence_with(E,W),
        sub_atom(W,_,_,_,'WB:WBPh'),
        atom_concat('WB:',Ph,W),
        class(Ph,PhN),
        curation_evidence(A,E),
        curation_statement(A,_,_,Go),
        class(Go,GoN),
        solutions(XPN,(class_cdef(Ph,CDef),cdef_label(CDef,XPN)),XPNs).

% perl -ne 'print "$2\t$1\n" if /(GO:\d+).*(WBPhenotype\d+)/' worm-go-assoc-imp.txt  > ph2go.xref
% blip -r worm_pa -i ~/cvs/go/gene-associations/gene_association.wb.gz -u query_worm -r go findall new_prediction/5 
% blip -debug query -r worm_phenotype -i ph2go_xref.pro -r worm_pa -r worm_ga -u query_worm -r go findall new_prediction/5
new_prediction(Gene,Go,GoN,Ph,PhN):-
        curation_statement(_PA,Gene,_,Ph),
        xref(Ph,Go),
        %debug(query,'checking ~w ~w ~w',[Gene,Ph,Go]),
        \+ ((curation_statement(_,Gene,_,GoC),
             parentRT(GoC,Go))),
        class(Go,GoN),
        class(Ph,PhN).



