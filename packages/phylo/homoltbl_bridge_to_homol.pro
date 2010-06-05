
:- module(homoltbl_bridge_to_homol,[]).
:- use_module(bio(homol_db),[]).
:- use_module(bio(seqfeature_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(bioprolog_util)).

% homoltbl does not have taxon for grouping
% homoltbl(Set,Tax,GeneID,Symbol,_,RefSeq)

homol_db:homologset(Set) :-
        solutions(Set1,homoltbl(Set1,_,_,_,_,_),Sets),
        member(Set,Sets).

%homol_db:homologset_member(Set,Member,Tax,Symbol):-
%        homoltbl(Set,Tax1,Member1,Symbol,_,_),
%        atom_concat('entrezgene:',Member1,Member),
%        atom_concat('NCBITax:',Tax1,Tax).

homol_db:homologset_member(Set,Member) :-
        homoltbl(Set,_,Member1,_Symbol,_,_),
        atom_concat('NCBI_Gene:',Member1,Member).

%homol_db:homologset_member_symbol(Set,Member,Symbol) :-
%        homoltbl(Set,_,Member1,Symbol,_,_),
%        atom_concat('NCBI_Gene:',Member1,Member).
metadata_db:member_symbol(Member,Symbol) :-
        homoltbl(_,_,Member1,Symbol,_,_),
        atom_concat('NCBI_Gene:',Member1,Member).

seqfeature_db:feature_organism(Member,Tax) :-
        homoltbl(_,Tax1,Member1,_,_,_),
        atom_concat('NCBI_Gene:',Member1,Member),
        atom_concat('NCBITaxon:',Tax1,Tax).

        
