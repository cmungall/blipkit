/* -*- Mode: Prolog -*- */


:- module(bioseq,
          [sub_sequence/3,
           revcomp/2,
           translate_dna/2,
           translate_dna/3,
           index_all_seqs_to_file/2]).

:- use_module(bio(gencode),[gencode_organism/2,codon_aa/3]).
:- use_module(bio(suffixtree)).
:- use_module(bio(dbmeta)).
:- use_module(bio(fasta_db)).
:- use_module(bio(iupac), [compl_symbol/2]).
:- dynamic seqidx/4.

%%  sub_sequence(+Seq,+ExtractTerm,?SubSeq) is semidet
%
%   if Len<0 or End<Beg, the extracted sequence will be reverse complemented using revcomp/2
%
%   @param Seq  input sequence atom
%   @param ExtractTerm  from(Pos,Len) | range(Beg,End) | revcomp(ExtractTerm) | rangeset(ExtractTerms)
%    Beg and End are interbase coordinates (counts positions between bases, origin zero)
%   @param SubSeq  extracted sequence atom
sub_sequence(Seq,from(From,Len),SubSeq):-
        Len >= 0,
        !,
        sub_atom(Seq,From,Len,_,SubSeq).
sub_sequence(Seq,from(From,Len),SubSeqRev):-
        Len < 0,                % assertion
        !,
        AbsLen is abs(Len),
        sub_atom(Seq,From,AbsLen,_,SubSeq),
        revcomp(SubSeq,SubSeqRev).
sub_sequence(Seq,range(Beg,End),SubSeq):-
        Beg =< End,
        !,
        Len is End-Beg,
        sub_atom(Seq,Beg,Len,_,SubSeq).
sub_sequence(Seq,range(Beg,End),SubSeqRev):-
        Beg > End,              % assertion
        !,
        Len is Beg-End,
        sub_atom(Seq,End,Len,_,SubSeq),
        revcomp(SubSeq,SubSeqRev).
sub_sequence(Seq,rangeset(L),FullSubSeq):-
        maplist(sub_sequence,Seq,L,SubSeqs),
        concat_atom(SubSeqs,FullSubSeq).
sub_sequence(Seq,revcomp(R),RevSubSeq):-
        sub_sequence(Seq,R,SubSeq),
        revcomp(SubSeq,RevSubSeq).
sub_sequence(_Seq,Term,_SubSeqRev):-
        throw(error(illegal_extract_term(Term))).
%% translate_dna(+DnaSeq,?AASeq)
%   as translate_dna/3, uses default eukaryotic genetic code (1)
translate_dna(S,Sr):-
        translate_dna(S,Sr,1,0).

%% translate_dna(+DnaSeq,?AASeq,+GeneticCode)
%
%   translates a dna sequence (represented as an atom) to an amino acid
%   sequence. excludes stop codon and everything after. See codon_aa/3
%
%   @param GeneticCode see gencode_organism/2
translate_dna(S,Sr,C):-
        translate_dna(S,Sr,C,0).

%% translate_dna(+DnaSeq,?AASeq,+GeneticCode,?IsFullTranslation)
%   translates a dna sequence (represented as an atom) to an amino acid
%sequence using the genetic code with ncbi ID GeneticCode (see the gencode
%module and gencode_organism/2). Is IsFullTranslation is non-zero, will translate past the stop codon  
translate_dna(S,Sr,Code,Full):-
        upcase_atom(S,Su),
        atom_chars(Su,RL),
        translate_dna_chars(RL,RLr,Code,Full),
        atom_chars(Sr,RLr).

%% translate_dna_full(+DnaSeq,?AASeq)
%   as translate_dna/2, but carries on past stop codon to the end of DnaSeq
translate_dna_full(S,Sr):-
        translate_dna(S,Sr,1,1).

%% translate_dna_full(+DnaSeq,?AASeq,+GeneticCode)
%   as translate_dna/3, but carries on past stop codon to the end of DnaSeq
translate_dna_full(S,Sr,C):-
        translate_dna(S,Sr,C,1).


% (+,?,+Code,+Full)
% translates a list of na symbols
translate_dna_chars([],[],_,_).
translate_dna_chars([_],[],_,_).
translate_dna_chars([_,_],[],_,_). % allow partial?
translate_dna_chars([Rx,Ry,Rz|Rs],NewAAs,Code,Full):-
        concat_atom([Rx,Ry,Rz],Codon),
        codon_aa(Code,Codon,AA),
        (   (AA='*',Full=0)
        ->  NewAAs=[]
        ;   translate_dna_chars(Rs,AAs,Code,Full),
            NewAAs=[AA|AAs]).

%% revcomp(+DnaSeq,?RevCompedSeq)
%
%  reverse-complements a DNA sequence, represented as an atom
%
%  this implementation is pure-prolog. Consider the squid module for
%faster revcomping
% TODO: check if we can get this faster by using codes not chars
revcomp(S,Sr):-
        atom_chars(S,RL),
        revcomp_chars(RL,RLr,[]),
        atom_chars(Sr,RLr).
revcomp_chars([],L,L).
revcomp_chars([R|RL],RLrev,BL):-
        compl_symbol(R,R2),
        revcomp_chars(RL,RLrev,[R2|BL]).

seq_codon_counts(Seq,CodonCounts) :-
        seq_codon_counts(Seq,0,[],CodonCounts).

seq_codon_counts(Seq,N,CIn,COut) :-
        sub_atom(Seq,N,3,_,Codon),
        !,
        symbol_inc_counts(Codon,CIn,CIn2),
        N2 is N+1,
        seq_codon_counts(Seq,N2,CIn2,COut).
seq_codon_counts(_,_,C,C).

symbol_inc_counts(Sym,L,[Sym-N2|L2]):-
        select(Sym-N,L,L2),
        !,
        N2 is N+1.
symbol_inc_counts(Sym,L,[Sym-1|L]).


%% kmers(?Length,?Nucs)
% generates/tests if Nucs is a list of valid dna nucleotide symbols of length Length
kmers(0,[]):- !.
kmers(Length,[N|Nucs]):-
        Length>0,
        dnasymbol(N),
        LengthMinus1 is Length-1,
        kmers(LengthMinus1,Nucs).

stree_insert_seq(Seq,ST,NewST):-
        atom_chars(Seq,SymL),
        stree_insert(SymL,ST,NewST).
stree_insert_seqs(Seqs,ST,NewST):-
        setof(SymL,(member(Seq,Seqs),atom_chars(Seq,SymL)),SymLs),
        stree_insert_list(SymLs,ST,NewST).
stree_has_seq(Seq,ST):-
        (var(Seq)
        ->  stree_has(SymL,ST),
            atom_chars(Seq,SymL)
        ;   atom_chars(Seq,SymL),
            stree_has_seq(SymL,ST)).

%% index_all_seqs(+Siz)
%  det
%   word-index all bioseq/3 facts.
%  stored in compiled seqidx/4 predicate
%  
%  EXPERIMENTAL
index_all_seqs(Siz):-
        calc_seqidx(ID,I,Siz,Subseq),
        assert(seqidx(ID,I,Siz,Subseq)),
        fail.
index_all_seqs(_):-
        compile_predicates([seqidx/4]).

%% index_all_seqs_to_file(+F,+Siz)
%  det
%   word-index all bioseq/3 facts and write index to file F
%
%  EXPERIMENTAL
index_all_seqs_to_file(F,Siz):-
        writeln(writing_to(F)),
        tell(F),
        wseqidx(Siz),
        told.

wseqidx(Siz):-
        calc_seqidx(ID,I,Siz,Subseq),
        writeq(seqidx(Subseq,I,ID,Siz)),
        write('.'),
        nl,
        fail.
wseqidx(_).

calc_seqidx(ID,I,Siz,Subseq):-
        fastaseq(ID,_,S),
        sub_atom(S,I,Siz,_,Subseq).

/** <module> utility module for biological sequences


  ---+ Synopsis

  ==
  :- use_module(bio(bioseq)).

  % demo: reverse complement a DNA sequence
  demo:-
    revcomp('TCAG',Seq1),
    format('revcomp(TCAG) = ~w~n',[Seq1]),
    translate_dna(atg,Seq2),
    format('translate(ATG) = ~w~n',[Seq2]).
  ==
  
  ---+ Description

  Standard sequence transformation operations

  these predicates are native prolog - consider using the squid module
in ext for speed

  @author Chris Mungall
  @license LGPL

  */
