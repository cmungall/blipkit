/* -*- Mode: Prolog -*- */


:- module(seqanalysis_db,
          [
           % intensional predicates
           result/4,
           result_param/3,
           result_statistic/3,
           hit/5,
           hsp/14,
           hsp_cigar/2,
           
           % extensional predicates
           hsp_hit/2,
           hit_acc/2,
           result_program/2,
           hit_result/2,
           project_point_over_hsp/3,

           % util
           parse_cigar/2
          ]).


% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(bioseq)).

%%              result(ID,QName,ProgName,QDesc)
%
% The result of running an analysis program over a query sequence, typically against a sequence database
% Example: a blast result
%
% @param ID identifies the result
% @param QName name/ID of the query sequence
% @param ProgName name of the program used
% @param Description of the query sequence
:- extensional(result/4).

%%              result_param(ID,Key,Val)
%
% tag-value parameters. Used to configure the program.
%
% @param Key parameter name. Examples (blast): expect | gapext | matrix | allowgaps
:- extensional(result_param/3).
        
%%              result_statistic(ID,Key,Bal)
%
% tag-value statistics. From the results of running the analysis
%
% @param Key statistic name. Examples (blast): lambda | entropy | kappa_gapped 
:- extensional(result_statistic/3).
        
%%              hit(ID,ResultID,HName,HAccession,HDesc)
% Example:
% =|hit('BLASTN-na1-na2.fa -na2', 'BLASTN-na1-na2.fa ', na2, na2, 'cp-of-na1-w-50bp-missing')|=
:- extensional(hit/5).

%%              hsp(ID,HitID,PctIdent,Length,MismatchCount,GapCount,QBeg,QEnd,QFrame,HBeg,HEnd,HFrame,E,BitScore)
% Example:
% =|hsp('BLASTN-na1-na2.fa -na2-2', 'BLASTN-na1-na2.fa -na2', 100.0, 12, 0, 0, 75, 87, 0, 85, 73, 0, 0.011, 24.3)|=
:- extensional(hsp/14).

%%              hsp_cigar(HspID,CigarLine)
%
% Example:
% =| hsp_cigar('BLASTN-na1-na2.fa -na2-0', '261M') |=
%
% @param CigarLine See http://www.ensembl.org/common/Help/Glossary
:- extensional(hsp_cigar/2).


% --

% VIEW PREDICATES

%% result_program(?ResultID,?Prog)
%   links results with program name
%   See result/2
result_program(ResultID,Prog):- result(ResultID,_,Prog,_).

%% hit_result(?HitID,?ResultID)
%   links hit with parent result
%   see hit/5
hit_result(HitID,ResultID):- hit(HitID,ResultID,_,_,_).

%% hit_acc(?HitID,?Acc)
%   shorthand for hit/5
hit_acc(HitID,Acc):- hit(HitID,_,Acc,_,_).

%% hsp_hit(?HspID,?HitID)
%   shorthand for hsp/14
%  
hsp_hit(HspID,HitID):- 
        hsp(HspID,HitID,_,_,_,_,_,_,_,_,_,_,_,_).

%% hsp_e(?HspID,?E)
% shorthand for hsp/14
hsp_e(ID,E):-
        hsp(ID,_HitID,_PctIdent,_Length,_MismatchCount,_GapCount,_QBeg,_QEnd,_QFrame,_HBeg,_HEnd,_HFrame,E,_BitScore).

%% parse_cigar(+CigarLine,?Transitions:list)
%
% parses a compact CIGAR atom into a list of transition states
%
% Example: =|parse_cigar('2M1D3M2D2M',[match(2), delete(1), match(3), delete(2), match(2)])|=
%
%  @param CigarLine atom following standard cigar syntax
%  @param Transitions list of match(Num) | insert(Num) | delete(Num)
parse_cigar(CigarLine,Transitions):-
        atom_codes(CigarLine,Codes),
        parse_cigar_codes(Codes,Transitions,[]).

% (+,?,+)
parse_cigar_codes([Code|Codes],Transitions,Buf):-
        code_type(Code,digit),
        !,
        parse_cigar_codes(Codes,Transitions,[Code|Buf]).
parse_cigar_codes([Code|Codes],[CigarElt|Transitions],Buf):-
        cigar_code_trans(Code,Trans),
        !,
        (   Buf=[]
        ->  Num=1
        ;   atom_codes(NumAsAtom,Buf),
            atom_number(NumAsAtom,Num)),
        CigarElt =.. [Trans,Num],
        parse_cigar_codes(Codes,Transitions,[]).
parse_cigar_codes([],[],[]). % Buf must be empty

% maps a character code in a cigar line to the transition name
cigar_code_trans(0'M,match).
cigar_code_trans(0'I,insert).
cigar_code_trans(0'D,delete).

%% program_multipliers(?Program,?QueryMultipler,?HitMultiplier)
%
% ==
%           Q  H  QM HM Frames
% tblastx:  na na  3  3 qh
% tblastn:  aa na  1  3 h
%  blastx:  na aa  3  1 q
%  blastn:  na na  1  1
%  blastp:  aa aa  1  1
% ==
program_multipliers(tblastx,3,3).
program_multipliers(tblastn,1,3).
program_multipliers(blastx,3,1).
program_multipliers(blastn,1,1).
program_multipliers(blastp,3,3).

% maps a HSP ID to a compound term representing that HSP
hsp_term(HspID,hsp(QBeg,QEnd,QFrame,HBeg,HEnd,HFrame,QM,HM,Transitions)):-
        hsp(HspID,HitID,_,_,_,_,QBeg,QEnd,QFrame,HBeg,HEnd,HFrame,_,_),
        hsp_cigar(HspID,Cigar),
        parse_cigar(Cigar,Transitions),
        hit_result(HitID,ResultID),
        result_program(ResultID,Prog),
        % ...
        downcase_atom(Prog,ProgLc),
        program_multipliers(ProgLc,QM,HM).

% what about ranges that span gaps?
project_coords(QBeg,QEnd,Hsp,HBeg,HEnd):-
        project_point_over_hsp(QBeg,Hsp,HBeg),
        project_point_over_hsp(QEnd,Hsp,HEnd).

%% project_point_over_hsp(+QP:int,?Hsp,?HP:int)
%
% given an interbase position, project this from
% a query sequence to a hit sequence.
%
% note: this assumes that the query sequence and
% hit sequence have been aligned AND the results
% have been parsed. This module currently does
% not provide support for the first step. For the
% second step, you can parse blast format using
% load_biofile/2 using blast as the first argument
% (uses bioperl)
%
% note: if Hsp is unbound, make sure it is for
% the same query sequence as is intended for QP
%
%  will not succeed if QP is not within QBeg..QEnd
%
%  will succeed if QP is within QBeg..QEnd, HP will be unified
%  with the corresponding position on hit sequence
%
%  If QP projects to a gap in the hit seq, then HP will be
%  unified with the compound term =|between(HBeg,HEnd)|=
%  
%  If QM=1 and HM=3 then input QP may be a float (tblastn)
%  
%  If QM=3 and HM=1 then output HP may be a float (blastx)
project_point_over_hsp(QP,HspID,HP):-
        hsp_term(HspID,Hsp),
        project_point_over_hsp_via_hspterm(QP,Hsp,HP).

% project_point_over_hsp_via_hspterm(+QP,+HspTerm,?HP) is semidet
%
% assuming QP on + strand: we want to keep shifting QP down, one match
% block or delete block at a time; at the same time we shift HP up
% (the idea being that when QP reaches zero HP will be at the projected
%  position)
% if we 'overrun' (ie projection is in middle of match block) then we
% add to HP the current value of QP
%
% the above assumes na2na match; if one or either sequence is aligned as
% aa then we add 3 at a time - this is M, the multiplier
project_point_over_hsp_via_hspterm(P,Hsp,HP):-
        Hsp = hsp(QBeg,QEnd,_QFrame,HBeg,HEnd,_HFrame,QM,HM,Transitions),
        (QBeg > QEnd -> QM2 is -QM ; QM2=QM ),
        (HBeg > HEnd -> HM2 is -HM ; HM2=HM ),
        project_point_over_hsp_via_hspterm1(P,hsp(QBeg,QEnd,HBeg,HEnd,QM2,HM2,Transitions),HP).

% QM and HM must be in agreement with strand
project_point_over_hsp_via_hspterm1(P,Hsp,HP):-
        Hsp = hsp(QBeg,QEnd,HBeg,HEnd,QM,HM,Transitions),
        in_hsp_boundary(P,QBeg,QEnd),
        !,
        shift_point(P,HBeg,QBeg,QEnd,QM,HM,Transitions,HP),
        (in_hsp_boundary(HP,HBeg,HEnd)
        ->  true
        ;   throw(assertion_error(P,Hsp,HP,HEnd))).

% shift QP until =< 0
%  if QP ends up <0 AND we are in a match state
%  then ignore whatever delta is (ie the length of this match)
%  and just adjust HP to whatever the equivalent position is in
%  this block
shift_point(QP,HP,QBeg,QEnd,QM,HM,[T|Transitions],HPFinal):-
        transition_delta(T,QM,HM,QDelta,HDelta),
        QPNew is QP - QDelta,
        HPNew is HP + HDelta,
        (QM > 0
        ->  QPRemaining is QPNew-QBeg
        ;   QPRemaining is QBeg-QPNew),
        (QPRemaining = 0
        ->  HPFinal is HPNew    % borderline
        ;   (QPRemaining < 0
            ->  (T=match(_Delta)
                ->  HPFinal is HPNew + QPRemaining * (HM / abs(QM))
                ;   HPFinal = between(HP,HPNew))
            ;   shift_point(QPNew,HPNew,QBeg,QEnd,QM,HM,Transitions,HPFinal))).

% (+,+,+,?,?) d
transition_delta(match(Delta),QM,HM,QDelta,HDelta):-
        QDelta is Delta * QM,
        HDelta is Delta * HM.
transition_delta(insert(Delta),QM,_HM,QDelta,0):-
        QDelta is Delta * QM.
transition_delta(delete(Delta),_QM,HM,0,HDelta):-
        HDelta is Delta * HM.

% check if point is within boundary
% (points are between bases)
in_hsp_boundary(P,QBeg,QEnd):-
        P >= QBeg,
        P =< QEnd.
in_hsp_boundary(P,QBeg,QEnd):-
        P =< QBeg,
        P >= QEnd.

% RUNNING ANALYSES : EXPERIMENTAL

run_program(Cmd,Out,Err):-
        tmp_file(out,OutFile),
        tmp_file(err,ErrFile),
        concat_atom([Cmd,'>',OutFile,'>&',ErrFile]),
        read_file_to_atom(OutFile,Out),
        read_file_to_atom(ErrFile,Err).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(mixed)=
      (   assert(user:dynamic_db(seqanalysis_db)),
          load_biofile(fasta,'mixed_test.fasta'))/
      []).
        
unittest(test(cigars,
            [_=load(mixed)],
            (   %assert(user:dynamic_db(seqanalysis_db)),
                load_biofile(blast,'out.blastn.blast'),
                findall(AlignTerm,
                        (
                          hsp_cigar(_HspID,Cigar),
                          parse_cigar(Cigar,AlignTerm)),
                        AlignTerms)),
            (member([match(21)],AlignTerms)))).

unittest(test(blast_cigars,
            [_=load(mixed)],
            (   ensure_loaded(bio(bioseq)),
                ensure_loaded(bio(fasta_db)),
                forall(member((QSeqID,HSeqID,BlastFile),
                              [(na1,na2,'out.blastn.blast'),
                               (   na1,na2,'out.tblastx.blast')]),
                       (
                        fastaseq(QSeqID,_,QSeq),
                        atom_length(QSeq,QSeqLen),
                        load_biofile(blast,BlastFile),
                        findall(Hsp-QP,
                                (   
                                    between(0,QSeqLen,QP),
                                    project_point_over_hsp(QP,Hsp,HP),
                                    hsp_hit(Hsp,Hit),
                                    hit_acc(Hit,HSeqID),
                                    fastaseq(HSeqID,_,HSeq),
                                    sub_sequence(QSeq,from(QP,1),QChar),
                                    sub_sequence(HSeq,from(HP,1),HChar),
                                    (   QChar \= HChar
                                    ->  Note='**MISMATCH**'
                                    ;   Note=''),
                                    format('Project ~w -> ~w [~w->~w] ~w over ~w ~n',[QP,HP,QChar,HChar,Note,Hsp]),
                                    QChar \= HChar
                                ),
                                NonMatchingHsps),
                        format('Bad HSPs=~w~n',[NonMatchingHsps])))),
            NonMatchingHsps=[])).

% DEPRECATED:
load_test_biofile(Fmt,File):-
        concat_atom(['lib/Test/data/',File],File2),
        load_biofile(Fmt,File2).

utest(foo):-
        load_test_biofile(fasta,'mixed_test.fasta'),
        forall(member((QSeqID,HSeqID,BlastFile),
                      [(na1,na2,'out.blastn.blast'),
                       (na1,na2,'out.tblastx.blast')]),
               (
                 user:fastaseq(QSeqID,_,QSeq),
                 atom_length(QSeq,QSeqLen),
                 load_test_biofile(blast,BlastFile),
                 findall(HP,
                         (
                           between(0,QSeqLen,QP),
                           project_point_over_hsp(QP,Hsp,HP),
                           hsp_hit(Hsp,Hit),
                           hit_acc(Hit,HSeqID),
                           user:fastaseq(HSeqID,_,HSeq),
                           sub_sequence(QSeq,from(QP,1),QChar),
                           sub_sequence(HSeq,from(HP,1),HChar),
                           writeln(project(QP,HP,via(Hsp),QChar-HChar))),
                         _)
               )
              ).
        
      
/** <module> computational results schema

  ---+ Synopsis

  ==
  :- use_module(bio(seqanalysis_db))

  demo:-
        load_biofile(blast,'out.blastn.blast'),
        solutions(Hit,(hsp_e(HSP,E), E < 1e20, hit_hsp(Hit,HSP)),Hits),
        forall((member(Hit,Hits),hit(Hit,_,_,HitAcc,HitDesc)),
               format('Significant Hit: ~w ~w',[HitAcc,HitDesc])).
  ==

  ---+ Description

  This module is a datalog schema for the representation of the results of sequence analysis programs such as BLAST.

  It also includes predicates for coordinate transformation based on sequence alignment

  ---++ Coordinate System
  
  All coordinates are interbase. Frames are +/- 0,1,2 

  ---++ CIGAR lines (Compact Idiosyncratic Gapped Alignment Report)

  Defines the sequence of matches/mismatches and deletions (or gaps). The cigar line defines the sequence of matches/mismatches and deletions (or gaps).

  For example, this cigar line =|2MD3M2D2M|= will mean that the alignment contains 2 matches/mismatches, 1 deletion (number 1 is omitted in order to save some space), 3 matches/mismatches, 2 deletions and 2 matches/mismatches.

  If the original sequence is =|AACGCTT|=

  The aligned sequence will be:
  
  cigar line: 2MD3M2D2M

  ==
  MMDMMMDDMM
  AA-CGC--TT
  ==

  If we use parse_cigar/2 we get =|[match(2), delete(1), match(3), delete(2), match(2)]|=

  ---++ Coordinate transformation

  project_point_over_hsp/3
  
  */