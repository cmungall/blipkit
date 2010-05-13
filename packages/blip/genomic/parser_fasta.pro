/* -*- Mode: Prolog -*- */


:- module(parser_fasta,
          [
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(phylo_db)).

io:parse_stream(fasta,IO):-
        load_seqs(IO).

load_seqs(IO):-
        repeat,
        (   read_block(IO,Block)
        ->  parse_block(Block),
            fail
        ;  true).

parse_stream_to_seq(IO,Seq):-
        read_line_to_codes(IO,Header),
        read_line_to_codes(IO,Codes,Tail),
        atom_codes(SeqAtom,Codes),
        parse_stream_to_seq([SeqAtom],IO,Tail).

parse_stream_to_seq(Atoms,IO,Tail):-
        read_line_to_codes
parse_stream_to_seqs(IO,Seqs):-
        read_line_to_codes(IO,Block,Tail),
        

read_block(IO,Block):-
        read_line_to_codes(IO,Block,Tail),
        read_block(IO,Block,Tail).

% (+IO,[+Block|?Tail],?Tail)
read_block(_IO,"\r\n",[]) :- !.
read_block(_IO,"\n",[]) :- !.
read_block(_IO,"",[]) :- !.
read_block(IO,_Block,Tail):-
        read_line_to_codes(IO,Tail,NewTail),
        read_block(IO,Tail,NewTail).

parse_block(Codes):-
        atom_codes(A,Codes),
        write(a=A),write('xxx'),nl,
        stanza(stanza(Type,Lines),Codes,[]),
        !.
parse_block(_Codes):-
        writeln(uhoh).

stanza( stanza(Type,Lines) ) -->
        "[",
        word(Type),
        "]",
        newline,
        {writeln(w=Type)},
        tagvals(Lines).

lines([]) --> [].
lines([Line|Lines]) --> line(Line),lines(Lines).

line(Cs) --> not_newlines(Cs).

tagvals([TV|TVs]) --> tagval(TV),{writeln(tv=TV/x)},newline,tagvals(TVs).
tagvals([]) --> [].

tagval([id=ID]) --> "id:",ws_star,not_ws_atom(ID).
tagval([name=N]) --> "name:",ws_star,basic_val(N).
tagval([namespace=N]) --> "namespace:",ws_star,basic_val(N).
tagval([def=N|Xs]) --> "def:",ws_star,quoted_val(N),xrefs(Xs).

basic_val(V) --> not_newlines(Cs), {atom_codes(V,Cs)}.
quoted_val(V) --> """",!,not_dquotes(Codes),"""",{atom_codes(V,Codes)}.

opt_comment --> "!",!,not_newlines.
opt_comment --> [].

vals([V]) --> val(V).
vals([V|Vs]) --> val(V),vals(Vs).

val(V) --> not_ws(Codes),{atom_codes(V,Codes)}.


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test_fasta)=
      ((true,load_biofile(fasta_test,'test.fasta')))/[]).
        
unittest(test(load_fasta_file,
             [_=load(test_fasta)],
            (   ensure_loaded(bio(ontol_db))),
            (1=24))).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2005/08/09 16:11:43 $
  @license LGPL

  
  ---+ Name
  ---++ parser_fasta
- 

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).

  % show height of all trees in file
  demo:-
    load_biofile(fasta,'test.fasta'),
  ==

  ---+ Description


  */