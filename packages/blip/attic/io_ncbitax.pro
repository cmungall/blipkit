/* -*- Mode: Prolog -*- */


:- module(io_ncbitax,
          [taxname/2,
           taxparent/2,
           load_taxnodes/1,
           load_taxnames/1
          ]).
:- use_module(bio(bioprolog_util),[load_factfile/1]).

%% taxname(?ID,?Name) nd
%  scientific name
taxname(ID,N):-
        taxname(ID,N,_,'scientific name').
%% taxparent(?ID,?ParentID) nd
%% taxparent(+ID,?ParentID) d
taxparent(ID,PID):-
        taxnode(ID,PID,_Rank,_EmblCode,_DivID,_InhDiv,_GCID,_InhGC,_MitoGCID,_GBHide,_STHide,_Comments),
        ID \= 1.                % why do ncbi create a cyclic link 1->1???
taxparent(ID,PID):-
        taxnode(ID,PID,_Rank,_EmblCode,_DivID,_InhDiv,_GCID,_InhGC,_MitoGCID,_GBHide,_STHide,_Comments,_UnknownColumn),
        ID \= 1.                % why do ncbi create a cyclic link 1->1???

%% load_taxnodes(+File)
%% load_taxnames(+File)
%   loads a ncbitax file into the in-memory database
%
%  
load_taxnodes(File) :-
        load_ncbi(File,taxnode).
load_taxnames(File) :-
        load_ncbi(File,taxname).
load_ncbi(FileIn,Type) :-
        (user:bioresource(FileIn,P)
        ->  expand_file_search_path(P,File)
        ;   File=FileIn),
	file_name_extension(Base, _Ext, File),
	file_name_extension(Base, pro, PrologFile),
	file_name_extension(Base, 'pro.tmp', TmpFile),
	(   exists_file(PrologFile),
	    time_file(PrologFile, PrologTime),
	    time_file(File, Time),
	    PrologTime >= Time
	->  load_factfile(PrologFile)
	;   access_file(PrologFile, write)
	->  convert_and_load(File,PrologFile,TmpFile,Type)
	;   throw(cannot_write_to(PrologFile))
        ).

% todo: allow custom directory
convert_and_load(File,PrologFile,TmpFile,Type):-
        (   access_file(PrologFile, write)
        ->  true
        ;   throw(cannot_write_to(PrologFile))),
        concat_atom([tbl2p,'-p',Type,'-d','''\\s*\\|\\s*''',File,'>',TmpFile,
                     '&&',mv,TmpFile,PrologFile],
                    ' ',
                    Cmd),
        (   shell(Cmd,0)
        ->  true
        ;   throw(problem_executing(Cmd))),
        load_factfile(PrologFile).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/05/14 19:04:05 $
  @license LGPL


  ==
  :- use_module(bio(io_ncbitax)).
  :- load_taxnodes('nodes.dmp').
  :- load_taxnames('names.dmp').
  ==
  
  download ftp.ncbi.nih.gov/pub/taxonomy/

  input/output module for NCBITAX files

  relies on tbl2p perl script in bin

  other than that, this module is relatively simple and
  self-contained.

  */