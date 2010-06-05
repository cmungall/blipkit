/* -*- Mode: Prolog -*- */



:- module(static_analyzer,
          [prolog_files_to_fileinfos/2]).

:- use_module(bio(mode)).

:- mode prolog_files_to_fileinfos(+,?) is det.
prolog_files_to_fileinfos(Files,Parses):-
        maplist(prolog_file_to_fileinfo,Files,Parses).

prolog_file_to_fileinfo(File,parse(File,Parse)):-
        open(File,read,PlFileIn,[]),
        stream_to_fileinfo(PlFileIn,Parse),
        close(PlFileIn).

stream_to_fileinfo(IO,[terminfo(Term,Vars)|Terms]):-
        read_term(IO,Term,[module(static_analyzer),variable_names(Vars)]),
        Term\=end_of_file,
        !,
        stream_to_fileinfo(IO,Terms).
stream_to_fileinfo(_,[]).

fileinfos_to_preds(Fileinfos,Preds):-
        maplist(fileinfo_to_preds,Fileinfos,PredsNested),
        flatten(PredsNested,Preds).
fileinfo_to_preds(fileinfo(File,Terms),Preds):-
        maplist(term_to_metadata(in(File)),Terms,Preds).



fileinfo_module_export_list(fileinfo(File,Terms),Preds):-
        member(terminfo((:-module(Module,Preds)),_),Terms),
        !.

fileinfo_pred_metadata(fileinfo(File,Terms),Pred,MetaData):-
        findall(TagVal,
                (   member(TermInfo,Terms),
                    terminfo_pred_metadata_tagval(TermInfo,Pred,TagVal)),
                TagVals),
        MetaData=predinfo(Pred,TagVals).

terminfo_pred_metadata(terminfo(Term,Vars),PredName/Arity,TagVal):-
        Term=(:-mode(PredTerm is Mode)),
        functor(PredTerm,PredName,Arity),
        TagVal=(mode(PredTerm) is Mode).
terminfo_pred_metadata(terminfo(Term,Vars),PredName/Arity,TagVal):-
        Term=(Head :- _),
        functor(Head,PredName,Arity),
        Head=..[_|HeadArgs],
        forall(member(Arg,HeadArgs),
               var(Arg)),
        TagVal=(mode(PredTerm) is Mode).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(self,
            [],
            (   ensure_loaded(bio(static_analyzer)),
                %prolog_files_to_fileinfos(['../../sb_db.pro'],L),
                %prolog_files_to_fileinfos(['../../static_analyzer.pro'],L),
                prolog_files_to_fileinfos(['../../query_mediator.pro'],L),
                maplist(writeln,L),
                nl
                ),
            true)).




/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ static_analyzer
- 

  ---+ Synopsis

  ==
  :- use_module(bio(static_analyzer)).

  ==

  ---+ Description
  

**/