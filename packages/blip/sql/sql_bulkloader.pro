:- module(sql_bulkloader,[]).

bulkload_module(Mod,DB,Opts) :-
        tmp_file(db,Dir),
        write_dump(Dir,Mod,Opts),
        bulkload_command(DB,Opts,Dir,Cmd),
        shell(Cmd).

write_dump(Dir,Mod,Opts) :-
        forall(datapred(Mod,Pred),
               write_dump_pred(Dir,Mod,Pred,Opts)).

write_dump_pred(Dir,Mod,Pred,Opts) :-
        write_ddl(Dir,Mod,Pred,Opts),
        pred_to_unground_term(Pred,PredTerm),
        sformat('~w/~w.txt',[Dir,Pred]),
        findall(PredTerm,(Mod:PredTerm,
                          write_rows(Fmt,Mod:PredTerm)),
        

        
/** <module> 

  ---+ Synopsis

==
:- use_module(bio(sql_bulkloader)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
