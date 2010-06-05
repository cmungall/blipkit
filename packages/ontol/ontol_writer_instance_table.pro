/* -*- Mode: Prolog -*- */


:- module(ontol_writer_instance_table,[
                                       write_instance_table/1
                                      ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(graphviz)).

% TODO - stdout
io:write_all(instance_table,F,_):-
        write_instance_table(F).
        
write_instance_table(_F):-
        solutions(R,inst_sv(_,R,_),Rs0),
        sort(Rs0,Rs),
        writecols([id|Rs]),nl,
        forall(inst_of(Inst,_),
               write_inst_row(Inst,Rs)).

write_inst_row(Inst,Cols):-
        findall(Val,(member(Col,Cols),inst_col_val(Inst,Col,Val)),Vals),
        writecols([Inst|Vals]),
        nl.

inst_col_val(Inst,Col,ValAtom):-
        findall(Val,inst_sv(Inst,Col,Val),Vals),
        concat_atom(Vals,'; ',ValAtom).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
