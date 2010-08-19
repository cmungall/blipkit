:- module(sql_bulkloader,
          [bulkload_pldata/4,
           bulkload_mysql/4,
           bulkload_pg/4,
           write_dump/1,
           write_dump/2,
           write_dump/3
           ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(bioprolog_util)).
:- use_module(sql_compiler).



%% bulkload_pldata(+Mod,+DB,Opts:list) is det
%
% bulkload data from a module (ie pl schema) into a SQL database
bulkload_pldata(Dir,Mod,DB,Opts) :-
        member(skip_dump,Opts),
        !,
        bulkload(Dir,Mod,DB,Opts).
bulkload_pldata(Dir,Mod,DB,Opts) :-
        write_dump(Dir,Mod,Opts),
        bulkload(Dir,Mod,DB,Opts).

bulkload_mysql(Dir,Mod,DB,Opts) :-
        bulkload_pldata(Dir,Mod,DB,[dbms(mysql)|Opts]).
bulkload_pg(Dir,Mod,DB,Opts) :-
        bulkload_pldata(Dir,Mod,DB,[dbms(pg)|Opts]).
        

bulkload(Dir,_Mod,DB,Opts) :-
        member(dbms(mysql),Opts),
        !,
        run('mysqladmin create ~w',[DB]),
        run('cat ~w/*.sql | mysql ~w',[Dir,DB]),
        run('mysqlimport -L ~w ~w/*.txt',[DB,Dir]).
bulkload(Dir,_Mod,DB,Opts) :-
        member(dbms(pg),Opts),
        !,
        run('createdb ~w',[DB]),
        run('cat ~w/*.sql | psql ~w',[Dir,DB]),
        run('pgimport ~w/*.txt | psql ~w',[Dir,DB]).

run(Fmt,Args) :-
        sformat(Cmd,Fmt,Args),
        debug(bulkload,'cmd: ~w',[Cmd]),
        shell(Cmd).

write_dump(Mod) :-
        write_dump('.',Mod,[]).
write_dump(Dir,Mod) :-
        write_dump(Dir,Mod,[]).


%% write_dump(+Dir,+Mod,+Opts:list) is det
%
% bulkload data from a module (ie pl schema) into a table
% ready for import via standard SQL bulk load commands
write_dump(Dir,Mods,Opts) :-
        is_list(Mods),
        !,
        forall(member(Mod,Mods),
               write_dump(Dir,Mod,Opts)).
write_dump(Dir,Mod,Opts) :-
        Dir\='.',
        Dir\='..',
        atom_concat('../',Dir,This),
        exists_directory(This),
        !,
        write_dump('.',Mod,Opts).
write_dump(Dir,Mod,Opts) :-
        \+   exists_directory(Dir),
        !,
        make_directory(Dir),
        write_dump(Dir,Mod,Opts).
write_dump(Dir,Mod,Opts) :-
        member(metadata(sql_compiler),Opts),
        !,
        forall(sql_compiler:relation(PN,A),
               write_dump_pred(Dir,Mod,PN/A,Opts)).
write_dump(Dir,Mod,Opts) :-
        forall(datapred(Mod,Pred),
               write_dump_pred(Dir,Mod,Pred,Opts)).

write_dump_pred(Dir,Mod,Pred,Opts) :-
        write_ddl(Dir,Mod,Pred,Opts),
        pred_to_unground_term(Pred,PredTerm),
        Pred=PN/_,
        concat_atom([Dir,/,PN,'.txt'],File),
        tell(File),
        forall(Mod:PredTerm,
               write_sqldata_row(PredTerm,Opts)),
        told.

write_sqldata_row(T,Opts) :-
        T=..[_|Args],
        write_sqldata_cols(Args,Opts),
        nl.
write_sqldata_cols([Arg],Opts) :-
        !,
        write_sqldata_col(Arg,Opts).
write_sqldata_cols([Arg|Args],Opts) :-
        !,
        write_sqldata_col(Arg,Opts),
        format('\t'),
        write_sqldata_cols(Args,Opts).

write_sqldata_col(X,_) :- compound(X),!,writeq(X).
write_sqldata_col(X,_) :- write(X).



        
write_ddl(Dir,Mod,Pred,Opts) :-
        Pred=PN/A,
        A>0,
        concat_atom([Dir,/,PN,'.sql'],File),
        tell(File),
        create_stmt(CreateStmt,Opts),
        format('~w ~w (~n',[CreateStmt,PN]),
        write_ddl_cols(PN,1,A,Opts),
        format(');~n',[]),
        write_indexes(Mod,Pred,Opts),
        told.

create_stmt('CREATE OR REPLACE TABLE',Opts) :-
        member(replace_table,Opts),
        !.
create_stmt('CREATE TABLE',_). 

write_indexes(_Mod,PN/A,Opts) :-
        member(index_all,Opts),
        !,
        forall(between(1,A,I),
               write_index(PN/A,I,Opts)).
write_indexes(_Mod,_,Opts) :-
        member(no_index,Opts),
        !.
write_indexes(_Mod,PN/A,Opts) :-
        write_index(PN/A,1,Opts).

write_index(PN/_,I,_Opts) :-
        format('CREATE INDEX ~w_ix~w ON ~w(c~w);~n',[PN,I,PN,I]).

write_ddl_cols(PN,N,N,Opts) :-
        !,
        write_ddl_col(PN,N,Opts),
        nl.
write_ddl_cols(PN,I,N,Opts) :-
        !,
        write_ddl_col(PN,I,Opts),
        writeln(','),
        I2 is I+1,
        write_ddl_cols(PN,I2,N,Opts).
write_ddl_col(PN,I,Opts) :-
        tbl_col_name(PN,I,CN),
        member(varchar(Size),Opts),
        Size>0,
        !,
        format('  ~w varchar(~w) NOT NULL',[CN,Size]).
write_ddl_col(PN,I,_) :-
        tbl_col_name(PN,I,CN),
        format('  ~w varchar NOT NULL',[CN]).

tbl_col_name(T,C,N) :-
        sql_compiler:attribute(C,T,N,_Type),
        !.
tbl_col_name(_,I,N) :-
        Code is I+96,
        atom_codes(N,[Code]).


        
/** <module> 

  ---+ Synopsis

==
:- use_module(bio(sql_bulkloader)).

% 
demo:-
  nl.
  

==

---+ Details

Example:
==
 blip  -i  ~/cvs/thea2/testfiles/pizza.owl sql-dump -schema ~/cvs/thea2/owl2_metamodel.pl owl2_model -metadata sql_compiler
==
 
---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
