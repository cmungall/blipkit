:- module(sql_schema_creator,[write_ddl/0,
			      convert_module/1,
			      convert_file/1]).

:- use_module(sql_compiler).
:- use_module(bio(dbmeta)).
:- use_module(library(pldoc)).
:- use_module(library('pldoc/doc_register')).
:- use_module(library('pldoc/doc_process')).
:- use_module(library('pldoc/doc_wiki')).
:- use_module(library('pldoc/doc_modes')). % this is required for compile/1 to register mode/2

read_term_wc(In,Term,Comments):-
	read_term(In, Term,
		  [ variable_names(_Bindings),
		    comments(Comments),
		    module(pldoc_modes)
		  ]).

write_ddl:-
	forall(relation(T,_),
	       write_table(T)).

write_table2(T):-
	format('CREATE TABLE ~w (~n',[T]),
	findall(A-D,attribute(_,T,A,D),As),
	write_cols([id-serial,As]),
	format('~n);~n~n').

write_cols([C]):-
	!,
	write_col(C).
write_cols([C|Cs]):-
	write_col(C),
	format(',~n'),
	write_cols(Cs).

write_col(A-D):-
	format('    ~w ~w',[A,D]).

write_table(Head):-
	Head =.. [P|Args],
	length(Args,Arity),
	find_relation(P,Arity,P2),
	!,
	format('CREATE TABLE ~w (~n',[P2]),
	findall(Att-D,table_col(P,Att,D),Atts), 
	write_cols([id-serial|Atts]),
	format('~n);~n~n').

write_table(Head1):-
	Head1 =.. [P|Args],
	length(Args,Arity),
	functor(Head,P,Arity),
	catch(plterm_to_sqlterm(Head,Head,SqlTerm),
              _,
              fail),
	sqlterm2atom(SqlTerm,Sql),
	!,
	format('CREATE VIEW ~w AS ~w~n~n',[P,Sql]).


write_table(Head):-
	!,
	format(user_error,'Cannot translate: ~w~n',[Head]).


table_col(P,A,D):-
	downcase_atom(P,Pd),
	attribute(_,P2,A,D),
	downcase_atom(P2,Pd).

find_relation(P,A,P2):-
	sql_compiler:relation(P2,A),
	downcase_atom(P2,Pd),
	downcase_atom(P,Pd).

convert_module(Mod) :-
	forall(datapred(Mod,PredSpec),
	       write_table_from_predspec(PredSpec)).

write_table_from_predspec(P/A) :-
	format('CREATE TABLE ~w (~n',[P]),
	forall(between(1,A,N),
	       write_anon_column(N,A)),
	format(');~n~n').

write_anon_column(N,A) :-
	format('    arg~w',[N]),
	(   N=A
	->  true
	;   format(',')),
	nl.
	

convert_file(File):-
	compile(File),
	%forall('$mode'(Head,Det),
	%       format(' ~w is ~w.~n',[Head,Det])),
	%forall(('$pldoc'(Id, Pos, Summary, Comment),indented_lines(Comment,[],Lines),doc_wiki:tags(Lines,Tags)),
	%       format('TAGS: ~w~n',[Tags])),
	%forall('$mode'(Head,_),
	forall(mode(Head,_),
	       write_table(Head)).

convert_file2(File):-
	open(File,read,In,[]),
	read_structured_comments(File,Comments),
	process_comments(Comments,_,File),
	forall('$pldoc'(Id, Pos, Summary, Comment),
	       writeln(Id-Summary-Comment)),
	repeat,
	(   read_term(In, Term,
		      [ variable_names(_Bindings),
			comments(Comments),
			term_position(Pos),
			module(pldoc_modes)
		      ]),
	    Term\=end_of_file
	->  (	process_comments(Comments,Pos,File)
	    ->	true
	    ;	true),
	    writeln(Pos-Term),
	    fail
	;   !),
	close(In),
	writeln(xxxx),
	forall('$pldoc'(Id, Pos, Summary, Comment),
	       writeln(Id-Summary-Comment)).

	



	

/** <module> generates SQL DDL from prolog source

  ---+ Synopsis

==
:- use_module(bio(sql_schema_creator)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README, sql_compiler
@license License


*/
