/* -*- Mode: Prolog -*- */


:- module(rdb_util,
          [
           rdb_lookup/2,
           rdb_query/2,
           rdb_query/3,
           rdb_forall/3,
           rdb_forall/4,
           rdb_findall/4,
           rdb_setof/4,
           rdb_solutions/4,
           rdb_connect/2,
           rdb_handle/2,
           
           sqltmpl_sqlterm/2,
           sqlbind/1,
           sqlbind/2,
           db_info/2
           ]).

:- use_module(library(odbc)).
:- use_module(bio(sql_compiler)).
:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util),[solutions/3,pred_to_unground_term/2]).

:- dynamic rdb_handle_cache/2.
:- dynamic schema_handle_cache/2.
:- dynamic sql_trace/0.

:- multifile qmap/2.

sql_trace(Sql):-
        sql_trace,
        !,
        writeln(sql(Sql)).
sql_trace(_).

%% rdb_connect(?Dbh,+Resource)
%
%  mode:semidet
%  
%  relies on bioresource/2 - for example:
%  ==
%  user:bioresource(rdb(foo),odbc_connect(foo,[user(me)])).
%  ==
:- mode rdb_connect(?,+) is det.
rdb_connect(H,Resource):-
        rdb_handle_cache(Resource,H),
        debug(sql,'reusing handle ~w -> ~w',[Resource,H]),
        !.
rdb_connect(H,Resource):-
        % TODO: clean this up
        (   user:bioresource(rdb(Resource),odbc_connect(Db,ConnArgs),Schema)
        ->  true
        ;   user:bioresource(Resource,odbc_connect(Db,ConnArgs),Schema),nonvar(Resource)
%	->  true
%        ;   user:bioresource(X,odbc_connect(Resource,ConnArgs),Schema),nonvar(X)
	->  true %Db=Resource
        ;   Db=Resource,ConnArgs=[]),
        !,
        debug(sql,'~w',[odbc_connect(Db,Dbh,ConnArgs)]),
        odbc_connect(Db,Dbh,ConnArgs), % throws?
        debug(sql,'Connected',[]),
        rdb_register_handle(Schema,Dbh),
        H=Dbh,
        assert(rdb_handle_cache(Resource,H)).
        %rdb_set_metapreds(H).
rdb_connect(H,Resource):-
        H=rdb(plsql(Db,ConnArgs),Schema,[]),
	user:bioresource(Resource,plsql_connect(Db,ConnArgs),Schema).


rdb_connect(Resource):-
        rdb_connect(_,Resource).

% TODO: fix or deprecate
rdb_set_metapreds(H):-
        assert( ( H:forall(X,Y) :- rdb_forall(H,X,Y) ) ),
        assert( ( H:findall(X,Y,Z) :- rdb_findall(H,X,Y,Z) ) ),
        assert( ( H:setof(X,Y,Z) :- rdb_setof(H,X,Y,Z) ) ),
        assert( ( H:solutions(X,Y,Z) :- rdb_solutions(H,X,Y,Z) ) ).


% todo - allow multiple handles per schema, connect across them all
rdb_register_handle(Schema,Dbh):-
        assert(schema_handle_cache(Schema,Dbh)).

%% rdb_handle(+Rdb,?Dbh)
%  @param Rdb
%  rdb(Dbh,Schema,Args)
%  given an Rdb term, return the ODBC handle
%
%  An Rdb term is a convenient wrapper for an ODBC handle, storing extra
%  info such as the name of the schema/datamodel
%  
rdb_handle(rdb(Dbh,_,_),Dbh):- !.
rdb_handle(H,H).

%% rdb_lookup(?Dbh,+QueryTerm)
%
%  mode:nondet
%  
%  queries Dbh for the specified term
%
%  @param Dbh
%  rdb handle
%  @param QueryTerm
%  
%       should have a mapping in the qmap table
:- mode rdb_connect(?,+) is det.
rdb_lookup(Rdb,QTerm):-
        qterm_sqlterm(QTerm,SqlTerm), % sd: find and unify via qmap

        % query based on SqlTerm (which has vars unified with QTerm)
        rdb_query(Rdb,SqlTerm).


%% rdb_query(?Dbh,+SqlTerm)
% DEPRECATEd???
%  mode:nondet
%  
%  queries Dbh using SqlTerm, unified unground vars
%
%  @param SqlTerm = sql(cols=[..],from=[..],where=[..])
%         should have a mapping in the qmap table
rdb_query(Rdb,SqlTerm):-
        % generate the SQL string from the sql term
        sqlterm_sql(SqlTerm,Sql,PlaceholderVals),
        rdb_handle(Rdb,Dbh),
        findall(default,member(_,PlaceholderVals),Params),
        debug(sql,'SQL: ~w',[Sql]),
        odbc_prepare(Dbh,
                     Sql,
                     Params,
                     Sth,
                     []),
        !,
        % problem with odbc interface?
        % integers need converted to atoms
        findall(Val,(   member(Val1,PlaceholderVals),
                        (   number(Val1)
                        ->  atom_number(Val,Val1)
                        ;   Val=Val1)),
                FixedVals),
        debug(sql,'Execute; binding: ~w to ~w',[FixedVals,Sth]),
        odbc_execute(Sth,FixedVals,RowTerm),
        % map a odbc row term back to original qterm, unifying args
        unify_sqlterm_and_rowterm(SqlTerm,RowTerm).

%% rdb_query(?Rdb,+Project,+Goal)
%  mode:nondet
%  
%  queries Dbh using Draxler method
%
%  @param Project=<functor>(ColVars)
%       this argument is unground
%  @param Goal
%       a prolog term expressing query
%
%  see plterm_to_sqlterm
:- module_transparent(rdb_query/3).
rdb_query(Rdb,Project,Goal):-
        rdb_handle(Rdb,Dbh),
        plterm_to_sqlterm(Project,Goal,SqlTerm),
        sqlterm2atom(SqlTerm,Sql),
        debug(sql,'SQL: ~w',[Sql]),
        odbc_prepare(Dbh,
                     Sql,
                     [],
                     Sth,
                     []),
        !,
        debug(sql,'prepared, now executing sth=~w',[Sth]),
        odbc_execute(Sth,[],RowTerm),
        debug(sql,'RowTerm: ~w',[RowTerm]),
        (   is_list(Project)
        ->  ProjectColVars=Project
        ;   compound(Project)
        ->  Project=..[_|ProjectColVars]
        ;   ProjectColVars=[Project]),
        RowTerm =.. [row|Vals],
        unify_project_cols(ProjectColVars,Vals).

:- module_transparent(rdb_forall/3).
rdb_forall(Db,Goal,Action):-
        term_variables(Goal,Vars),
        Term=..[f|Vars],
	%Term=Vars,
        forall(rdb_query(Db,Term,Goal),
               Action).

:- module_transparent(rdb_forall/4).
rdb_forall(Db,Proj,Goal,Action):-
        forall(rdb_query(Db,Proj,Goal),
               Action).

:- module_transparent(rdb_findall/4).
rdb_findall(Db,Template,Goal,Results):-
        findall(Template,
                rdb_query(Db,Template,Goal),
                Results).
:- module_transparent(rdb_setof/4).
rdb_setof(Db,Template,Goal,Results):-
        setof(Template,
              rdb_query(Db,Template,Goal),
              Results).
:- module_transparent(rdb_solutions/4).
rdb_solutions(Db,Template,Goal,Results):-
        solutions(Template,
                  rdb_query(Db,Template,Goal),
                  Results).

%% unify_project_cols(+ProjectionVars,+ResultVals) is det
% assumes order is preserved
unify_project_cols([],[]).
unify_project_cols([Var|RestVars],Vals):-
	compound(Var),
	!,
	Var=..[_|InnerVars],
	append(InnerVars,RestVars,Vars),
	unify_project_cols(Vars,Vals).
unify_project_cols([Var|Vars],[Val|Vals]):-
        Var=Val,
        !,
        unify_project_cols(Vars,Vals).
unify_project_cols([Var|Vars],[Val|Vals]):-
        number(Var),
        atom_number(Val,Var),
        !,
        unify_project_cols(Vars,Vals).

%% unify_sqlterm_and_rowterm(+SqlTerm,+RowTerm) is det
% given a term of form sql(...) and a row of form row(...),
% unify with the sql select columns, assuming order is identical
unify_sqlterm_and_rowterm(SqlTerm,RowTerm):-
        RowTerm =.. [row|Vals],
        sqlterm_select(SqlTerm,SqlColVals),
        unify_vals(SqlColVals,Vals).

% (+,?) d
% unify a list of Col=Vals to the equivalent Val list
unify_vals([],[]).
unify_vals([_=Val|ColVals],[Val|Vals]):-
        unify_vals(ColVals,Vals).

% (+,?) nd
% (mode is sd no more than one templates for a qterm)
% given a query term, get the equivalent sql term using qmap
qterm_sqlterm(QTerm,SqlTerm):-
        qmap(QTerm,Sqlt),
        sqltmpl_sqlterm(Sqlt,SqlTerm).

% (+,?)
sqltmpl_sqlterm(Sqlt,SqlTerm):-
        sqlterm_cols(Sqlt,SqlColVals),
        unify_sql(SqlColVals,WhereColVals,SelectColVals),
        sqlterm_from(Sqlt,Froms),
        SqlTerm=select(select=SelectColVals,
                       from=Froms,
                       where=WhereColVals).

% (+,?,?) d
% given a list of Col=Var select constraints, get back a list of
% where constraints and select Col=Var constraints depending on
% whether the variable is ground
% (ground variables become where constraints, unground become selects)
unify_sql([],[],[]).
unify_sql([Col=Val|ColVals],[Col=Val|WhereColVals],SelectColVals):-
        not(var(Val)),
        !,
        unify_sql(ColVals,WhereColVals,SelectColVals).
unify_sql([Col=Val|ColVals],WhereColVals,[Col=Val|SelectColVals]):-
        unify_sql(ColVals,WhereColVals,SelectColVals).

% (+,?,?) d
% map a sql term an SQL string with an ordered list of placeholder
% values
sqlterm_sql(SqlTerm,Sql,PlaceholderVals):-
        % select clause
        sqlterm_select(SqlTerm,SelectColVals),
        findall(Col,member(Col=_,SelectColVals),Cols),
        concat_atom(Cols,',',SelectClause),

        % from clause - implicitly a conjunction
        sqlterm_from(SqlTerm,Froms),
        concat_atom(Froms,' AND ',FromClause),

        % where clause: use a place holder '?' for every constraint
        % make sure we can map from placeholders to a value list
        sqlterm_where(SqlTerm,Constraints),
        findall(WhereQt,(member(Constraint,Constraints),
                         constraint_sql(Constraint,WhereQt)),WhereClauses1),
        % standard, non-placeholder constraints
        findall(Constraint,(member(Constraint,Constraints),
                            not(constraint_sql(Constraint,_))),
                WhereClauses2),
        flatten([WhereClauses1,WhereClauses2],WhereClauses),
        findall(Val,(member(Constraint,Constraints),
                     constraint_sql(Constraint,_,Val)),PlaceholderVals),
        (WhereClauses=[]
        ->  WhereClause=''
        ;   concat_atom(WhereClauses,' AND ',WhereClause0),
            concat_atom(['WHERE ',WhereClause0],WhereClause)),
        sformat(Sql,            
                'SELECT ~w~nFROM ~w~n~w~n',
                [SelectClause,FromClause,WhereClause]).
        


% (+,?) d
% given a Col=Val constraint, generate equivalent sql string
constraint_sql(Constr,A):-
        constraint_sql(Constr,A,_).
constraint_sql(Col=Val,A,Val):-
        !,
        concat_atom([Col,' = ?'],A).
constraint_sql(Constr,A,Val):-
        Constr =.. [Operator,Col,Val], % eg like(X,'foo%'), X < 5
        !,
        concat_atom([Col,Operator],A).

% fetch things from an sql term
sqlterm_cols(SqlTerm,Val):-  sqlterm_clause(cols,SqlTerm,Val).
sqlterm_select(SqlTerm,Val):-  sqlterm_clause(select,SqlTerm,Val).
sqlterm_from(SqlTerm,Val):-  sqlterm_clause(from,SqlTerm,Val).
sqlterm_where(SqlTerm,Val):-  sqlterm_clause(where,SqlTerm,Val).

sqlterm_clause(Clause,SqlTerm,Val):-
        SqlTerm =.. [_|SqlArgs],
        member(Clause=Val1,SqlArgs),
        !,
        % ensure Val is a list
        (is_list(Val1)
        ->  Val=Val1
        ;   Val=[Val1]).

%% sqlbind(+BindArgs) is det
% BindArgs = Module:PredicateSpec-DB or Module:PredicateSpec
% PredicateSpace = Predicate/Arity or 'all'
% leave DB to connect to the current open db handle.
% Examples:
% ==
% sqlbind(ontol_db:all)
% ==
% or
% ==
% sqlbind(genome_db:all-enscore(homo_sapiens_lite)),sqlbind(ontol_db:subclass/2-godb(go))
% ==
%  see sqlbind/2
sqlbind(Mod:HeadSpec-DB):-
        !,
        sqlbind(Mod:HeadSpec,DB).
sqlbind(Mod:HeadSpec):-
        !,
        sqlbind(Mod:HeadSpec,_).
sqlbind(Pred):-
        throw(bad_argument(Pred)).

%% sqlbind(Module:Predicate,DB)
% rewrites Predicate to a SQL call on DB
%
% combining sqlbind/2 with table_pred/2 - always table the predicate after binding the predicate to SQL.
% this has the effect of cacheing SQL calls in-memory
sqlbind(Mod:all,DB):-
        !,
        ensure_loaded(bio(sql_compiler)),
        ensure_loaded(bio(dbmeta)),
        %ensure_loaded(bio(Mod)),
        debug(sql,'Binding all preds in ~w to ~w',[Mod,DB]),
        %forall(datapred(Mod,Pred),
        export_list(Mod,Preds),
        forall(member(Pred,Preds),
               sqlbind(Mod:Pred,DB)).
sqlbind(Mod:HeadSpec,DB):-
        !,
        debug(sql,'Binding ~w in ~w to ~w',[HeadSpec,Mod,DB]),
        ensure_loaded(bio(sql_compiler)),
        %ensure_loaded(bio(rdb_util)),
        pred_to_unground_term(HeadSpec,Head),
        Clause = (Head :- rdb_util:rdb_connect(Rdb,DB),rdb_util:rdb_query(Rdb,Head,Head)),
        forall((clause(Mod:Head,Body),\+ sql_compiler:view(Head,_),\+ sql_compiler:view(_:Head,_)),
               (   debug(sql,'Asserting View: ~w <- ~w',[Head,Body]),
                   assert(sql_compiler:view(Head,Body)))),
        debug(sql,'Abolishing: ~w ',[HeadSpec]),
        Mod:abolish(HeadSpec),
        debug(sql,'Asserting: ~w : ~w',[Mod,Clause]),
        Mod:assert(Clause).
sqlbind(Pred,DB):-
        throw(cannot_bind(Pred,DB)).

% extracts schema.
% suitable form for draxler code sql_compiler
db_info(Rdb,relation(R,Arity,TableType,Facets)):-
        rdb_handle(Rdb,Dbh),
        odbc_current_table(Dbh,R,arity(Arity)),
        odbc_current_table(Dbh,R,type(TableType0)),
        table_type_map(TableType0,TableType),
        findall(Facet,odbc_current_table(Dbh,R,Facet),Facets).
db_info(Rdb,attribute(Ord,R,Attr,Type,Facets)):-
        rdb_handle(Rdb,Dbh),
        db_info(Dbh,relation(R,_,_,_)),
        % first fetch in bulk - we want to ennumerate
        findall(X,odbc_table_column(Dbh,R,X),Attrs),
        nth1(Ord,Attrs,Attr),
        odbc_table_column(Dbh,R,Attr,type_name(DbType)),
        dbtype_to_type(DbType,Type),
        findall(Facet,odbc_table_column(Dbh,R,Attr,Facet),Facets).

% ----------------------------------------
% wrapper that abstracts from ODBC
% ----------------------------------------

%% plsql_prepare(+Dbh, +Sql, +Params, ?Sth, ?Opts)
% wraps odbc_prepare/5
plsql_prepare(Dbh, Sql, _Params, Sth, _L):-
	Dbh=rdb(plsql(_Db,_ConnArgs),_Schema,[]),
	!,
	tmp_file(SqlFile),
	tell(SqlFile),
	writeln(Sql),
	told,
	Sth=sth(Dbh,SqlFile).
plsql_prepare(Dbh, Sql, Params, Sth, L):-
	odbc_prepare(Dbh, Sql, Params, Sth, L).


%% plsql_execute(+Sth,+Params,?Row)
% wraps odbc_execute/3
plsql_execute(sth(mysql,H,SqlFile),_,Row):- % TODO: hooks
	tmp_file(Out),
	sformat(Cmd,'cat ~w | mysql ~w > ~w',[SqlFile,H,Out]),
	open(pipe(Cmd),read,IO,[]),
	stream_parsetab_row(IO,Row).
plsql_execute(sth(postgresql,H,SqlFile),_,Row):- % TODO: hooks
	tmp_file(Out),
	sformat(Cmd,'cat ~w | psql ~w -A -F $\'\\t\' > ~w',[SqlFile,H,Out]),
	open(pipe(Cmd),read,IO,[]),
	stream_parsetab_row(IO,Row).
plsql_execute(Sth,Params,Row) :-
        odbc_execute(Sth,Params,Row).

stream_parsetab_row(IO,row(Vals)):-
	read_line_to_codes(IO,Codes),
	atom_codes(A,Codes),
	atom_codes(Delim,[9]),
	concat_atom(Vals,Delim,A).
        
table_type_map(X,Y):- downcase_atom(X,Y).

dbtype_to_type(T,T1):-
        typemap(T,T1),
        !.
dbtype_to_type(X,X).

typemap(varchar,string).
typemap(bpchar,string).
typemap(char,string).
typemap(text,string).
typemap(int4,integer).
typemap(int3,integer).
typemap(int2,integer).
typemap(int1,integer).
typemap(int,integer).

/** <module> abstracts queries on relational databases

  ---+ Synopsis

  ==
  :- use_module(bio(rdb_util)).
  rdb_util:qmap(term_assoc(ID,Sym),
     sql(cols=['term.acc'=ID,
               'gene_product.symbol'=Sym],
         from=['term INNER JOIN association ON (term.id=term_id) INNER JOIN gene_product ON (gene_product.id=gene_product_id)'])).

  demo:-
    rdb_connect(Dbh,go),
    GeneSym=dpp,
    forall(rdb_lookup(Dbh,term_assoc(ID,GeneSym)),
           format('Gene: ~w annotated_to: ~w~n',[GeneSym,ID])).
  ==
  
  ---+ Description

  This module provides convenience methods for SQL database access, and
  generalized term to SQL mapping

  allows a db to be queried transparently

  ---++ SqlTerm representations

  An SqlTerm is represented as =|sql(cols=Cols,from=From,where=Where)|= where

  
    * Cols
        is a list of Col=Val pairs. Val can be unground (to be fetched) or
        ground (a constraint)
    * From
        an SQL from clause or list of clauses to be ANDed together
    * Where
        an SQL where clause or list of clauses to be ANDed together

  ---+ TODO

  currently we must query via rdb_lookup(Dbh,Pred)
  fix it so that we can manifest all qmap predicates and cache handle
  so that the rdb layer is completely transparent

@author   Chris Mungall
@version  $Revision: 1.12 $
@see      rdb_connect/2, view/2, odbc_setup.txt
@license  LGPL
  */
