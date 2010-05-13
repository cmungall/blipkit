/* -*- Mode: Prolog -*- */



:- module(sql_parser,
          []).

% basic types of statement
sql_stmts( [S|Ss] ) --> sql_stmt(S),[';'],sql_stmts(Ss).
sql_stmts( [] ) --> [].

sql_stmt( S ) --> select_stmt(S).
sql_stmt( S ) --> create_stmt(S).

% SELECT statements
select_stmt( select(cols(SQ,Cols),From,Where,Group,Having,Comb,Order,Limit,Offset) ) -->
        kw(select),
        select_qualifier(SQ),
        select_cols(Cols),
        from_clause(From),
        where_clause(Where),
        group_clause(Group),
        having_clause(Having),
        combiner(Comb),
        order_clause(Order),
        limit_clause(Limit),
        offset_clause(Offset).

% SELECT
select_qualifier(all) --> kw(all).
select_qualifier(distinct) --> kw(distinct).
select_qualifier([]) --> [].

% brackets??
select_cols([C]) --> select_col(C).
select_cols([C|Cs]) --> select_col(C),[','],select_cols(Cs).

select_col('*') --> ['*'].
select_col(C) --> column_ref(C).

% FROM
from_clause(L) --> kw(from),from_tables(L).

from_tables([X]) --> from_table(X).
from_tables([X|Xs]) --> from_table(X),[','],from_tables(Xs).

from_table(X) --> table_name(X).
from_table(join(X,J)) --> table_name(X),table_join(J).

table_join(natural(X)) --> kw(natural),kw(join),from_table(X).
table_join(inner(X,C)) --> kw(inner),kw(join),from_table(X),join_cond(C).
table_join(left(X,C)) --> kw(left),kw(outer),kw(join),from_table(X),join_cond(C).

join_cond(using(X)) --> kw(using),select_cols(X).
join_cond(on(X)) --> kw(on),bool_expr(X).

% WHERE
where_clause(X) --> kw(where),bool_expr(X).

bool_expr(bool(Op,X1,X2)) -->  not_bool_expr(X1) boolop(Op) bool_expr(X2).
bool_expr(X) -->  not_bool_expr(X).

not_bool_expr(not(X)) --> ['!'],brack_bool_expr(X).
not_bool_expr(X) --> brack_bool_expr(X).

brack_bool_expr(X) --> ['('],bool_expr(X),[')'].
brack_bool_expr(X) --> prim_bool_expr(X).
prim_bool_expr(X) --> boolval(X).
prim_bool_expr(X) --> expr(X).
boolval(true) --> kw(true).
boolval(false) --> kw(false).
boolval(null) --> kw(null).

% CREATE statements

create_stmt( S ) --> create_table( S ).

%  CREATE TABLE
create_table( table(Name,CreateTableParts) ) -->
        kw(create),kw(table),table_name(Name),
        ['('],
        column_defs_or_constraints(CreateTableParts),
        [')'].

column_defs_or_constraints( [P] ) -->
        column_defs_or_constraint( P ).
column_defs_or_constraints( [P|Ps] ) -->
        column_defs_or_constraint( P ),[','],
        column_defs_or_constraints( Ps ).

column_def_or_constraint( ColDef ) -->
        column_def( ColDef ).
column_def_or_constraint( Constraint ) -->
        table_constraint( Constraint ).

column_def( column(Name,Type,Qualifiers) ) -->
        column_name(Name),
        column_type(Type),
        column_qualifiers(Qualifiers).

column_type(Type) -->
        column_type_with_size(BaseType),['('],number(Num),[')'],
        {Type=..[BaseType,Num]}.
column_type(Type) -->
        column_type_with_no_size(Type).

column_type_with_size(Type) --> [Type],{column_type_with_size(Type)}.
column_type_with_no_size(Type) --> [Type],{column_type_with_no_size(Type)}.

column_qualifiers( Qs) --> zero_or_more(column_qualifier,Q,Qs).

column_qualifier( not_null ) -->
        kw(not),kw(null).
column_qualifier( Ref ) -->
        foreign_key_reference(Ref).

table_constraint( unique(Cols) ) -->
        kw(unique),column_list(Cols).
table_constraint( foreign_key(Col,Ref) ) -->
        kws([foreign,key]),column_name(Col),foreign_key_reference(Ref).

% can occur in the qualifiers of the column def,
% OR in a separate table-constraint line after "FOREIGN KEY"
foreign_key_reference( references(Table,Col,CascadeRule) ) -->
        kw(references),table_name(Table),'(',column_name(Col),')',
        cascade_rule(CascadeRule).

cascade_rule(default) --> [].
cascade_rule(on_delete(Action,Quals)) -->
        kws([on,delete]),
        cascase_rule_action(Action),
        cascade_rule_qualifiers(Quals).

cascase_rule_action(cascade) --> kw(cascade).
cascase_rule_action(set_null) --> kws([set,null]).

% generic
table_name(X) --> word(X).
column_name(X) --> word(X).
column_ref(C) --> column_name(C).
column_ref(col(T,C)) --> table_name(T),['.'],column_name(C).


% metadata
column_type_with_size(varchar).
column_type_with_size(char).
column_type_with_no_size(int).
column_type_with_no_size(boolean).
column_type_with_no_size(text).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
  ---++ sql_parser
- parses SQL queries

  ---+ Synopsis

  ==
  :- use_module(bio(sql_parser)).

  ==

  ---+ Description

  Parses SQL query syntax - to provide SQL-datalog bridge

  Can we query a datalog db using SQL? What about the lack of column names?
  allow positional specs to be used too
  
  SELECT isaT.arg1
  FROM isaT INNER JOIN class ON (isaT.arg2=class.id)
  WHERE class.name='foo';
  
**/