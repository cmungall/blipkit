:- module(restl,
          [
           start_server/0,
           start_server/1
          ]).

:- use_module(interprocess).

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/html_head')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_client')).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(www, /, []).
http:location(restl, '/', []).
http:location(script, www(script), []).
http:location(css, www(css), []).

background:-
        repeat,
        fail.

start_server :-
        start_server(9000).

start_server(Port) :-
        http_server(http_dispatch, [port(Port)]).
        %http_server(reply, [port(Port)]).

:- http_handler(restl(.), root, [prefix]).
:- http_handler(restl('_all_dbs'), all_dbs, []).
%:- http_handler(restl(js), js_dir, [prefix]).

% TODO
param(title,     [optional(true)]).
param(name,      [length >= 2 ]).
param(url,       [optional(true),default('http://')]).
param(open,      [zero_or_more]).
param(action,    [optional(true)]).

% put vs get
root(Request) :-
        debug(restl,'Req: ~w',[Request]),
        member(method(Method), Request), !,
        root(Method,Request).

% PUT - assert or create
root(put,Request) :-
        request_path_local(Request,root,Db),
        debug(restl,'Db: ~w',[Db]),
        put_database(Db,Request).

% GET
root(_,Request) :-
        request_path_local(Request,root,Path),
        debug(restl,'Path: ~w',[Path]),
        concat_atom([Db|Parts],'/',Path),
        concat_atom(Parts,'/',Q),
        debug(restl,'Query: ~w for ~w',[Db,Q]),
        query_database(Db,Q).

query_database(Db,'') :-
        reply_json(json([db=Db])).

query_database(Db,A) :-
        atom_to_term(A,Q,Bindings),
        debug(restl,'Query[pl]: ~q bindings: ~w',[Q,Bindings]),
        db_stream(Db,S),
        debug(restl,'  s: ~w',[S]),
        ipr_query(S,json(Bindings),Q,L),
        debug(restl,'Results: ~w',[L]),
        reply_json(json([results=L])).

put_database(Db,Request) :-
        member(content_length(Len),Request),
        Len>1,
        !,
        http_read_data(Request, Data, [to(atom)]),
        debug(restl,'Data: ~w',[Data]),
        atom_to_term(Data,Term,_),
        database_assert(Db,Term),
        reply_json(json([ok=true,number_of_facts=1])).
        
put_database(Db,_Request) :-
        create_database(Db).

% HTTP UTIL

request_path_local(Request,Loc,X) :-
        memberchk(path(ReqPath), Request),
	http_location_by_id(Loc, Me),
	atom_concat(Me, X, ReqPath).

% LOGIC

:- dynamic db_stream/2.

database_assert(Db,Term) :-
        db_stream(Db,S),
        !,
        debug(restl,'Asserting: ~w in ~w',[Term,S]),
        ipr_assert(S,Term).

create_database(Db) :-
        db_stream(Db,_),
        !,
        reply_json(json([error=db_exists])).

create_database(Db) :-
        \+ db_stream(Db,_),
        !,
        debug(restl,'initializing: ~w',[Db]),
        init_pr(S),
        debug(restl,'stream: ~w',[S]),
        assert(db_stream(Db,S)),
        reply_json(json([ok=true])).

all_dbs(_) :-
        findall(Db,db_stream(Db,_),Dbs),
        reply_json(json([dbs=Dbs])).
