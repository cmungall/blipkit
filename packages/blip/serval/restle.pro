/* -*- Mode: Prolog -*- */

%% ===================================
%% == DOCUMENTATION AT END OF FILE  ==
%% ===================================

:- module(restle,
          [
           ]).

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_session')).

%% start_server(+Port)
%
%  starts a server on a specified port
%  
start_server(Port) :-
        http_set_session_options([cookie(serval_cookie)]), % todo - param
        mutex_create(mutex_session_id), % make session-granting threadsafe
        http_server(reply, [port(Port)]).

reply(Request):-
        parse_request(Request),
        nl.

handle('/person/{id}',person_resource).
handle('/person/{id}.{fmt}',person_resource).
handle('/person/{id}',[id=ID],person_resource(ID)):- nl.
handle([person,var(id)],person_resource).
handle('/image/',directory_resource('images')).

path_pattern(path([M|Ms])) --> path_part(M),"/",!,path_pattern(path(Ms)).
path_pattern(path([])) --> [].

path_part(var(P)) -> "{",name(P),"}",!.
path_part(P) -> toks(P).

path_request(path([M|Ms])) --> path_part(M),"/",!,path_request(path(Ms)).
path_request(path([])) --> [].

