/* -*- Mode: Prolog -*- */


:- module(blipkit_serval,[]).

:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util)).
%:- use_module(bio(metadata_db)).
:- use_module(bio(io)).
:- use_module(bio(serval)).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_session')).

:- multifile
        user:bench_call/2,
        user:amigo_component/1.

:- multifile
        blipkit:opt_description/2,
        user:use_bioresource/1,
        user:use_bioresource/3,
        user:amigo_port/1.

blipkit:example('blip -r go -r obd/obd_annot -u blipkit_serval serval -port 9000 -u amigo_term -bg',
                'starts a serval server').
:- blip('serval-wget',
        'emulates a web page invocation',
        [
         atoms(amigo,ConfL),
         atoms(aconf,Ms),
         term(func,Term,init_page)
        ],
        _,
        (   debug(obol,'serval: ~w',[ConfL]),
            forall(member(Conf,ConfL),
                   user:consult(amigo_conf(Conf))),
            forall(member(M,Ms),
                   user:consult(bio(M))),
            forall(amigo_component(Src),
                   user:consult(amigo_src(Src))), % !!!
            ensure_loaded(bio(serval)),
            write_sterm(Term))).

blipkit:opt_description(amigo,'conf file to consult. Uses amigo_conf( ...). E.g. -amigo amigo_obol.').
:- blip('serval',
        'starts a serval/http web server',
        [
         number(port,Port,8200),
         bool([background,bg],Bg),
         atoms(amigo,ConfL)
        ],
        _,
        (   debug(obol,'conf: ~w',[ConfL]),
            forall(member(Conf,ConfL),
                   user:consult(amigo_conf(Conf))),
            forall(amigo_component(Src),
                   user:consult(amigo_src(Src))), % !!!
            (   var(Port)
            ->  (   amigo_port(Port)
                ->  true
                ;   Port=8111)
            ;   true),
            ensure_loaded(bio(serval)),
            start_server(Port),
            (   Bg=1
            ->  background
            ;   true),
            prolog)).
background:-
        repeat,
        fail.


:- blip('blip-server',
        'RESTful wrapper to blip services',
        [atom(port,Port,9090 ),
         bool([bg,background],Bg)],
        _Files,
        (   start_server2(Port),
            (   Bg=1
            ->  background
            ;   true),
            prolog)).

% todo: clean this up
start_server2(Port) :-
        http_set_session_options([]),
        mutex_create(mutex_session_id), % make session-granting threadsafe
        http_server(reply2, [port(Port)]).

%fselect(E,L,Rest):- select(E,L,Rest),!.
%fselect(_,L,L):- !.

reply2(Request):-
        format(user_error,'r=~w~n',[Request]),
        member(path(Path),Request),
        member(search(Params),Request),
        format(user_error,'r=~w~n',[Params]),
        concat_atom(PathElts,'/',Path),
        reverse(PathElts,[Cmd|_]),
        nl,
        tagval_parameters([atoms([r,resource],Resources),
                           atoms([u,use],UseModules)],
                          Params,
                          Params1),
        debug(foo,'loading ~w',[Resources]),
        maplist(load_bioresource,Resources),
        forall(member(M,UseModules),
               ensure_loaded(bio(M))),
        debug(foo,'path ~w',[Cmd]),
        blipkit:main(Cmd,_,Module,OptSpec,RemainingArgs,Modules,Action),
        debug(foo,'os ~w',[OptSpec]),
        tagval_parameters(OptSpec,
                          Params1,
                          RemainingArgs),
        debug(foo,'p1 ~w',[Params1]),
        maplist(ensure_loaded,Modules),
        (   time_goal(Module:Action,Time),
            debug(time,'action: ~w in ~w',[Action,Time])
        ->  true
        ;   throw(error(command_failed(Cmd)))).

tagval_parameters([Spec|Specs],P0,P1):-
        debug(foo,'spec ~w',[Spec]),
        process_spec(Spec,P0,PX),
        debug(foo,'pspec ~w',[Spec-P0-PX]),
        !,
        tagval_parameters(Specs,PX,P1).
tagval_parameters([_|Specs],P0,P1):-
        !,
        tagval_parameters(Specs,P0,P1).
tagval_parameters([],P,P).

process_spec(Spec,PsIn,PsOut):-
        Spec=..[T,X|L],
        \+ is_list(X),
        !,
        Spec2=..[T,[X]|L],
        debug(foo,'rewritten ~w',[Spec2]),
        process_spec(Spec2,PsIn,PsOut).

        
process_spec(atoms(Tags,Values),PsIn,PsOut):-
        findall(V,(member(T,Tags),member(T=V,PsIn)),Values),
        findall(T=V,(member(T=V,PsIn),\+member(T,Tags)),PsOut).
process_spec(atom(Tags,Value,Default),PsIn,PsOut):-
        (   process_spec(atom(Tags,Value),PsIn,PsOut)
        ->  true
        ;   Value=Default,
            PsOut=PsIn).
process_spec(atom(Tags,Value),PsIn,PsOut):-
        findall(V,(member(T,Tags),member(T=V,PsIn)),[Value]),
        findall(T=V,(member(T=V,PsIn),\+member(T,Tags)),PsOut).
process_spec(bool(Tags,1),PsIn,PsOut):-
        setof(T,(member(T,Tags),member(T,PsIn)),_Ts),
        !,
        findall(T,(member(T,Tags),\+member(T,PsIn)),PsOut).
process_spec(number(Tags,Number),PsIn,PsOut):-
        process_spec(atom(Tags,Value),PsIn,PsOut),
        !,
        atom_number(Value,Number).
process_spec(number(Tags,Value,Default),PsIn,PsOut):-
        (   process_spec(number(Tags,Value),PsIn,PsOut)
        ->  true
        ;   Value=Default,
            PsOut=PsIn).




        
        

        
        





