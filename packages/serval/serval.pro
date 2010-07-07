/* -*- Mode: Prolog -*- */

%% ===================================
%% == DOCUMENTATION AT END OF FILE  ==
%% ===================================

:- module(serval,
          [
           start_server/1,
           nmap/2,
           write_sterm/1,
           write_sterm/2,
           write_sterm/3,
           test_serval/1,
           append_session_data/4,
           add_session_data/3,
           get_app_param/3,     % DEPREC - use config_setting
           config_setting/2,
           getparam_as_num/3,
           ngetparam/3,
           ngetparam/4,
           dgetparam/4,
           getparam/3,
%           getparam/4,
           lgetparam/3,
           lgetparam/4,
           submit_param/2,
           userlog/1,

           op(800,xfy,forall),
           op(800,xfy,forall_unique),
           op(800,xfy,where),
           op(600,xfy,in),
           op(1200,xfy,(::)),
           op(1150,xfy,(=>))
          ]).
:- op(800,xfy,forall).
:- op(800,xfy,forall_unique).
:- op(800,xfy,where).
:- op(600,xfy,in).
:- op(1200,xfy,(::)).
%:- op(1150,xfy,(=>)).

% see SWI documentation for different servers
% TODO: make this configurable at run-time
%:- use_module(library('http/xpce_httpd')).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_session')).
:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util),
              [solutions/3,
               list_to_tuple/2,
               tuple_to_list/2]).
:- use_module(bio(xml_writer),
              [
               xmldoc_start/1,
               xmlnode_start/4,
               xmlnode_end/3,
               xmlnode/2
              ]).

/*
  run time facts

  the server process is kept alive. new sessions are asserted as facts
  in the runtime db.
  
  */

% numeric ID. incremented each new session
:- dynamic next_session_id/1.
:- assert(next_session_id(1)).

% session_data(SessionID,Param,Value)
% tag/value data per session
:- dynamic session_data/3.
:- dynamic session_stats/2.

:- discontiguous user:sdefun/2, user:sdefun/3.
:- discontiguous (=>)/2.

% the application is defined by predicates in the user module
% (is there a way to avoid redundantly specifying multifile and
%  default predicates?)
:- multifile
        user:http_param_label/2,
        user:init_page/1,
        user:custom_http_header/1,
        user:sdefun/2,
        user:sdefun/3,
        user:transition/4,
        user:strans/4,
        user:strans/5,
        user:on_transition/3,
        user:init_session_state/1,
        user:app_name/1,
        user:app_param/2,       % deprec
        user:config_global_default/2,  % per-serval settings
        user:config_default/2,  % per-application settings
        user:config_desc/2,     % Name,Desc
        user:config_type/2,     % enum(List)
        user:form_method/1,
        user:hidden_param/1,
        user:init_session_state/1.

:- multifile
	system:term_expansion/2.

system:term_expansion( (Head => Body :: pre(S,Conds)),
                     [   user:sdefun(Head,Goals),
                         user:strans(Head,S,Conds,true,add([]))]):-
        !,
        debug(serval_macros,'expanding ~w',[ sdefun_cond(Head,Body) ]),
        tuple_to_list(Body,Goals).

system:term_expansion( (Head => Body),
                     user:sdefun(Head,Goals)):-
        !,
        debug(serval_macros,'expanding ~w',[ sdefun(Head,Body) ]),
        tuple_to_list(Body,Goals).

%(test_(x) => y):- fail.
%user:sdefun(H,BodyGoals):-
%        (H => BodyTuple), tuple_to_list(BodyTuple,BodyGoals).
user:sdefun(func,body,comments):- fail.
user:custom_http_header(page):- fail.
user:strans(pIn-pOut,sIn-sOut,pre,post):- fail.
user:on_transition(toPage,sessionParamIn,sessionParamOut):- fail.
user:init_page(name):- fail.
user:app_name(name):- fail.
user:app_param(p,v):- fail.
user:form_method(m):- fail.
user:init_session_state(state):- fail.

user:hidden_param(session_id).

user:config_global_default(_,_):- fail.
user:config_desc(colors,'Color scheme').
user:config_type(colors,enum([red,green,black,white])).

% strans1(?PDelta,?S,?Pre,?Post) nd
% strans1(?PDelta,?S,?Pre,?Post,?Change) nd
%  convenience wrapper for user:strans/4 and user:strans/5
strans1(PageIn-PageOut,S,Pre,Post):-
        strans(PageIn-PageOut,S,Pre,Post).
strans1(_-PageOut,S,Pre,Post):-
        strans(PageOut,S,Pre,Post),
        nonvar(PageOut),
        \+ PageOut = _-_.
strans1(PageIn-PageOut,S,Pre,Post,D):-
        strans(PageIn-PageOut,S,Pre,Post,D).
strans1(_-PageOut,S,Pre,Post,D):-
        strans(PageOut,S,Pre,Post,D),
        nonvar(PageOut),
        \+ PageOut = _-_.
        
% use of user:transition/4 in user is deprecated - now use strans/4
make_transition(PageIn,PageOut,SIn,SOut):-
        user:transition(PageIn,PageOut,SIn,SOut).

% go from one state to another based on user rules
make_transition(PageIn,PageOut,SIn,SOut):-
        strans1(PageIn-PageOut,SIn-SOut,Pre,Post),
        debug(serval,'testing pre ~w',[Pre]),
        Pre,
        !,
        Post.
make_transition(PageIn,PageOut,SIn,SOut):-
        strans1(PageIn-PageOut,SIn,Pre,Post,SDelta),
        debug(serval,'testing pre ~w',[Pre]),
        Pre,
        !,
        Post,
        make_sdelta(SDelta,SIn,SOut).

make_sdelta(add(PL),SIn,SOut):-
        add_session_data(SIn,PL,SOut).
make_sdelta(null,S,S).

get_app_param(_,P,V):-
        % DEPREC!
        config_setting(P,V).

config_setting(Param,Val):-
        config_default(Param,Val),
        !.
config_setting(Param,Val):-
        config_global_default(Param,Val),
        !.
config_setting(Param,Val):-   % DEPREC
        app_param(Param,Val),
        %format(user_error,'deprecated ~w ~',[Param,Val]),
        !.
config_setting(_Param,null):- !.

%% start_server(+Port)
%
%  starts a server on a specified port
%  
start_server(Port) :-
        http_set_session_options([cookie(serval_cookie)]), % todo - param
        mutex_create(mutex_session_id), % make session-granting threadsafe
        http_server(reply, [port(Port)]).

% -- SERVAL --

test(PL):-
        catch(reply([search(PL)]),E,writeln(exception:E)).
test_serval(Params):-
        pvs_to_pairs(Params,Pairs),
        find_page(Pairs,Page,Pairs2),
        write_sterm(Pairs2,Page).

% swi http library defines params using '=' function; eg Param=Val
% we translate this to a [Param,Val] list
%  this makes is easier to deal with multivalued params:
pvs_to_pairs([],[]).
pvs_to_pairs([X=Y|Li],[[X,Y]|Lo]):-
        !,
        pvs_to_pairs(Li,Lo).
pvs_to_pairs([X|Li],[[X]|Lo]):-
        pvs_to_pairs(Li,Lo).

get_next_session_id(ID):-
            next_session_id(ID), % start new session
            ID2 is ID+1, % incrememnt session id
            retractall(next_session_id(_)), % increment
            assertz(next_session_id(ID2)).

% the reply/1 goal is called by the SWI http lib, in
% response to an end-user www transaction
reply(Request):-
        
        % translate P=V list to [P,V] list
        (member(search(Search),Request)
        ->   pvs_to_pairs(Search,PLin)
        ;    PLin=[]),

        debug(request,'~w',[Request]),
        
        % continue session or initiate a new one?
        (   getparam(PLin,session_id,ID)
        ->  PL=PLin              % 
        ;   with_mutex(mutex_session_id,
                       get_next_session_id(IDnumeric)),
            atom_number(ID,IDnumeric), % (?,+) number => atom

            % record stats
            get_time(Time),
            retractall(session_stats(ID)),
            assertz(session_stats(ID,[[init_time,Time],[request,Request]])),

            % user predicate: app can define default state
            (init_session_state(Init) -> true ; Init=[]),

            % add session to parameter list
            PL = [[session_id,ID]|PLin],

            % record session data
            findall([P,V],(member([P,V],Init),
                           assertz(session_data(ID,P,V))),_)),

        % session continued or initiataed; PL unified with
        % new param list. now we find what happened last session
        % transaction and continue from there
        findall([P,V],session_data(ID,P,V),PLstored),
        append(PL,PLstored,PLbegin),

        % -- CONTROLLER--
        % PLbegin is now unified with end-user parameters and
        % last/new session parameters. Find which page/view we
        % want based on application transitions
        find_page(PLbegin,Page,PLend),

        % -- VIEW --
        % Page is always non-compound
        write_sterm(PLend,Page).

% todo - transactional model, declarative

check_nonvar(X):-
        var(X),throw(var(X)).
check_nonvar(_).

% ===============================================================
% find_page(+StateIn,?Page,?StateOut) det private
% ===============================================================
find_page(L,P,[[page,P]|Lo]):-
        ngetparam(L,page,Pi,L1),
        !,
        debug(serval,'checking_transition ~w ~w',[Pi,L1]),
        (   make_transition(Pi,P,L1,Lo1)
        ->  check_nonvar(Lo1),
            debug(serval,'made_transition_to ~w',[P])
        ;   Lo1=L,
            debug(serval,'no_transition ~w',[Pi]),
            (Pi=null
             ->  (init_page(P)
                 ->  true
                 ;   throw(application_must_define_pred(init_page/1)))
            ;   P=Pi)),
        % CHANGED: if we are entering a new page, check for on_transition/3
        %(Pi \= P   --- NO: eg wing->wing vein
        %->  check_on_transition(P,Lo1,Lo)
        %;   Lo=Lo1).
        check_on_transition(P,Lo1,Lo).

check_on_transition(P,Si,So):-
        on_transition(P,Si,So),
        userlog(transition(P)),
        !.
check_on_transition(_P,S,S).

%  @pred getparam(+State,+Param,?Val) nondet
%  @pred getparam_as_num(+State,+Param,?NumericVal) nondet
%  @pred ngetparam(+State,+Param,?Val) det
%  @pred lgetparam(+State,+Param,?ValList) det
%% getparam(+State,+Param,?Val)
%% dgetparam(+State,+Param,?Val,+Default)
%% getparam_as_num(+State,+Param,?NumericVal)
%% ngetparam(+State,+Param,?Val)
%% lgetparam(+State,+Param,?ValList)
%
%  these prolog predicates allow for the fetching of parameter
%tag-value data from the session state. ngetparam will get a value with
%default = atom 'null' (TODO: change to null(_)?). getparam_as_num will fail
%unless that parameter has an atom value that can be coerced to a
%number.
%
%this is a standard prolog predicate and can be called as such; a
%corresponding function is part of serval-fun language (in which the
%input state and the return value is implicit)
%
%dgetparam - with default
%
%TODO: check nondet vs semidet
% sd
getparam_as_num(S,P,Num):-
        getparam(S,P,V),
        catch(atom_number(V,Num),_,fail).
dgetparam(S,P,V,Default):- (getparam(S,P,V) -> true ; V=Default).
ngetparam(S,P,V):- getparam(S,P,V,_S2),! ; V=null.
ngetparam(S,P,V,S2):- getparam(S,P,V,S2),! ; V=null,S2=S.

% (+PL,+P,?V) d
% (+PL,+P,?V,?PLout) d
getparam(S,P,V):- getparam(S,P,V,_).
getparam([[P,V]|L],P,V,L):- !.
getparam([P=V|L],P,V,L):- !.  % also allow N=V pairs
getparam([H|L],P,V,[H|L2]):-
        !,
        getparam(L,P,V,L2).

% (+PL,+P,?VL) d
% (+PL,+P,?VL,?PLout) d
lgetparam(S,P,VL):- lgetparam(S,P,VL,_).
lgetparam([],_,[],[]).
lgetparam([[P,V]|L],P,[V|VL],L2):-
        !,
        lgetparam(L,P,VL,L2).
lgetparam([PV|L],P,[V|VL],L2):-  % NEW -alternative style
        PV =.. [P|V],
        !,
        lgetparam(L,P,VL,L2).
lgetparam([H|L],P,VL,[H|L2]):-
        lgetparam(L,P,VL,L2).

%% submit_param(+SessionParams,?Text)
%  nondet
%
%  unifies with the text value of the submit param
%  
% (+,?) nd
% requires http_param_label/2
% allows submit params to be treated as atoms
submit_param(PL,N):-
        getparam(PL,submit,UserText),
        http_param_label(N,UserText).


% (+,+,+,?)
%  todo: make this have no side-effect, move db to find_page/2
%  [currently conflating params with session data]
set_session_data(S,P,V,[[P,V]|Sr]):-
        getparam(S,session_id,ID),
        retractall(session_data(ID,P,_)),
        assertz(session_data(ID,P,V)),
        delete(S,[P,_Vprev],Sr).
unset_session_data(S,P,Sr):-
        getparam(S,session_id,ID),
        retractall(session_data(ID,P,_)),
        delete(S,[P,_Vprev],Sr).

% add_session_data -
%  transforms list AND alters db
% (+,+,?)
add_session_data(S,[],S):-!.
add_session_data(S,[[P,V]|S1],Sr):-
        !,
        set_session_data(S,P,V,S2),
        add_session_data(S2,S1,Sr).
add_session_data(S,[[P]|S1],Sr):-
        !,
        unset_session_data(S,P,S2),
        add_session_data(S2,S1,Sr).
add_session_data(S,D,S2):-throw(add_session_data(S,D,S2)).

%% append_session_data(+S,+P,+Lin,?S2)
%  
append_session_data(S,P,Lin,S2):-
        (getparam(S,P,Lcur) -> true ; Lcur=[]),
        append(Lcur,Lin,L),
        add_session_data(S,[[P,L]],S2).

get_sdefun(F,R):-
        sdefun(F/_Comments,R),
        !.
get_sdefun(NS:F,R):-
        sdefun(NS:F/_Comments,R),
        !.
get_sdefun(F,R):-
        sdefun(F,R,_Comments),
        !.
get_sdefun(F,R):-
        sdefun(F/_Summary,R,_Comments),
        !.
get_sdefun(F,R):-
        sdefun(F,R).

/*
sdefun_to_xml(sdefun(F,Body,Comments),elt(sdefun,AL,EL),Vars):-
        !,
        AL=[],
        sdefun_to_xml(Body,EL2,Vars),
        EL=[elt(comments,[],Comments)|EL2].
sdefun_to_xml(X,elt(var,[name=V]),Vars):-
        var(X),
        !,
*/        

        

joinpl(_,[],[]).
joinpl(C,[X|T],L2):-
        joinpl(C,T,L),
        (X = (P=V)
        ->   escape_param(V,V2),concat_atom([P,'=',V2],X2)
        ;    X2=X),
        (T=[]
        ->  L2=[X2|L]
        ;   L2=[X2,C|L]).

% URI chars muse be escaped
escape_param(V,V2):-
        atom_codes(V,CL),
        escape_codes_to_atoms(CL,AL),
        concat_atom(AL,V2).

escape_codes_to_atoms([],[]).
escape_codes_to_atoms([C|CL],[C2|CL2]):-
        escape_code_to_atom(C,C2),
        escape_codes_to_atoms(CL,CL2).

escape_code_to_atom(C,A):-
        ( (C >= 97, C =< 122)
        ; (C >= 65, C =< 90)
        ; (C >= 48, C =< 57)
        ; C=95
        ; C=46
        ; C=45),
        !,
        atom_codes(A,[C]).
escape_code_to_atom(C,A):-
        sformat(A,'%~16r',C).


make_href(S,PL,Href):-
        % users PL takes precedence over hidden params
        findall(P=V,(hidden_param(P),not(member(P=_,PL)),getparam(S,P,V)),PLh),
        append(PL,PLh,PLall),
        joinpl('&',PLall,PLflat),
        app_name(N),
        concat_atom([N,'?'|PLflat],Href).

:- mode write_sterm(+) is det.
write_sterm(Term):-
        X=xml([]),
        xmldoc_start(X),
        write_sterm([],X,Term),
        nl.

:- mode write_sterm(+,+) is det.
write_sterm(S,Term):-
        % TODO: other content types
        %(   custom_http_header(Term)
        %->  true
        %;   format('Content-type: text/html~n~n', [])),
        X=xml([]),
        xmldoc_start(X),
        write_sterm(S,X,Term),
        nl.

%% write_sterm(+SessionParams, +XmlStack, +HtmlFuncTerm)
%  
%  evaluates an html function term, writing html/xml as a side-effect
:- mode write_sterm(+,+,+) is det.
%write_sterm(_D,_X,T):- debug(serval,'write_sterm ~',[T]),fail.
write_sterm(_D,_X,[]):-!.
write_sterm(D,X,[H|L]):-
        !,
        write_sterm(D,X,H),
        write_sterm(D,X,L).
write_sterm(D,X,F):-
        pfunc0(D,F,R),
        !,
        write_sterm(D,X,R).
write_sterm(_,_,null(V)):-
        var(V),
        !.
write_sterm(D,X,F):-
        get_sdefun(F,R),
        !,
        write_sterm(D,X,R).
write_sterm(D,X,Term where Goal):-
        (   findall(Term,Goal,[Term0|_])
        ->  write_sterm(D,X,Term0)
        ;   true).
write_sterm(D,X,F):-
        F =.. [FN,_|_],         % must have argument; eg b/0 not allowed
        builtin_element(FN),    % standard html
        !,
        write_sterm(D,X,html:F).
write_sterm(D,X,s:F):-           % serval builtin namespace
        write_sterm(D,X,F).
write_sterm(D,X,xml:F):-        % xml namespace - direct
        write_sterm(D,X,html:F).
write_sterm(D,X,html:F):-        % html namespace
        F =.. [FN|ArgL],
        % we assume builtin_element(FN),
        !,
        getatts(ArgL,AttL,EL), % TODO - check for 0 arg preds
        % TODO: funcs on attl?
        xmlnode_start(X,Xo,FN,AttL),
        write_sterm(D,Xo,EL),
        xmlnode_end(Xo,X,FN).
write_sterm(D,X,F):-
        F =.. [ilink,Body|PL],
        !,
        make_href(D,PL,HRef),
        write_sterm(D,X,a(href=HRef,Body)).
write_sterm(D,X,F):-
        F =.. [sform,Nform,PL|AL],
        !,
        app_name(Napp),
        form_method(Method),
        write_sterm(D,X,form(name=Nform,action=Napp,method=Method,
                            AL,hidden_excl(PL))).
write_sterm(D,X,this):-
        !,
        write_sterm(D,X,'serval').
write_sterm(D,_X,session(S)):-
        !,
        D=S.
write_sterm(D,X,F):-
        F =.. [in,D|L],
        !,
        write_sterm(D,X,L).
write_sterm(D,_X,getparam(P,V,Def)):-
        debug(ontol_rest,'fetching ~w from ~w',[P,D]),
        dgetparam(D,P,V,Def).
write_sterm(D,_X,lgetparam(P,V)):-
        lgetparam(D,P,V).
write_sterm(D,_X,getparam(P,V)):-
        ngetparam(D,P,V).
write_sterm(D,X,getparam(P)):-   % no V arg - write straight out
        userlog(deprecated(getparam(P))),
        write_sterm(D,X,paramval(P)).
write_sterm(D,X,paramval(P)):-   % no V arg - write straight out
        (getparam(D,P,V)->
            true
        ;
            V=null
        ),
        write_sterm(D,X,data(V)).
write_sterm(_,_,log(M)):-
        userlog(M).
write_sterm(_,_,debug(D,Fmt,Args)):-
        debug(D,Fmt,Args).
write_sterm(_,_,debug(D,Args)):-
        debug(D,'~w',Args).
write_sterm(D,X,setof(V,C)):-
        setof(V,C,L),
        write_sterm(D,X,L).
write_sterm(D,X,ufindall(V,C)):-
        (   setof(V,C^C,L)
        ->  write_sterm(D,X,L)
        ;   true).
write_sterm(D,X,findall(V,C)):-
        findall(V,C,L),
        write_sterm(D,X,L).
write_sterm(D,X,Term forall Goal):-
        findall(Term,Goal,L),
        write_sterm(D,X,L).
write_sterm(D,X,Term forall_unique Var in Goal):-
        solutions(Var,Goal,Vals),
        solutions(Term,member(Var,Vals),L),
        write_sterm(D,X,L).
write_sterm(D,X,Term forall_unique JC/Goal):-
        solutions(Term,Goal,L),
	list_addj(L,JC,L2),
        write_sterm(D,X,L2).
write_sterm(D,X,Term forall_unique Goal):-
        solutions(Term,Goal,L),
        write_sterm(D,X,L).
write_sterm(D,X,count(V,C)):-
        findall(V,C,L1),
        (setof(V,member(V,L1),L) % uniquify
        ->   length(L,Num)
        ;    Num=0),
        write_sterm(D,X,data(Num)).
write_sterm(D,X,map(V,LF,L)):-
        findall(LF,member(V,L),RL),
        write_sterm(D,X,RL).
write_sterm(D,X,jmap(JC,V,LF,L)):-
        findall(LF,member(V,L),RL),
        write_sterm(D,X,join(JC,RL)).
write_sterm(D,X,nmap(V,NV,LF,L,From)):-
        (var(From)->throw(instantiation_err(nmap(V,NV,LF,L,From)));true),
        forall(nmember(V,Num,L),
               (   NV is Num+From,
                   write_sterm(D,X,LF))).
write_sterm(_D,_X,call(C)):-
        (call(C)
        ->  true
        ;   throw(problem_calling_prolog_goal(C))).
write_sterm(_,_,doc:_).
write_sterm(D,X,(C -> Then ; Else)):-
        (   C
        ->  R=Then
        ;   R=Else),
        write_sterm(D,X,R).
write_sterm(D,X,if(C,then:Then,else:Else)):-
        !,
        (   C
        ->  R=Then
        ;   R=Else),
        write_sterm(D,X,R).
write_sterm(D,X,if(C,then:Then)):-
        (C->  write_sterm(D,X,Then) ; true).
write_sterm(D,X,if(C,Then)):-
        (C->  write_sterm(D,X,Then) ; true).
write_sterm(D,X,unless(C,Then)):-
        (C->  true ; write_sterm(D,X,Then)).
write_sterm(D,X,if(C,then:Then)):-
        (C
        ->  write_sterm(D,X,Then)
        ;   true).
write_sterm(_D,X,data(Body)):-
        !,
        xmlnode(X,Body).
write_sterm(_D,_X,noescnl(Body)):-
        !,
        writeln(Body).
write_sterm(_D,_X,noesc(Body)):-
        !,
        write(Body).
write_sterm(_D,X,F):-
        F =.. [_FN],
        !,
        xmlnode(X,F).
write_sterm(D,X,F):-
        diagnose_page_err(F),
        throw(write_sterm(D,X,F)).

diagnose_page_err(F):-
        F =.. [FN|Args],
        length(Args,NumArgs),
        write(cannot_evaluate_func(FN/NumArgs)),
        !.
diagnose_page_err(F):-
        write(cannot_evaluate_func(F)).

list_addj([],_,[]).
list_addj([X],_,[X]).
list_addj([X|L],J,[X,J|L2]):-
	list_addj(L,J,L2).

getatts([],[],[]).
getatts([A=V|L],[A=V|AL],EL):-
        !,
        getatts(L,AL,EL).
getatts([E|L],AL,[E|EL]):-
        !,
        getatts(L,AL,EL).

nmember(V,NV,L):-
        nmember(V,NV,L,0).

nmember(H,NV,[H|_L],NV).
nmember(V,NV,[_H|L],N):-
        N2 is N+1,
        nmember(V,NV,L,N2).
        
nmap(L,L2):-
        nmap(1,L,L2).
nmap(_,[],[]).
nmap(N,[H|L],[N/H|L2]):-
        N2 is N+1,
        nmap(N2,L,L2).

%pfunc0(D,F,R):-
%        pfunc(D,F,R),
%        !.
pfunc0(D,F,R):-
        builtin_pfunc(D,F,R).
        
                                % GENERIC - use different predicate??
builtin_pfunc(_S,hidden,hidden_excl([])).
builtin_pfunc(S,hidden_excl(ExclL),L):-
        findall(input_hidden(P,V),(hidden_param(P),
                                   not(member(P,ExclL)),
                                   getparam(S,P,V)),L).
            
builtin_pfunc(_S,
     input_hidden(P,V),
     [input(type=hidden,name=P,value=V)]).

builtin_pfunc(_S,
     join(C,L),
     L2):-
        (L=[H|T]->
            findall([C,X],member(X,T),L1),
            L2=[H|L1]
        ;
            L2=[]
        ).

builtin_pfunc(_S,app_home,N):- app_name(N).

builtin_pfunc(_S,
     qtable(T),
     table(border=1,TRL)):-
        findall(tr(TDL),(member(R,T),
                         findall(td(Cell),member(Cell,R),TDL)),TRL).


builtin_pfunc(_,
              include_file(F),
              [call(import_file(F,Data)),noesc(Data)]).

builtin_pfunc(_S,
              write_file(F,X),
              []):-
        tell(F), % TODO - allow for nesting
        write_sterm(X),
        told.

import_file(F,Data):-
        expand_file_search_path(F,Fx),
        open(Fx,read,H,[]),
        read_stream_to_codes(H,CL),
        atom_codes(Data,CL),
        close(H).

userlog(M):-                      
        %write(user_error,M),nl(user_error).
        debug(serval,'~w',[M]).


/*

  built-in primitive functions

  these evaluate to strings corresponding to the functor

  MAY NOT BE COMPLETE!!!!!!! TODO: auto???
  
*/
builtin_element(body).
builtin_element(head).
builtin_element(title).
builtin_element(html).
builtin_element(link).
builtin_element(img).

builtin_element(script).

builtin_element(table).
builtin_element(tr).
builtin_element(td).
builtin_element(th).

builtin_element(span).

builtin_element(font).

builtin_element(a).

builtin_element(div).
builtin_element(pre).
builtin_element(b).
builtin_element(i).
builtin_element(p).
builtin_element(hr).
builtin_element(br).
builtin_element(center).
builtin_element(h1).
builtin_element(h2).
builtin_element(h3).
builtin_element(h4).

builtin_element(ul).
builtin_element(li).
builtin_element(dl).
builtin_element(dd).
builtin_element(dt).

builtin_element(form).
builtin_element(input).
builtin_element(textarea).
builtin_element(text).
builtin_element(select).
builtin_element(option).

%  @author Chris Mungall
%  @version  $Revision: 1.37 $
%  @date  $Date: 2006/01/25 02:17:22 $
%  @license LGPL
%
%
%  ---+ Name
%  serval: a lightweight prolog application server
%
%  ---+ Synopsis
%
%  :- use_module(bio(serval)).
%
%  init_page(intro).
%  sdefun(intro,
%         html(head(title(intro)),
%              body(h1('hello world')))).
%
%  run:-
%      start_server(8080),
%      prolog.
%  
%  ---+ Description
%  
%  serval makes it easy to write web applications using a
%model/view/controller paradigm. the model is defined in prolog, the
%controller is defined using strans/4 predicates, and the view is
%defined using using a functional language (specified as prolog terms
%which evaluate to text/XML/HTML). Together these constitute a
%domain-specific language (DSL)
%
%  ---++ The serval model
%
%  each web application can be viewed as a state machine. Each "page"
%in the application is a state, and end-user actions generate state
%transitions. Each page/state is specified as a zero-argument funtion,
%which can in turn call other functions; These functions constitute the
%application 'view'
%
%  ---++ MVC: VIEW
%
%  Views are specified as function definitions in a lisp-like language
%with prolog syntax. Here is a user-defined function for a small web
%page:
%  
%  ==
%  sdefun(intro,
%         html(head(title(intro)),
%              body(h1('hello world')))).
%  ==
%
%  Top-level functions like this one have zero arguments, and can also
%serve as 'states' - a session is a linear sequence of states
%  
%  Here is another example - a user query form for a database web
%interface style application. functions can be namespaced just like
%with prolog modules. functions can call other functions, which may
%take prolog variables as arguments:
%
%  ==
%  sdefun(main,
%         html:html(html:head(title('query page')),
%                   html:body(s:sform(query_form,[],
%                                     my:textfield('Enter text to search on',
%                                                  search_text))))).
%  sdefun(my:textfield(Text,Name),
%         html:div(class=textfield,
%                  html:h2(Text),
%                  html:input(type=textfield,name=Name)))
%  ==
%
%  ---+++ Serval functions
%  
%  html elements are builtin functions; the application can define its
%own functions, or use the builtin serval lightweight functional
%programming language. this functional language (TODO: LIST BUILTINS) has
%some standard operators and mixes some prolog idioms.
%
%  consider the following example: a function for showing an html table
%containing a summary of a list of data entities (eg genes). the
%function takes one argument, a (prolog) list of IDs. the function
%makes use of a prolog predicate gene_name/2 which must be defined in
%the model/logic section. this also uses the builtin serval function
%findall(Term,Goal) in which Goal is some non-deterministic prolog
%goal. This is similar to a standard functional map operator, but
%mixes non-deterministic prolog idioms with a functional idiom. Eg:
%
%  ==
%  sdefun(query_results,
%         html(head(title('query results)),
%              getparam(gene_ids,IDs),
%              body(div(class=genes,
%                       gene_table(IDs))))).
%
%  sdefun(gene_table(IDs),
%         table(border=1,
%               tr(th('ID'),th('Name')),
%               findall(tr(td(ID),td(Name)),    % serval terms
%                       (member(ID,IDs),gene_name(ID,Name))))).  % prolog goal
%  ==
%
%  The gene_table(IDs) function is a reusable widget for showing a
%table of gene IDs and names
%         
%  In the above, we are assuming the existence of a session parameter
%'gene_ids'. These parameters come from the CONTROLLER, which executes
%LOGIC based on user-actions.
%
%  ---++ MVC CONTROLLER: TRANSITIONS
%
%  A transition is a change in state, usually based on the user going
%from one page to another (though this is not always the case - eg AJAX
%applications may execute 'transitions' to get data from the server,
%although these may be null transitions eg they do not change state)
%  
%  transitions are specified as prolog rules using strans/4 and
%strans/5. A transition has certain preconditions which must be
%satisfied before the transition can take place. The preconditions are
%the current page/state, and a standard prolog list of predicates that
%must succeed.
%
% a transition also contains actions: these modify the current session
%state. the session state cannot be modified in the views, only in the
%controller.
%
%  ==
%  strans(query_results,StateIn,
%         
%          % PRECONDITIONS:
%          %  the 'submit' param must be 'search' and the
%          %  value of the param 'data_class' must be 'gene'
%          (   submit_param(StateIn,search_text),
%              getparam(S,data_class,gene)),
%
%          % ACTIONS:
%          %  the database is searched for genes and the
%          %  resulting ID set is added to the state
%          (  ngetparam(StateIn,search_text,Search),
%             % this predicate should be defined in the model/logic
%             % part of the application
%             gene_ids_by_search_text(Search,IDs)),
%
%          % CHANGES:
%          add([[gene_ids,IDs]])).
%  ==
%
%  ---++ MVC: LOGIC
%
%  The logic (or model) part of the application is normal prolog. For
%our application above, we will need to define a predicate for
%searching the prolog database, like this:
%  
%  ==
%  % assumes some fact gene_name(ID,Name), loaded at startup
%
%  gene_ids_by_search_text(Search,IDs):-
%          setof(ID,gene_id_by_search_text(Search,ID),IDs),
%          !.
%  gene_ids_by_search_text(Search,[]).
%  gene_id_by_search_text(Search,ID):-
%          gene_name(ID,Name),
%          sub_atom(Name,_,_,_,Search).
%  ==
%  
%  If we combine the four sections of code above, we have a complete
%(but limited!) serval application. For more examples, see the
%examples/serval part of the blip distribution
%  
%  ---++ Sessions
%
%  serval handles session management for you. The first time a user
%visits the application, they are granted a session ID. The application
%can then keep track of persistent data associated with that session.
%
%note that the same prolog database is used by all users of an
%application hosted on one serval instance. Anything you assert in the
%database will be global. Data that is local to any one user session
%should be manipulated in the transition predicates using
%+StateIn and ?StateOut
%  
%  ---++ Prolog predicates
%
%  Your application can define the following predicates (typically in
%the 'user' module)
%
%  
%
%  * sdefun(func,body)
%  * sdefun(func,body,comments)
%  * app_name(name)
%  * form_method(m)
%  * hidden_param(p)
%  * init_session_state(state)
%
%  
%  
%  ---++ Dependencies
%  
%  this module depends on the standard SWI http library, and the
%  @l xml_writer
%  library (part of the blip distribution)
%
%  ---++ Mixing prolog and serval functions
%
%  One possibly point of confusion is the mixing of prolog and 
%functions calls.
%
%A serval application consists of definitions of serval-language
%functions, which consist of calls to other functions, either
%application-user-defined or builtins.
%
%Some functions take prolog terms as one or more of their
%arguments. One obvious example is call(Goal) which calls a prolog Goal
%(deterministically - ie it will succeed at most once).
%
%Other examples are builtin functions such as findall(Term,Goal)
%
%  ---++ SERVAL FUNCTIONS
%
%  These are the builtin serval functions. Some functions take an exact
%number of args; some take a variable number of args. Variable numbers
%of args are represented using the notation <Foo...>
%
%  
%
%  * <HTML-ELEMENT>(<Body...>)
%
%  serval has most common html tags as builtins. If in doubt, use the html: namespace
%
%  * ilink(Body,<Atts..>)
%
%  Create an internal link (ie within the same serval application)
%
%  Example:
%  ==
%  ilink(h3(help),page=help)
%  ==
%
%  * sform(Name,Params,<Body...>)
%
%  an HTML Form. Automatically includes hidden variables defined with
%user:hidden_param/1
%  
%  * this
%  * session(?S)
%
%  unifies S with the session parameter list. S can then be used with prolog predicates that require state info, eg getparam/3
%  
%  * in(?S,<Body...>)
%
%  unifies S with the session parameter list and executes <Body...>
%
%  Example:
%  
%  ==
%  sdefun(show_result,
%         in(S,
%            h1('Result'),
%            if(getparam(S,id,ID),
%               then: show_result(ID),
%               else: h3('no ID passed')))).
%  ==
%  
%  * getparam(+ParamName,?Value,+DefaultValue)
%
%  unifies Value with the current value of ParamName in the current session
%
%  if not supplied, defaults to DefaultValue
%  
%  * getparam(+ParamName,?Value)
%
%  unifies Value with the current value of ParamName in the current session
%
%  if not supplied, defaults to null
%
%  * getparamval(+ParamName)
%
%  returns values of ParamName is current session
%
%  example:
%  
%  ==
%  sdefun(hello,h3('hello ',getparam(name))).
%  ==
%  
%  * userlog(Msg)
%
%  writes Msg on user_error stream
%  
%  * findall(Body,Goal)
%
%  for every solution of Goal [prolog], writes out Body
%
%  ==
%  sdefun(show_results,
%         table(lgetparam(id,IDs),
%               findall(tr(td(ID),td(Name)),
%                       (member(ID,IDs),id_to_name(ID,Name))))).
%  ==
%  
%  * ufindall(Body,Goal)
%
%  as findall(Body,Goal), but only unique solutions of Goal
%  
%  * setof(Body,Goal)
%
%  as unfindall - fails and throws an exception if no solutions
%  
%  * count(Template,Goal)
%
%  number of unique solutions Template to Goal
%  
%  * map(Element,Body,List)
%  * nmap(Element,Number,Body,List,From)
%  * call(Goal)
%
%  Goal is any prolog goal. This is primarily for the purpose of
%unifying variables
%  
%  * if(Goal,then:ThenBody,else:ElseBody)
%  * if(Goal,then:Body)
%  * if(Goal,Body)
%  * unless(Goal,Body)
%  * data(Body)
%
%  do not evaluate Body - return as-is. escape XML characters
%
%  * noesc(Body)
%
%  as data(Body), but does not escape XML characters
%
%  
%
%  ---++ Running the server
%
%  the server is started with start_server/1
%
%  ---++ Hot swapping
%
%  It is often a good idea to start the server in an interactive prolog
%session (or call prolog/0 in your server program, see the example in
%the synopsis).
%
%  As well as being able to test parts of your application
%interactively, you can modify the source of your application and
%reconsult simply by calling make/0. This allows for a much faster
%test-and-code cycle
%  
%  ---++ TODO
%
%  more consistent usage of terminology: state/page vs session params
%  vs session state....
%
%  Cookies - instead of session_id - current impl allows session theft
%  however, sessionIDs make testing easier. combine?
/** <module>
  @pred config_setting(+Name,?Value)
  det
  configuration setting for this session

  the application can allow the user to configure this
  recorded via http_session_data(?Term)

  this takes precedence over config_default, which is per-application
  */
