/* -*- Mode: Prolog -*- */

:- module(io,
          [load_biofile/1,
           load_biofile/2,
           load_biofiles/1,
           load_bioresource/1,
           load_bioresources/1,
           load_factfile/1, % needs to be exported to load into a mod

           unload_biofile/2,
           unload_bioresource/1,
           write_biofile/1,
           write_biofile/2,

           format_module_xmlmap/3,
           
           url_cachepath/2,
           add_to_include_list/2,
           format_module/2,
           file_to_prolog_cmd/2,
           filter_fact/2,
           
           consult_bioconf/0,
           consult_bioconf/1
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(dbmeta),[insert_fact/1,insert_facts/2]).
:- use_module(bio(mode)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_http_plugin')).

:- encoding(utf8).

:- multifile user:bioresource/2,user:bioresource/3,user:bioresource/4.
:- multifile user:uri_resolution/2.

:- dynamic user:max_cached_file_age_seconds/1.
:- multifile user:max_cached_file_age_seconds/1.

:- multifile
        user:show_db_summary/1.

:- multifile cached_format/1,redirect_stdout/1,write_all/3,write_all/2,write_all/1.
:- multifile
        file_to_prolog_cmd/2,
        format_module/2,
        filter_fact/2,
        format_writer/2.

io:filter_fact(_,_):- fail.

%user:load_factfile(File):- bioprolog_util:load_factfile(File).
user:show_db_summary(mod):- fail.

% bioresource(Name,Path)
% bioresource(Name,Path,Format)
% bioresource(Name,Path,pro,Format)
%
%  This predicate should be defined in the user module, like this:
%
%  ==
%user:file_search_path(my_ontology_dir,'/users/me/ontologies').
%user:bioresource(go,my_ontology_dir('gene_ontology.obo'),obo)
%  ==
%
%  place the line above in your =|~/.plrc|= file or in your
%=|bioconf.pro|= file

%**********************************************************
%  LOADING
%***********************************************************


%% load_bioresource(+Res)
%  
%  
%% load_bioresources(+ResList)
%
%  Loads a file, first looking up the location of the resource in
%bioresource/2 in the user module
%
%  You can specify the lookup table like this:
%
%  ==
%  user:bioresource(my_resource,'~/src/mydata.pro').
%  user:bioresource(wine,my_ontologies('wine.owl'),owl).
%  user:bioresource(humpath,'/users/me/data/reactome.sbml',sbml).
%  user:bioresource(cell,obo_anat('cell_type/cell.pro'),pro,ontol_db).
%  user:bioresource(fb,godata('gene_association.fb.gz'),gzip(go_assoc)).
%  ==
%
%  in future it will be possible to specify URLs too
load_bioresources(RL):-
        forall(member(R,RL),load_bioresource(R)).
load_bioresource(R):-
        user:bioresource(R,L),
        is_list(L),
        debug(load,'loading list: ~w',[L]),
        !,
        maplist(load_bioresource,L).
load_bioresource(R):-
        user:bioresource(R,P),
        !,
        expand_bioresource_search_path(P,Px),
        load_factfile(Px,user).
load_bioresource(R):-
        user:bioresource(R,P,pro,Mod),
        !,
        expand_bioresource_search_path(P,Px),
        load_biomodule(Mod),
        load_factfile(Px,Mod).
load_bioresource(R):-
        user:bioresource(R,odbc_connect(_Db,_),Schema),
        !,
        ensure_loaded(bio(rdb_util)),
        %rdb_util:rdb_connect(Rdb,Db), % todo - load sqlmaps
        rdb_util:rdb_connect(Rdb,R),    % todo - load sqlmaps
        nb_setval(rdb,Rdb), % DEPRECATED
        assert(sql_compiler:sqlschema_connection(Schema,Rdb)).
load_bioresource(R):-
        user:bioresource(R,P,Fmt),
        !,
        expand_bioresource_search_path(P,Px),
        load_biofile(Fmt,Px).
load_bioresource(F):-
        atom(F),
        concat_atom(L,'/',F),
        L=[_,_|_],
        !,
        Term=..L,
        load_bioresource(Term).
load_bioresource(R):-
        throw(no_such_resource_as(R)).

% a bioresource can be file or URL - download all URLs to cached file
expand_bioresource_search_path(file(File),File):-
        !.
expand_bioresource_search_path(url(URL),Px):-
        !,
        (   debug(load,'expanding URL ~w',[URL]),
            download_url(URL,Px)
        ->  true
        ;   throw(error(cannot_download(URL)))).
expand_bioresource_search_path(P,Px):-
        expand_file_search_path(P,Px).

url_cachepath(URL,Px):-
        url_to_safe_filename(URL,LocalName),
        debug(load,'safe filename = ~w',[LocalName]),
        (   expand_file_search_path(data_cache(LocalName),Px)
        ->  true
        ;   atom_concat('/tmp/',LocalName,Px)).


%% download_url(+URL,?Path) is det
% Maps URL to a cached file (Path). If Path is recent then do nothing, otherwise download URL to Path
%
% contents will first be fetched to a temp file, so as not to disturb cache
%
% relies on data_cache being set
% loads URL into Px, after checking cache
% TODO: register time of last check properly, not using filesystem
download_url(InURL,Px):-
        debug(load,'Mapping: ~w --first checking cache to see if download required',[InURL]),
        (   user:uri_resolution(InURL,URL) % user can set up their own registry
        ->  true
        ;   URL=InURL),
        url_cachepath(URL,Px),  % e.g. $HOME/.blip/data_cache/http:::purl.org:obo:obo:CHEBI.obo
        ensure_directory_exists_for(Px),
        !,
        download_url2(URL,Px).

%% download_url2(+URL,+Px) is det
download_url2(_,Px):-
        path_lock(Px,Lock),
        debug(load,'Testing lockfile: ~w',[Lock]),
        exists_file(Lock), % locked: someone else downloading latest version
        !,
        debug(load,'file locked; will not attempt download: ~w',[Lock]),
        (   exists_nonempty_file(Px)
        ->  true                % use existing one, even if stale
        ;   throw(io(locked('Cannot access file, index is locked. Try again later',
                            Px)))).

download_url2(URL,Px):-         % not locked
        path_lock(Px,Lock),
        atom_concat(Px,'.timestamp',Timestamp),
        \+ exists_file(Lock),
        !,
        get_time(Time),
        debug(load,'current time: ~w',[Time]),
                                % check cache is present and recent
        (   exists_nonempty_file(Px), % TODO: policy to force a refresh?
            file_age(Px,Age),
            \+size_file(Px,0 )  % file must have contents

        ->  (   age_exceeds_threshold(Age), % potentially stale; check
                debug(load,'age of  ~w exceeds threshold',[Px]),
                \+ ((file_age(Timestamp,TSAge),
                     debug(load,'checking timestamp: ~w ~w',[Timestamp,TSAge]),
                     \+ age_exceeds_threshold(TSAge)))
            
            ->  (   tmp_file(wget,TmpPx),
                    file_from_url(TmpPx,URL) % succeeds if URL accessible
                ->  (   file_contents_identical(TmpPx,Px)
                    ->  touch_file(Timestamp) % do not check again for a while
                    ;   rename_file(TmpPx,Px), % make the tmp file live in the cache
                        touch_file(Timestamp),
                        touch_file(Px)) % cache timestamp & trigger for recompiling to prolog
                ;   !,
                    %touch_file(Timestamp),
                    format(user_error,'Cannot access ~w -- Using cached file ~w which is ~ws old~n',[URL,Px,Age])) % URL inaccessable
                
            ;   debug(load,'Timestamp File ~w exists, has content, and a recent checked showed the external site to have identical comments to cached version. using as-is',[Px])) % 

        ;   file_from_url(Px,URL)). % cache stale or non-existent: download

exists_nonempty_file(P):-
        exists_file(P),
        \+size_file(P,0).

path_lock(Px,Lock):-
        atom_concat(Px,'.lock',Lock).

remove_stale_lock(Lock):-
        exists_file(Lock),
        file_age(Lock,Age),
        Age > 300,              % 5 mins
        !,
        delete_file(Lock).
remove_stale_lock(_).

file_age(P,Age):-
        exists_file(P),
        get_time(Time),
        time_file(P,PTime),   % last mod time
        Age is Time-PTime,
        debug(load,'now: ~f file: ~w: filetime: ~f age: ~f',[Time,P,PTime,Age]).
        

        
%% file_from_url(+Path,+URL) is det
% downloads contents of URL into Path on disk
file_from_url(Px,URL):-
        path_lock(Px,Lock),
        debug(load,'  Locking;  lockfile = ~w',[Lock]),
        touch_file(Lock),
        (   expand_file_search_path(path_to_wget(wget),Wget)
        ->  true
        ;   Wget=wget),
        !,
        % 2010-11-13 : added -N option: don't re-retrieve files unless newer than local
        sformat(Cmd,'~w -N -q -O \'~w.tmp\' \'~w\' && mv \'~w.tmp\' \'~w\'',[Wget,Px,URL,Px,Px]),
        debug(load,'Executing ~w',[Cmd]),
        (   shell(Cmd)
        ->  true
        ;   debug(load,' FAILED: ~w',[Cmd]),
            fail),
        delete_file(Lock).

age_exceeds_threshold(_):-
        getenv('BLIPKIT_DISABLE_WGET',_),
        !,
        fail.
age_exceeds_threshold(Age):-
        debug(load,'Testing if age: ~w exceeds threshold',[Age]),
        user:max_cached_file_age_seconds(MaxAge),
        debug(load,'  User-defined threshold',[MaxAge]),
        Age > MaxAge,
        debug(load,'Age: ~w exceeds threshold: ~w',[Age,MaxAge]),
        !.
age_exceeds_threshold(Age):-
        \+ user:max_cached_file_age_seconds(_),
	MaxAge=86400,
        Age > MaxAge,
        !,
        debug(load,'Age: ~w exceeds default threshold: ~w',[Age,MaxAge]).

file_contents_identical(F1,F2):-
        sformat(Cmd,'cmp \'~w\' \'~w\' > /dev/null 2>1',[F1,F2]),
        debug(load,'Executing ~w',[Cmd]),
        shell(Cmd,Status),
        Status=0.               % unify after
        
touch_file(F):-
        sformat(Cmd,'touch \'~w\'',[F]),
        debug(load,'Executing ~w',[Cmd]),
        shell(Cmd).
        
directory_name_part(Dir,Name):-
        concat_atom(Parts,'/',Dir),
        reverse(Parts,[Name|_]).

url_to_safe_filename(URL,Name):-
        concat_atom(Parts,'/',URL),
        concat_atom(Parts,':',Name).


% util
copy_file(In,Out):-
        open(In,read,InStream,[]),
        open(Out,write,OutStream,[]),
        copy_stream_data(InStream,OutStream),
        close(OutStream),
        close(InStream).

% recursively create a directory
ensure_directory_exists_for(F):-
        file_directory_name(F,Dir),
        ensure_directory_exists(Dir).
ensure_directory_exists(Dir):-  %  make_directory_path/1 in 5.11.9
        (   exists_directory(Dir)
        ->  true
        ;   file_directory_name(Dir,DirUp),
            ensure_directory_exists(DirUp),
            make_directory(Dir)).
        

%% load_biofiles(+InputFileList)
%   loads a list of files - format is guessed from suffix
load_biofiles(FL):-
        forall(member(F,FL),load_biofile(F)).
%% load_biofile(+InputFile)
%   loads a file - format is guessed from suffix
load_biofile(InputFile):-
        atom(InputFile),
        sub_atom(InputFile,0,_,_,'http:'),
        !,
        load_biofile(url(InputFile)).
load_biofile(InputFile):-
        atom(InputFile),
        sub_atom(InputFile,0,_,_,'https:'),
        !,
        load_biofile(url(InputFile)).
load_biofile(InputFile):-
        atom(InputFile),
        sub_atom(InputFile,0,P,_,'file:'),
        sub_atom(InputFile,P,_,0,InputFile2),
        !,
        load_biofile(InputFile2).
load_biofile(InputFileIn):- % special case - e.g. mygenome-genome_db.pro
        expand_bioresource_search_path(InputFileIn,InputFile),
        file_name_extension(X, pro, InputFile),
	sub_atom(X,_,_,E,'-'),
	sub_atom(X,_,E,0,Mod),
	\+ sub_atom(Mod,_,_,_,'-'),
        \+ \+ format_module(_,Mod),
	catch(load_biofile(Mod:pro,InputFile),
              _,
              fail),
        !.
load_biofile(InputFileIn):- % special case - gzip
        expand_bioresource_search_path(InputFileIn,InputFile),
        file_name_extension(F2, gz, InputFile),
        file_name_extension(_Base, Fmt, F2),
        !,
        load_biofile(gzip(Fmt),InputFile).
load_biofile(InputFileIn):-
        expand_bioresource_search_path(InputFileIn,InputFile),
        (   file_name_extension(_Base, Fmt, InputFile)
        ->  load_biofile(Fmt,InputFile)
        ;   throw(cannot_determine_format(InputFile))).
%% load_biofile(+Fmt,+InputFile)
%   loads a file - you have to specify the full path
%  
%  If Fmt is typically ground (nonvar); if non-ground, will try and
%determine from file_name_extension
load_biofile(Fmt,InputFile):-
        var(Fmt),
        !,
        load_biofile(InputFile).
load_biofile(Fmt,InputFile):-
        atom(InputFile),
        sub_atom(InputFile,0,_,_,'http:'),
        !,
        load_biofile(Fmt,url(InputFile)).
load_biofile(Ext=Fmt,InputFileIn):-
        !,
        expand_bioresource_search_path(InputFileIn,InputFile),
        (   file_name_extension(_Base, Ext, InputFile)
        ->  load_biofile(Fmt,InputFileIn)
        ;   load_biofile(InputFileIn)).
load_biofile(Fmt,InputFileIn):-
        expand_bioresource_search_path(InputFileIn,InputFile),
        load_special(Fmt,InputFile),
        !.
load_biofile(Fmt,InputFile):-
        format_synonym(ActualFmt,Fmt),
        !,
        load_biofile(ActualFmt,InputFile).
load_biofile(Fmt,InputFileIn):-
        expand_bioresource_search_path(InputFileIn,InputFile),
        format_parser(Fmt,Parser),
        ensure_loaded(bio(Parser)),
        forall(schema_dependency(Parser,Dep),
               ensure_loaded(bio(Dep))),
        parse_and_load(Fmt,InputFile),
        !.
load_biofile(pro,F):-
        !,
        load_factfile(F,user).
load_biofile(pro(Mod),F):-
        !,
        ensure_loaded(bio(Mod)),
        load_factfile(F,Mod).
load_biofile(Mod:pro,F):-
        !,
        ensure_loaded(bio(Mod)),
        load_factfile(F,Mod).
load_biofile(Fmt,F):-
        atom(Fmt),
        concat_atom([Mod,pro],':',Fmt),
        !,
        T=..[pro,Mod],
        load_biofile(T,F).
load_biofile(Fmt,F):-
        atom(Fmt),
        concat_atom(L,'/',Fmt),
        L=[_,_|_],
        !,
        Term=..L,
        load_biofile(Term,F).
load_biofile(Fmt,InputFileIn):-
        expand_bioresource_search_path(InputFileIn,InputFile),
        get_module_by_format(Fmt,Mod),
        forall(schema_dependency(Mod,Dep),
               ensure_loaded(bio(Dep))),
	file_name_extension(Base, _Ext, InputFile),
	file_name_extension(Base, pro, PrologFile),
        debug(load,'input: ~w prolog: ~w',[InputFileIn,PrologFile]),
        assert(user:source_localpath(InputFileIn,PrologFile)),
	(   exists_file(PrologFile),
	    time_file(PrologFile, PrologTime),
	    time_file(InputFile, InputTime),
            \+ user:recompile_all_biofiles,
            \+ (user:recompile_biofiles_before(Before),
                PrologTime < Before),
	    PrologTime >= InputTime
	->  load_factfile(PrologFile,Mod)
	;   convert_and_load(Fmt,InputFile,PrologFile)).

% hook
user:recompile_all_biofiles:- fail.
user:recompile_biofiles_before(_):- fail.

% parse_and_load(+Fmt,+File)
%  uses prolog parser
% requires parse_stream/2 and parse_stream_to_facts/3 are defined
% in the io module *by the specific parser module*
parse_and_load(Fmt,InputFile):-
        cached_format(Fmt),
        !,
        get_module_by_format(Fmt,Mod),
        % TODO: collect duplicated code
        % TODO: allow all derived files to go in a separate location
        file_name_extension(Base, _Ext, InputFile),
        file_name_extension(Base, pro, PrologFile),
        debug(load,'Checking for cached : ~w',[PrologFile]),
	(   exists_file(PrologFile),
	    time_file(PrologFile, PrologTime),
	    time_file(InputFile, InputTime),
	    PrologTime >= InputTime
	->  load_factfile(PrologFile,Mod)
	;   access_file(PrologFile, write)
	->  open(InputFile,read,IO),
            debug(load,'No cached or out of date: parsing ~w',[InputFile]),
            %call_det(parse_stream_with_cache(Fmt,IO,PrologFile)),
            parse_stream_with_cache(Fmt,IO,PrologFile),
            close(IO)).
parse_and_load(Fmt,File):-
        open(File,read,IO),
        parse_stream(Fmt,IO),
        close(IO),
	!.
parse_and_load(Fmt,File):-
	throw(parse_and_load_failed(Fmt,File)).

        
% convert_and_load(+Fmt,+InputFile,+PrologFile)
%  uses external parser
% todo: allow custom directory
convert_and_load(_,_,PrologFile):-
        \+ access_file(PrologFile, write),
        access_file(PrologFile, read),
        get_time(T1),
        time_file(PrologFile,T2),
        TD is T1-T2,
        debug(load,'cannot write to ~w; current file age: ~w',[PrologFile,TD]),
        TD<360,
        debug(load,'  -- within threshold. using older file:',[PrologFile]),
        !.
convert_and_load(Fmt,InputFile,PrologFile):-
        \+ access_file(PrologFile, write),
        print_message(error,cannot_write_to(PrologFile)),
        tmp_file(cvt,Prefix),
        atom_concat(Prefix,'.pro',TmpPlFile),
        debug(load,'cannot write to ~w; using: ~w',[PrologFile,TmpPlFile]),
        !,
        setup_call_cleanup(true,
                           convert_and_load(Fmt,InputFile,TmpPlFile),
                           delete_file(TmpPlFile)).
convert_and_load(Fmt,InputFile,PrologFile):-
        debug(load,'converting: ~w fmt: ~w',[InputFile,Fmt]),
        get_module_by_format(Fmt,Mod),
        (   file_to_prolog_cmd(Fmt,InputFile,PrologFile,CmdTokens)
        ->  true
        ;   throw(cannot_convert_fmt(Fmt))),
        flatten(CmdTokens,CmdTokensFlat),
        concat_atom(CmdTokensFlat,' ',Cmd),
        path_lock(InputFile,Lock),
        debug(load,' testing for lock  ~w',[Lock]),
        remove_stale_lock(Lock),
        (   exists_file(Lock)
        ->  throw(io(lock('cannot access locked file. Try again later',InputFile)))
        ;   true),
        touch_file(Lock),
        debug(load,'Executing ~w',[Cmd]),
        (   shell(Cmd,0)
        ->  true
        ;   throw(problem_executing(Cmd))),
        load_factfile(PrologFile,Mod),
        delete_file(Lock).

% todo: abstract these
load_special(XmlFormat,File):-
        format_module_xmlmap(XmlFormat,Schema,XmlMap),
        !,
        ensure_loaded(bio(XmlMap)),
        ensure_loaded(bio(Schema)),
        transform_and_load_xml(XmlFormat,Schema,File).
load_special(rdfcache,File):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        rdf_load_db(File).
load_special(rdfs,File):-
        !,
        load_special(rdf,File).
load_special(rdfurl,File):-
        !,
        load_special(owl,url(File)).
/*
load_special(rdfowl,File):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        ensure_loaded(library('semweb/rdf_http_plugin')),
        ensure_loaded(bio(ontol_bridge_from_owl)),
        debug(load,'rdf_load(~w)',[File]),
        rdf_load(File,[namespaces(NSList)]),
        % TEMPORARY! TODO!
        forall(rdf_has(Ont,owl:imports,URI),
               debug(ontol_imports,'LOADED: ~w. States: ~w imports ~w',[File,Ont,URI])),
	register_namespaces(NSList).
*/
load_special(rdf,File):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        ensure_loaded(library('semweb/rdf_http_plugin')),
        debug(load,'rdf_load(~w)',[File]),
        rdf_load(File,[namespaces(NSList)]),
	register_namespaces(NSList).
load_special(ttl,File):-
        !,
        load_special(turtle,File).
load_special(turtle,File):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        ensure_loaded(library('semweb/rdf_turtle')),
        ensure_loaded(bio(ontol_bridge_from_owl)),
        %rdf_process_turtle(File,assert_triples,[]).
        rdf_load(File).
load_special('owl-with-imports',File):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        ensure_loaded(library('semweb/rdf_http_plugin')),
        ensure_loaded(bio(ontol_bridge_from_owl)),
        rdf_load(File,[namespaces(NSList)]),
        findall(URL,rdf_has(_,owl:imports,URL),URLs),
        !,
        forall(member(URL,URLs),
               (   writeln(loading(URL)),
                   (   rdf_load(URL,[namespaces(NSList)])
                   ->  writeln(loaded)
                   ;   writeln(failed)))), % fails but succeeds???
	register_namespaces(NSList).
				%forall((member(NS=Full,NSList),NS\=[]),
				%               catch(rdf_register_ns(NS,Full),_E,true)).
load_special(owlpl,File):-
        !,
        ensure_loaded(library('thea2/owl2_io')),
        ensure_loaded(library('thea2/owl2_util')),
        load_axioms(File,owlpl).
load_special(thea2_owl,File):-
        load_special(owl,File).
load_special(owl,File):-
        !,
        debug(load,'Using rdf_direct mapping from Thea to parse ~w',[File]),
        catch(ensure_loaded(library('thea2/owl2_rdf')),
              E,
              print_message(error,error('Please install Thea:',E))),
        load_special(rdf,File).
load_special(obo,File):-
        % if this goal fails, defaults to using file_to_prolog_cmd/2
        %
        % note that you can set vars on the command line; e.g.
        %  blip-findall -set use_obo_java=true -r caro "owl2_model:subClassOf(A,B)"
        nb_current(use_obo_java,_),
        !,
        load_special(oboj,File).
load_special(oboj,File):-
        % obo via java (still in testing, but this will be the default
        % method in future, bypassing the current perl conversion)
        !,
        ensure_loaded(bio(obo_new_namespaces)), % USE THIS NOW
        ensure_loaded(bio(ontol_bridge_from_owl2)),
        ensure_loaded(library(thea/owl2_io)),
        ensure_loaded(library(thea/owl2_util)),
        load_axioms(File,obo).
load_special(ncboowl2obo,File):-
        !,
        ensure_loaded(bio(obo_namespaces)),
        ensure_loaded(bio(ontol_bridge_from_owl2)),
        load_special(owl,File).
load_special(owl2obo,File):-
        !,
        ensure_loaded(bio(obo_new_namespaces)),
        ensure_loaded(bio(ontol_bridge_from_owl2)),
        load_special(owl,File).
load_special(owl_i,File):-
        !,
        ensure_loaded(library('thea2/owl2_from_rdf')),
        owl_parse_rdf(File,[imports(true)]).
load_special(obo_rules,File):-
        !,
        load_biofile(obo,File),
        ensure_loaded(bio(ontol_bridge_from_rules)),
        ontol_bridge_from_rules:assertall.
load_special(biopax_level2,File):-
        !,
        load_bioresource(biopax2),
        load_special(owl,File),
        ensure_loaded(bio(sb_bridge_from_biopax_level2)).

transform_and_load_xml(Fmt,Mod,File):-
	statistics(cputime, CpuOld),
        ensure_loaded(library(sgml)),
        load_structure(File,XML,[dialect(xmlns),space(remove)]),
        statistics(cputime, CpuLoaded),
        ParseTime is CpuLoaded - CpuOld,
        debug(load,'Parsed: ~w time:~w',[File,ParseTime]),
        xml_to_preds(Fmt,XML,PL), % hook predicate
        findall(Fact,(member(P,PL),
                      (   P=_:_
                      ->  Fact=P
                      ;   Fact=Mod:P)),
                Facts),
	sort(Facts,FactsU),
        insert_facts(FactsU,[transactional(true)]),
        statistics(cputime, CpuAsserted),
        AssertTime is CpuAsserted - CpuLoaded,
        debug(load,'Loaded: ~w time: ~w',[File,AssertTime]).

register_namespaces(_):- !.
register_namespaces(NSList):-
        debug(load,'caching namespaces ~w',[NSList]),
        forall((member(NS=Full,NSList),NS\=[]),
               catch(rdf_register_ns(NS,Full),_E,true)).

%**********************************************************
%  UNLOADING
%***********************************************************


%% unload_biofile(+Fmt,+File)
%   removes all facts defined in File from the database
%
%  only works if a db module is defined as having dynamic
%predicates. This must be done at startup time (ie in your =|~/.plrc|=
%or your =|bioconf.pro|= file):
%
%  for example, if you want all your ontologies to be dynamic, then add
%this to your startup:
%
%  ==
%  user:dynamic_db(ontol_db).
%  ==
%
%  note that this will make the ontol_db module slower, as the data will
%not be compiled
unload_biofile(Fmt,File):-
        format_module(Fmt,Mod),
	file_name_extension(Base, _Ext, File),
	file_name_extension(Base, pro, PrologFile),
        unload_mod_biofile(Mod,PrologFile).
unload_mod_biofile(Mod,File):-
        absolute_file_name(File,AbsFile),
        forall(dbmeta:datapred(Mod,Pred),
               (
                 dbmeta:pred_to_unground_term(Pred,Term),
                 forall((Mod:Term,
                         source_file(Mod:Term,AbsFile)), % builtin predicate
                        retract(Mod:Term)))).



%% unload_bioresources(+Resources)
%  
unload_bioresources(RL):-
        forall(member(R,RL),unload_bioresource(R)).
%% unload_bioresource(+Resource)
%   only works with dynamic data - see unload_biofile/2
%
%  see also load_bioresource/1 for details about resources
%
%  Example:
%  
%  ==
%  % -- purpose: show loading and unloading of a dynamic db --
%
%  % make sure ontol db is modifiable BEFORE loading module
%  user:dynamic_db(ontol_db).
%
%  % load modules
%  :- use_module(bio(io)).
%  :- use_module(bio(ontol_db)).
%
%  % summarize all ontologies currently loaded into the db
%  summarize_ontol:-
%    setof(Ont,ID^belongs(ID,Ont),Onts),
%    length(Onts,CountOnts),
%    format('Summary of ~w ontologies:~n',[CountOnts]),
%    forall(member(Ont,Onts),show_ontol_size(Ont)).
%
%  % fetch all IDs belonging to an ontology and display count
%  show_ontol_size(Ont):-
%    setof(ID,belongs(ID,Ont),IDs),
%    length(IDs,CountIDs),
%    format('ontol ~w has ~w classes~n',[Ont,CountIDs]).
%
%  demo:-
%    load_bioresource(cell),
%    load_bioresource(go),
%    writeln('cell+go:'),
%    summarize_ontol,
%    unload_bioresource(cell),
%    writeln('go:'),
%    summarize_ontol,
%    unload_bioresource(go).
%  ==
unload_bioresource(R):-
        user:bioresource(R,P),
        !,
        %expand_file_search_path(P,Px),
        %unload_biofile(user,Px).
        throw(todo(P,'bioresource must be associated with a module!')).
unload_bioresource(R):-
        user:bioresource(R,P,pro,Mod),
        !,
        expand_file_search_path(P,Px),
        unload_mod_biofile(Mod,Px).
unload_bioresource(R):-
        user:bioresource(R,P,Fmt),
        !,
        expand_file_search_path(P,Px),
        unload_biofile(Fmt,Px).
unload_bioresource(R):-
        throw(no_such_resource_as(R)).

%**********************************************************
%  WRITING
%***********************************************************

% each io module can override this
redirect_stdout(_):- fail.

%% write_biofile(+File) is det.
%   see load_biofile/2
%  
%% write_biofile(?Fmt,+File) is det. 
%   exports data from the in-memory db to a particular file and file
%format. If the format is not specified, will try and guess from file
%suffix
%
%  note: if a format is loadable, it is not necessarily writeable
%(except as prolog facts)
%  
%  [THIS PREDICATE NEEDS A BIT OF WORK]
%
%  make it more customizable: eg export predicates from a specific
%module to either prolog facts or bio format; filtering
%
%  examine the <io.pro> source of this module to see
%supported formats
%
%  supported formats (not an exhaustive list):
%  
%  
%    * pro
%        native prolog db / fact file - all data can be written as prolog
%    * tbl
%        tab delimited fact files - first column is predicate/relation
%        name, subsequence columns are argument values
%    * txt
%        tab delimited fact files - filename is used as predicate/relation
%    * summary
%        summary of db contents (written in prolog syntax)
%    * chadoxml
%        (for sb_db and phylo_db only)
%    * owl
%    * phenoxml
%        phenotype ontology association xml
%    * sb_dot
%        GraphViz dot format (sb_db only)
%  
write_biofile(F):-
        write_biofile(_,F).
write_biofile(Fmt,F):-
        write_biofile(Fmt,F,true).
write_biofile(Fmt,F,_):-
        var(Fmt),
        throw(must_specify_format_for(F)).
write_biofile(Fmt,F,Filter):-
        format_writer(Fmt,W),
        !,
        ensure_loaded(bio(W)),
        (   redirect_stdout(Fmt)   % some modules just use write/1
        ->  (   nonvar(F)
            ->  tell(F),
                current_output(Stream),
                set_stream(Stream,encoding(utf8)),
                write_all(Fmt,F,Filter),
                told
            ;   current_output(Stream),
                set_stream(Stream,encoding(utf8)),
                write_all(Fmt,null(_),Filter))
        ;   write_all(Fmt,F,Filter)).
write_biofile(Mod:pro,F,_Filter):- % TODO - append
        !,
        load_files(bio(Mod), [if(not_loaded),imports([])]),
        ensure_loaded(bio(dbmeta)),
        (   var(F)
        ->  true                % stdout; F1 remain var
        ;   F=append(F1)
        ->  true                % do nothing; unifdy F1; already appending
        ;   tell(F),            % empty file then append
            F1=F),
        write_db_facts(pro,F,Mod),
        forall(schema_dependency(Mod,Dep),
               write_biofile(Dep:pro,append(F1))),
        told.
write_biofile(Fmt,F,Filter):-
        concat_atom([Mod,pro],':',Fmt),
        !, 
        write_biofile(Mod:pro,F,Filter).
write_biofile(pro,F,Filter):-
        !,
        ensure_loaded(bio(dbmeta)),
        write_db_facts(pro,F,Filter).
write_biofile(rdfcache,F,_):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        rdf_save_db(F).
write_biofile(rdf,F,_):-
        !,
        ensure_loaded(library('semweb/rdf_db')),
        rdf_save(F).
write_biofile(tbl,F,_):-
        !,
        ensure_loaded(bio(dbmeta)),
        write_db_facts(tbl,F).
write_biofile(summary,F,_):-
        !,
        ensure_loaded(bio(dbmeta)),
        write_db_summary(F).
write_biofile(Fmt,F,_):-
        throw(unknown_output_format(Fmt,F)).


%**********************************************************
%  DEFAULT CONFIGURATION
%
%  the predicates below are typically multifile and
%  can be expanded in a users program
%***********************************************************

schema_dependency(pheno_db,metadata_db).
schema_dependency(ontol_db,metadata_db).
schema_dependency(curation_db,ontol_db).
schema_dependency(curation_db,metadata_db).
schema_dependency(curation_db,seqfeature_db).
schema_dependency(genome_db,metadata_db).
schema_dependency(phylo_db,metadata_db).
schema_dependency(pathway_db,metadata_db).
schema_dependency(pkb_db,genome_db).
schema_dependency(kegg_db,metadata_db).

%% file_to_prolog_cmd(?Format,?Cmd)
%  
%  maps file formats (obo, sbml, etc) to an executable that will parse the file to prolog facts
%
%  this predicate is not exported, but you can add your own mappings
%like this:
%
%  ==
%  io:file_to_prolog_cmd(myfmt,'myfmt2prolog.sh').
%  ==
file_to_prolog_cmd(go_assoc,'go2prolog -p go_assoc').
file_to_prolog_cmd(assoc,'go2prolog -p go_assoc'). % synonym
file_to_prolog_cmd(go_tagval,'go2prolog -p generic_tagval').
file_to_prolog_cmd(generic_tagval,'go2prolog -p generic_tagval').
file_to_prolog_cmd(dag,go2prolog).
file_to_prolog_cmd(obo,go2prolog). % 
file_to_prolog_cmd('annotation-obo',go2prolog). % 
file_to_prolog_cmd(go,'go2prolog -p go_ont').
file_to_prolog_cmd(obo_xml,go2prolog).
file_to_prolog_cmd(go_xref,'go2prolog -p go_xref').
file_to_prolog_cmd(pthr,'pthr2phylo_db.pl').
%file_to_prolog_cmd(gff,gff2p).
file_to_prolog_cmd(fasta,fasta2p).
file_to_prolog_cmd(blast,'search2p -f blast').
file_to_prolog_cmd(tagval,'tagval2p -reify -m user').
file_to_prolog_cmd(tagval(NS),X):- atom_concat('tagval2p -m ',NS,X).
file_to_prolog_cmd(tbl,'tbl2p').
file_to_prolog_cmd(ptbl,'tbl2p').
file_to_prolog_cmd(rpt,'tbl2p -use_filename').
file_to_prolog_cmd(tsv,'tbl2p -use_filename').
file_to_prolog_cmd(txt,'tbl2p -use_filename').
file_to_prolog_cmd(tab,'tbl2p -use_filename').
file_to_prolog_cmd(csv,'tbl2p -csv -p filedata').
file_to_prolog_cmd(tbl(P),X):- atom_concat('tbl2p -p ',P,X).
file_to_prolog_cmd(tbl(P,Cols),X):- concat_atom(Cols,',',A),concat_atom(['tbl2p -s ',A,' -p ',P],X).
file_to_prolog_cmd(idlist,'tbl2p -p id').
file_to_prolog_cmd(inparanoid_tbl,'tbl2p -p inpara').
file_to_prolog_cmd(homoltbl,'tbl2p -p homoltbl').
file_to_prolog_cmd(zfintab,'tbl2p -p phenorow').
file_to_prolog_cmd(gene_info,'tbl2p -cols 14 -p gene_info').
file_to_prolog_cmd(gene_rif,'tbl2p -p gene_rif --prefix 1=NCBITaxon: --prefix 2=NCBI_Gene: --prefix 3=PMID:').
file_to_prolog_cmd(gi_taxid,'tbl2p -p gi_taxid').
file_to_prolog_cmd(ontol_align,'tbl2p -p ontol_align').
file_to_prolog_cmd(ncbitaxnode,Cmd):-
        concat_atom([tbl2p,'-prefix 1,2=NCBITaxon: -cols 13','-p',taxnode,'-d','''\\s*\\|\\s*'''],' ',Cmd).
file_to_prolog_cmd(ncbitaxname,Cmd):-
        concat_atom([tbl2p,'-prefix 1=NCBITaxon: -cols 5','-p',taxname,'-d','''\\s*\\|\\s*'''],' ',Cmd).

file_to_prolog_cmd(_:Fmt,I,P,C):-
        !,
        file_to_prolog_cmd(Fmt,I,P,C).
file_to_prolog_cmd(gzip(Fmt),InputFile,PrologFile,CmdTokens):-
        file_to_prolog_cmd(Fmt,InnerCmd),
        !,
	file_name_extension(Base, pro, PrologFile),
	file_name_extension(Base, tmp, TmpFile),
        CmdTokens = ['gzip -dc',InputFile,'|',InnerCmd,'-','>',TmpFile,'&&',mv,TmpFile,PrologFile].
file_to_prolog_cmd(Fmt,InputFile,PrologFile,CmdTokens):-
        file_to_prolog_cmd(Fmt,Prog),
	file_name_extension(Base, pro, PrologFile),
	file_name_extension(Base, tmp, TmpFile),
        CmdTokens = [Prog,InputFile,'>',TmpFile,'&&',mv,TmpFile,PrologFile].


get_module_by_format(Mod:_,Mod):-
        !.
get_module_by_format(gzip(Fmt),Mod):-
        !,
        get_module_by_format(Fmt,Mod).
get_module_by_format(Fmt,Mod):-
        format_module(Fmt,Mod),
        load_biomodule(Mod),
        !.
get_module_by_format(_,user).

load_biomodule(Mod):-
        %writeln(loading(Mod)),
        ensure_loaded(bio(Mod)).
%        Mod:import(bioprolog_util:load_factfile/1).


:- dynamic filter_include_list/2.

:- mode in_include_list(+,+) is semidet.
in_include_list(_,ListType):-
        \+ filter_include_list(_,ListType), % no include list for this type - default is always to include
        !.
in_include_list(ID,ListType):-
        filter_include_list(ID,ListType).

%% add_to_include_list(+ID,+ListType)
%   All IDs of this type will be include in export
%
%  this is only respected by some export modules at this time
%  
:- mode add_to_include_list(+,+) is det.
add_to_include_list(ID,ListType):-
        assert(filter_include_list(ID,ListType)).
%% format_module(?Format,?Module)
%   maps file formats (obo, sbml, etc) to bio modules (ontol_db, sb_db)
%
%  You can add your own mappings
%like this:
%
%  ==
%  io:format_module(myfmt,mymod).
%  ==
%
%  you can query your blip instance to see which formats map to which
%modules like this:
%
%  ==
%  io:format_module(Fmt,Mod).
%  ==
format_module(go_assoc,curation_db).
format_module(go_tagval,ontol_db).
format_module(generic_tagval,ontol_db).
format_module(obo,ontol_db).
format_module('annotation-obo',curation_db).
format_module(go,ontol_db).
format_module(obo_xml,ontol_db).
%format_module(thea,owl_parser).
format_module(go_xref,ontol_db).
format_module(dag,ontol_db).
format_module(obo_native,ontol_db).
format_module(phenosyn,pheno_db).
format_module(ocelot,ontol_db).
format_module(sbml,sb_db).
format_module(pathway_gaf,pathway_db).
format_module(paint,phylo_db).
format_module(nhx,phylo_db).
format_module(nh,phylo_db).
format_module(fasta,fasta_db).
format_module(blast,seqanalysis_db).
format_module(ncbitaxname,taxon_db).
format_module(ncbitaxnode,taxon_db).
format_module(gene_rif,gene_db).
format_module(gene_info,gene_db).

format_synonym(phenoxml,'pheno-xml').

format_module_xmlmap(homologene,homol_db,homol_xmlmap_homologene).
format_module_xmlmap(sbml,sb_db,sb_xmlmap_sbml).
format_module_xmlmap(keggxml,kegg_db,kegg_xml).
format_module_xmlmap(kegg,kegg_db,kegg_xml).
%format_module_xmlmap(entrezgene,gene_db,gene_xmlmap_entrezgene). % todo - see ontol map
format_module_xmlmap(entrezgene,ontol_db,ontol_xmlmap_entrezgene). % todo - see ontol map
format_module_xmlmap(chaos,seqfeature_db,seqfeature_xmlmap_chaos).
format_module_xmlmap(phyloxml,phylo_db,phylo_xmlmap_phyloxml).
format_module_xmlmap(game,seqfeature_db,seqfeature_xmlmap_game).
format_module_xmlmap(phenoxml,pheno_db,pheno_xmlmap_phenoxml).
format_module_xmlmap(phenotexml,pkb_db,pkb_xmlmap_phenote).
format_module_xmlmap(omimxml,omim_db,omim_xmlmap_omimxml).
format_module_xmlmap(pdb,structure_db,structure_xmlmap_pdb).

format_parser(obolog,parser_obolog).
format_parser(obolog_simple,parser_obolog).
format_parser(obolog_hr,parser_obolog).
format_parser(ocelot,parser_ocelot).
format_parser(obo_native,parser_obo).
format_parser(phenosyn,parser_phenosyn).
%format_parser(obo,parser_obo).
format_parser(paint,parser_paint).
format_parser(nhx,parser_nhx).
format_parser(nh,parser_nhx).
format_parser(gff3,parser_gff3).
format_parser(gff,parser_gff3).

format_parser(sxpr,sxpr_parser).

cached_format(obo_native).
cached_format(obo).

%% format_writer(?Fmt,?WriterModule)
%   this predicate is used to determine which module is
%  to be used to write a particular format. the deterministic predicate
%  io:write_all/2 is used to export data
%  
format_writer(obo,ontol_writer_obo).
format_writer(ptagval,ontol_writer_ptagval).
format_writer(curation_obo,curation_writer_obo).
format_writer(annotation_obo,curation_writer_obo).
format_writer(asp,ontol_writer_asp).
format_writer(powerloom,ontol_writer_powerloom).
%format_writer(obolog,ontol_writer_obolog). DEPRECATED
format_writer(obolog_latex,obolog_writer_latex).
format_writer(prover9,obolog_writer_prover9).
format_writer(prover9_minimal,obolog_writer_prover9).
format_writer(prover9(_),obolog_writer_prover9).
format_writer(kif,obolog_writer_kif).
format_writer(cl,obolog_writer_kif).
format_writer(clif,obolog_writer_kif).
format_writer(obolog,obolog_writer_kif).
format_writer(obolog_prolog,obolog_writer_prolog_unreified).
format_writer(owl,ontol_writer_owl).
format_writer(owl2_prolog,ontol_writer_owl2).
format_writer(owl2,ontol_writer_owl2).
format_writer(owl2(_),ontol_writer_owl2).
format_writer(owl2(_,_),ontol_writer_owl2).
format_writer(thea,ontol_writer_thea_syntax).
format_writer(thea_owlrdf,ontol_writer_thea_owlrdf).
format_writer(thea_dlp,ontol_writer_thea_syntax).
format_writer(ontol_dot,ontol_writer_dot).
format_writer(go_assoc,curation_writer_go_assoc).
format_writer(gene_exhibit,curation_exhibit_bridge).
format_writer(annotation_exhibit,curation_exhibit_bridge).
format_writer(instance_table,ontol_writer_instance_table).
format_writer(chaos,seqfeature_writer_chaos).
format_writer(seqfeature_dot,seqfeature_writer_dot).
format_writer(seqfeature_dlv,seqfeature_writer_dlv).
format_writer(seqfeature_xsb,seqfeature_writer_xsb).
format_writer(phenoxml,pheno_writer_general).
format_writer(obd,ontol_writer_obd_generic).
format_writer(intermine,ontol_writer_intermine).
format_writer(phenotbl,pheno_writer_general).
format_writer(sb_chadoxml,sb_writer_chadoxml).
format_writer(sb_dot,sb_writer_dot).
format_writer(sb_dot2,sb_writer_dot).
format_writer(pathway_gaf,pathway_writer_gaf).
format_writer(phylo_chadoxml,phylo_writer_chadoxml).

%% consult_bioconf
%  
%
%  reads in a user's bioconf.pro file. this is typically stored in
%$HOME/.blip/bioconf.pro, but this may be overridden with $BLIP_BIOCONF
%
%  if no bioconf.pro is found, an informational message (not visible in
%verbose(silent) mode) is written, and the blip default bioconf.pro
%file (found wherever blip is installed) is used instead
%
%  this predicate is called automatically whenever a blip script is invoked
consult_bioconf:-
        (   catch(user:bioresource(_,_,_),
                  _,
                  fail)
        ->  print_message(informational,bioconf_already_loaded)
        ;   consult_bioconf_force).

consult_bioconf_force:-
        user_bioconf_path(Path),
        (   exists_file(Path)
        ->  consult_bioconf(Path)
        ;   (   system_bioconf_path(DefaultPath),
                exists_file(DefaultPath)
            ->  debug(load,'Using system bioconf',[]),
                consult_bioconf(DefaultPath)
            ;   throw(error(cannot_find_bioconf)))).

%% consult_bioconf(+FilePath)
%  
%
%  consult a particular bioconf.pro file. throw an error if not found
consult_bioconf(Path):-
        debug(load,'Consulting bioconf file: ~w',Path),
        catch([Path],
              E,
              throw(error(error_in_bioconf(Path,E)))).

system_bioconf_path(Path):-
        expand_file_search_path(bio('system_bioconf.pl'),Path),
        exists_file(Path),
        !.
system_bioconf_path(Path):-
        expand_file_search_path(bio('system_bioconf.pro'),Path),
        exists_file(Path),
        !.

user_bioconf_path(Path):-
        (   getenv('BLIP_BIOCONF',Path),
            exists_file(Path)
        ->  true
        ;   (   expand_file_search_path(home('.blip/bioconf.pro'),Path)
            ->  true
            ;   Path='~/.blip/bioconf.pro')).

%**********************************************************
%  INFORMATIONAL MESSAGES
%***********************************************************

prolog:message(message(M)) --> [M].
prolog:message(bioconf_already_loaded) -->
        ['It appears bioresources have been defined --',
         '  will not consult bioconf.pro without force'].
/** <module> input and output for bio file formats

  ---+ Synopsis
  
  ==
  % -- load an ontology into the db and query it --
  :- use_module(bio(io)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(sb_db)).

  demo:-
    load_bioresource(chebi), % this ontology must be defined in bioconf.pro
    load_biofile(sbml,'BIOMD0000000018.xml'),
    class(ClassID,'amino acids'),
    forall((   subclassRT(AnnotClassID,ClassID),
               entity_class(EntID,AnnotClassID),
               class(AnnotClassID,AnnotClassName)),
           format('Entity: ~w AnnotClass: ~w ~w~n',
                  [EntID,AnnotClassID,AnnotClassName])).
  ==

  ==
  % examples of converting from one file format to another:
  % Chaos-XML to Chado/Sequence Ontology OWL format
  :- use_module(bio(io)).

  demo:-
    load_bioresource(so),
    load_biofile(chaos,'Rab1.chaos-xml'),
    write_biofile(pro,'Rab1.pro'),
    ensure_loaded(bio(seqfeature_bridge_to_inst)),
    write_biofile(owl,'Rab1.owl'),
    writeln('Showing first 100 lines of exported OWL file:'),
    shell('head -100 Rab1.owl').
  ==

  ---+ Description

  Input-Output for various file formats

  Data from a supported format is loaded into the in-memory database
using load_bioresource/1. The path is expanded and the resource is
loaded into a particular module space, depending on the file format

  ---++ bioconf

  you should set up a bioconf file: see the example file in the 'conf'
directory in he distribution. This file defines some common bio
resources you may wish to use - ontology files, databases, xml exports
of particular data sets, etc. Some of these resources may be simple
prolog facts files to be loaded into the 'user' module. Others may be
file formats that blip recognizes and loads into the appropriate
module space. For example, 'obo' files are loaded into the
<ontol.html> ontol module space

  see bioresource/2,bioresource/3 and bioresource/4

  to make the most of a bioconf file and to avoid hardcoding directory
paths repeatedly, you should understand the standard SWI Prolog
predicate expand_file_search_path/2

  ---++ Supported formats

  These are all read-only unless otherwise specified

  
    * pro
        native prolog db / fact file
    * tbl
        native tab-delimited fact file
    * obo
        obo-text format - into ontol_db module. uses parser_obo module
    * dag
        dag-edit format - into ontol_db module
    * rdfs
        RDFS ontology format - into ontol_db module
    * owl
        OWL ontology format - into ontol_db module
    * go_assoc
        Gene Ontology association file
        loaded as inst/2 and inst_sv/2 facts in ontol module
    * sbml
        SBML systems biology level 1 format (limited support)
        - into sb_db module
    * nhx, nh
        new hampshire (extended) - into phylo_db module. uses parser_nhx module
    * chaos
        Chaos-XML format
        - into seqfeature_db module
    * gff
        GFF [TESTING]
        - into seqfeature_db module
    * inparanoid_tbl
        Inparanoid ortholog format [TESTING]
        - into user module for now [TODO: homology_db module]
    * ncbitaxnames
        NCBI Taxon names (names.dmp) format - into taxon_db
    * ncbitaxnodes
        NCBI Taxon nodes (nodes.dmp) format - into taxon_db
    * gene_rif
        NCBI Gene_rif - into gene_db
    * gene_info
        NCBI gene_info - into gene_db
    * gzip(Fmt)
        a compressed file in format Fmt, defined above
  
  

  ---++ Caching and parsing

  When parsing some file formats, a cache file will be created. This
means that the second time the same file is parsed, loading into
memory will be much faster.

  For now, the cached file will be placed in the same directory as the
original file. It will get the .pro suffix (for the prolog fact file),
and the .qlf suffix (SWI-Prolog quickload format)

  Currently, no XML formats are cached (parsing of these is already
fast). Any parser that calls an external program (for example the
ncbitaxnames format) will result in a cached file. The obo ontology
parser will also create a cached file.

  In future the caching strategy will be made more configurable; for
now you will have to alter this file or contact the author for other
caching strategies.
  
  ---++ Verbose mode

  This module contain debug flags for printing information whilst
processing. To activate:
  
  ==
  set_prolog_flag(verbose,normal).
  debug(load).
  ==

  If you are using the blip command line utility, you can do this from
the command line:

  ==
  shell> blip -r go -debug load
  % bio(ontol_db) compiled into ontol_db 0.01 sec, 22,296 bytes
  % /Users/cjm/cvs/go/ontology/gene_ontology.qlf loaded into 0 0.79 sec, 12,626,192 bytes
  % Loaded /Users/cjm/cvs/go/ontology/gene_ontology.pro into [ontol_db] time:0.79
  ?- 
  ==

  @author Chris Mungall
  @version  $Revision: 1.42 $
  @license LGPL

  */
