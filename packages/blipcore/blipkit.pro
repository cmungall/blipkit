/* -*- Mode: Prolog -*- */

:- module(blipkit,
          [
           main/0,
           blip_shell/0,
           blip/5,
           show_factrow/2
           ]).

:- use_module(bio(bioprolog_util)).
:- use_module(bio(io)).
:- use_module(bio(index_util)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(blipkit_shell_dcg)).
:- use_module(bio(tabling)).

:- module_transparent blip/5.
:- multifile
        user:meta/1,
        user:opt_insecure/1,
        trusted_command/1,
        opt_description/2,
        main/6,
        main/7,
        example/1,
        example/2,
        user:program_info/1.        



% GLOBAL VARIABLES:
% (normally these are to be avoided at all costs, but they are fine
%  in a wrapper module like this)
%
%  format - input file format

opt_description(trace,'If set, enters prolog trace mode').
opt_description(guitracer,'If set, tracing happens in GUI. Use in conjunction with -trace').
opt_description(profile,'Profile execution of this command').
opt_description(pregoal,'A prolog goal to be satisfied').
opt_description(goal,'A prolog goal to be satisfied').
opt_description(debug,'A debug category, eg "load" or "blip"').
opt_description(from,'Input format, see io module for full list').
opt_description(input,'Input file, format specified by filename suffix or -from').
opt_description(include,'Add to include list. Only IDs belonging to the include category will be reported. E.g -include ontology//cellular_component').
opt_description(resource,'Input resource, must be defined in bioconf.pro').
opt_description(verbose,'If set, prints informational messages').
opt_description(help,'General help, or help on a specific command if specified').
opt_description(conf,'Prolog conf files to be loaded').
opt_description(use,'Name of blip module to be used; eg a bridge module').
opt_description(materialize,'Name of predicate to materialize. Module and arity must be supplied. e.g ontol_db:subclassRT/2').
opt_description(table_pred,'Pred/Arity to be tabled (cached, memoized). Eg -table_pred subclassRT/2').
opt_description(label,'Shows labels, using entity_label/2').
opt_description(sqlbind,'Rewrites a predicate to a SQL call on a specified database. E.g. -sqlbind curation_db:curation_statementT/5-go or -sqlbind ontol_db:all-go').

user:opt_insecure(goal).
user:opt_insecure(pregoal).
user:opt_insecure(midgoal).
user:opt_insecure(assert).
user:opt_insecure(output).
user:opt_insecure(set_prolog_flag).

blipkit:example('blip -i go_v1.obo -i go_v2.obo -db ontol_db io-diff',
                'compare two Gene Ontology files').
                
main:-
        Opts =
        [bool(trace,Trace),
         bool(guitracer,Guitracer),         
         bool(profile,Profile),         
         bool(import_all,ImportAll),
         terms(sqlbind,SQLBinds),
         terms(index,Indexes),
         terms(multifile,MultifileDirectives),
         atom(index_file,IndexFile),
         atoms([pregoal,pre],PreGoals),
         atoms(endgoal,EndGoal),
         atoms([spy],SpyPoints),
         atoms(debug,Debugs),
         terms(goal,Goals,true),
         terms(fdl,FDLs,true),
         atoms(goalfile,GoalFiles,true),
         term([format,from,f],Format),
         atoms([i,input],InputFileL),
         atoms([ff,formatfile],FormatFileL),
         atoms([include],Includes),
         atoms([r,resource],ResourceL),
         atoms([mat,materialize],MaterializePreds),
         atoms(dedupe,DeDupeList),
         atom(table_file,TableFile),
         terms(table_pred,TablePreds),
         terms(assert,AssertTerms),
         atoms(set_prolog_flag,SetPrologFlags),
         atom(set_data_cache,DataCache),
         bool([v,verbose],Verbose),
         bool([h,help],Help),
         bool(statistics,Statistics),
         atoms([set],SetVars),
         atoms([c,conf],ConfL),
         atoms([consult],ConsultFileL),
         terms([u,use],UseModL)],
        getopt(Opts,
               FileL),
        forall(member(Flag,SetPrologFlags),
               set_prolog_flag(Flag,true)),
        (   Verbose=1 -> set_prolog_flag(verbose,normal) ; true),
        (   Debugs=[]
        ->  true
        ;   set_prolog_flag(verbose,normal),
            maplist(debug,Debugs)),
        print_message(banner,welcome),
        % TODO: handle this upstream
        %catch(consult_bioconf,
        %      E,
        %      (	  write(user_error,E),
        %          die('You must set up a bioconf.pro file!'))),
        (Guitracer=1->guitracer;true),
        forall(member(PreGoal,PreGoals),PreGoal),
        maplist(multifile,MultifileDirectives),
        forall(member(SetVar,SetVars),
               (   concat_atom([Var,Value],'=',SetVar),
                   nb_setval(Var,Value))),
        (   nonvar(DataCache)
	->  retractall(user:file_search_path(data_cache,_)),
	    assert(user:file_search_path(data_cache,DataCache))
	;   true),
        forall(member(Conf,ConfL),
               (   expand_file_search_path(Conf,ConfPath),
                   consult(ConfPath))),
        forall(member(Mod,UseModL),
               use_blip_module(Mod)),
        forall(member(Resource,ResourceL),
               load_bioresource(Resource)),
        forall(member(File,InputFileL),
               load_biofile(Format,File)),
        forall(member(FF,FormatFileL),
               (   concat_atom([LocalFormat,LocalFile],'^',FF),
		   load_biofile(LocalFormat,LocalFile))),
        forall(member(File,ConsultFileL),
               ensure_loaded(File)),
        forall(member(Term,AssertTerms),
               user:assert(Term)),
        forall(member(SpyPoint,SpyPoints),
               (   concat_atom([P,A],'/',SpyPoint),
                   spy(P/A))),
        forall(member(Include,Includes),
               (   concat_atom([IncludeType,IncludeID],'//',Include),
                   add_to_include_list(IncludeID,IncludeType))),
        % bind SQL after all mapping modules are loaded
        (   SQLBinds=[]
        ->  true
        ;   ensure_loaded(bio(rdb_util)),
            maplist(sqlbind,SQLBinds)),
        (   ImportAll=1
        ->  ensure_loaded(bio(ontol_db)),
            ontol_db:import_all_ontologies
        ;   true),
        (   nonvar(IndexFile)
        ->  materialize_indexes_to_file(Indexes,IndexFile)
	;   forall(member(Index,Indexes),
                   materialize_index(Index))),
        maplist(remove_duplicates,DeDupeList),

        % global variable alert!
        nb_setval(format,Format),
        
        % tabling-lite
        % DEPRECATED: use index_util
        maplist(materialize_view,MaterializePreds),

        % tabling
	(   var(TableFile)
        ->  maplist(table_pred,TablePreds)
	;   forall(member(TablePred,TablePreds),
		   persistent_table_pred(TablePred,TableFile))),
        
        forall(member(File,ConsultFileL),
              ensure_loaded(File)),
        forall(member(Goal,Goals),Goal),
        forall(member(Goal,FDLs),
               forall(Goal,true)),
        forall(member(GoalFile,GoalFiles),process_goalfile(GoalFile)),
        (Trace=1->trace;true),
        (   FileL=[Cmd|FileL2]
        ->  G=run_user_command(Cmd,FileL2,[help(Help)]),
            (   Profile=1
            ->  profile(G),
                prolog_shell
            ;   G)
        ;   (   Help=1          % 
            ->  usage(Opts)
            ;   prolog_shell)),
        (   Statistics=1
        ->  statistics
        ;   true),
        EndGoal.

:- module_transparent use_blip_module/1.
use_blip_module(library(Mod)) :-
	!,
	user:ensure_loaded(library(Mod)).
use_blip_module(Mod) :-
	!,
	user:ensure_loaded(bio(Mod)).


prolog_shell:-
        format('% Starting blip shell~n'),
        repeat,
        catch(prolog,
              E,
              (format('ERROR:~n~w~n',[E]),fail)),
        !,
        format('Come back soon!~n').

blip_shell:-
        current_input(IO),
        repeat,
        read_line_to_codes(IO,Codes),
        writeln(codes=Codes),
        (   Codes=end_of_file
        ->  !
        ;   atom_codes(A,Codes),
            rl_add_history(A),
            blip_shell_exec(A),
            fail).

blip_shell_exec(A):-
        trace,
        tokenize_blip_command(A,[Cmd|Args]),
        run_user_command(Cmd,Args,[]).

process_goalfile(F) :-
        open(F,read,IO,[]),
        repeat,
        read(IO,Goal),
        (   Goal=end_of_file
        ->  !
        ;   Goal,
            fail),
        close(IO).


%% blip(+C,+Desc,+Opts,+Files,+Action)
%  called from *within* a module body to register a blip command
blip(C,Desc,Opts,Files,Action):-
        context_module(Mod),
        assert(blipkit:main(C,Desc,Mod,Opts,Files,[],Action)).

:- blip('prolog',
        'query blip resources via prolog shell',
        [],
        FileL,
        (   trust_current_user,
            forall(member(File,FileL),
                   load_biofile(File)),
            prolog)).

:- blip('true',
        '',
        [],
        _,
	true).

:- blip('index-file',
        'indexes predicate(s) for all facts in a file, materializes the index',
        [atom(source_format,SrcFmt),
         terms(spec,IndexSpecs),
         bool(force,Force),
         atom(loc,IxFile),
         atom(suffix,Suffix,'_ix.pro')
         ],
        [SrcFile],
        (   (   var(IxFile)
            ->  atom_concat(SrcFile,Suffix,IxFile)
            ;   true),
            (   Force=1
            ->  Opts=[force(true)]
            ;   Opts=[]),
            create_index_file_from_source_file(SrcFile,SrcFmt,IxFile,IndexSpecs,Opts))).

create_index_file_from_source_file(SrcFile,SrcFmt,IxFile,IndexSpecs,Opts) :-
        member(force(Force),Opts),
        Force,
        !,
        (   exists_file(IxFile)
        ->  delete_file(IxFile)
        ;   true),
        debug(index,'forcing index of',[SrcFile]),
        load_biofile(SrcFmt,SrcFile),
        materialize_indexes_to_file(IndexSpecs,IxFile).
create_index_file_from_source_file(SrcFile,SrcFmt,IxFile,IndexSpecs,_) :-
        exists_file(IxFile),
        time_file(SrcFile,SrcTime),
        time_file(IxFile,IxTime),
        SrcTime>IxTime,
        !,
        debug(index,'~w more recent than ~w - reindexing',[SrcFile,IxFile]),
        load_biofile(SrcFmt,SrcFile),
        delete_file(IxFile),
        materialize_indexes_to_file(IndexSpecs,IxFile).
create_index_file_from_source_file(_,_,IxFile,_,_) :-
        exists_file(IxFile),
        !,
        debug(index,'current index ~w is good',[IxFile]).
create_index_file_from_source_file(SrcFile,SrcFmt,IxFile,IndexSpecs,_) :-
        \+ exists_file(IxFile),
        debug(index,' creating new index: ~w',[IxFile]),
        load_biofile(SrcFmt,SrcFile),
        materialize_indexes_to_file(IndexSpecs,IxFile).


blipkit:example('blip sed -rule "line(ID,FullName):-line(ID,First,Last),concat_atom([First,Last],-,FullName)"',
                'processes a 3 column file into a 2 column file').
opt_description(rule,'A prolog rule of the form: "line(OutArgs) :- line(InArgs....),Goals').
opt_description(rulefile,'As -rule, but load from a file').

:- blip('sed',
        'process input file according to rule or rulefile. analagous to unix sed',
        [term(rule,Rule),
	 atom(rulefile,RuleFile)],
        Args,
        (   (	nonvar(Rule)
	    ->	Files=Args
	    ;	(   nonvar(RuleFile)
		->  Files=Args
		;   Args=[RuleFile|Files]),
		open(RuleFile,read,IO),
		read_term(IO,Rule,[]),
		close(IO)),
	    !,
	    debug(sed,'rule: ~w',[Rule]),
	    ensure_loaded(bio(process_streams)),
	    forall(member(File,Files),
		   process_file_lines(Rule,File)))).

:- blip('labelify',
        'add tables',
        [bool(read_prolog,ReadProlog)],
        Files,
        (   Rule=( (Head :- Row,Row=..L,maplist(blipkit:labelify,L,L2),Head=..L2) ),
            !,
	    debug(sed,'rule: ~w',[Rule]),
	    ensure_loaded(bio(process_streams)),
            (   ReadProlog=1
            ->  IsPl=true
            ;   IsPl=false),
	    forall(member(File,Files),
		   process_file_lines(Rule,File,[pl(IsPl)])))).

labelify(X,X2) :- entity_label(X,L),!,concat_atom([X,L],'-',X2).
labelify(X,X).

:- blip('io-extract',
        'chains',
        [atom(to,Fmt,pro),
	 atom(query,QA,true),
	 atom(select,FactA)],
        _,
        (   atom_to_term(QA,Q,Bindings1),
	    atom_to_term(FactA,Fact,Bindings2),
	    unify_bindings(Bindings1,Bindings2),
	    forall(Q,
		   write_db_fact_chain(Fmt,Fact)))).

unify_bindings([],_).
unify_bindings([A=B|L1],L2) :-
	member(A=B,L2),
	!,
	unify_bindings(L1,L2).
unify_bindings([_|L1],L2) :-
	unify_bindings(L1,L2).


blipkit:trusted_command('read').
:- blip('read',
        'reads command via standard_input and executes',
        [],
        _,
        (   prompt(_,''),
            read_stream_to_codes(user_input,Codes),
            atom_codes(A1,Codes),
            sub_atom(A1,0,_,1,A), % remove newline
            debug(read,'cmd=~w;;',[A]),
            concat_atom(Args1,' ',A), % tokenize
            %exclude(=(''),Args1,Args2),
            %maplist(strip_quotes,Args2,Args),
            process_cmdargs(Args1,Args,''),
            debug(read,'args=~w',[Args]),
            set_prolog_flag(argv,['--'|Args]),
            main)).

quotechar('"').
quotechar('\'').

process_cmdargs([],[],_):- !.
process_cmdargs([A|T],T2,QP):- % start
        \+ quotechar(QP),
        quotechar(Q),
        atom_concat(Q,A1,A),
        !,
        process_cmdargs([A1|T],T2,Q).
process_cmdargs([A|T],[A1|T2],Q):-      % end
        quotechar(Q),
        atom_concat(A1,Q,A),
        !,
        process_cmdargs(T,T2,'').
process_cmdargs([A|T],[Next|T2],Q):-      % quoted gap
        quotechar(Q),
        !,
        process_cmdargs(T,[X|T2],Q),
        concat_atom([A,X],' ',Next).
process_cmdargs([''|T],T2,Q):-      % empty
        !,
        process_cmdargs(T,T2,Q).
process_cmdargs([A|T],[A|T2],Q):-      % unquoted gap
        !,
        process_cmdargs(T,T2,Q).


strip_quotes(A,A2):-
        atom_concat(A1,'"',A),
        !,
        strip_quotes(A1,A2).
strip_quotes(A,A2):-
        atom_concat(A1,'\'',A),
        !,
        strip_quotes(A1,A2).
strip_quotes(A,A2):-
        atom_concat('"',A1,A),
        !,
        strip_quotes(A1,A2).
strip_quotes(A,A2):-
        atom_concat('\'',A1,A),
        !,
        strip_quotes(A1,A2).
strip_quotes(A,A).



:- blip('run-tests',
        'runs test suite',
        [],
        _,
        (   run_tests)).

opt_description(force,'Overwrite existing file(s)').

:- blip('config',
        'Setup a user''s bioconf.pro file in ~/.blip/',
        [bool(force,Force)],
        _,
        (   trust_current_user,
            (   io:user_bioconf_path(UserPath)
            ->   (   exists_file(UserPath),
                     Force=0
                 ->  format(user_error,'File already exists:~w~n',[UserPath]),
                     die('will not overwrite without -force option')
                 ;   (io:system_bioconf_path(SysPath)
                     ->  open(SysPath,read,InStream,[]),
                         open(UserPath,write,OutStream,[]),
                         format('Copying from ~w to ~w~n',[SysPath,UserPath]),
                         copy_stream_data(InStream,OutStream),
                         close(OutStream),
                         close(InStream),
                         writeln(' ** Blip is now configured for this user **')
                     ;   die('could not find system_bioconf file')))
            ;   die('Cannot determine user bioconf path'))
        )).

% IO %

opt_description(to,'Output format, see io module for list of allowed formats').
opt_description(output,'Output file name or path').

blipkit:trusted_command('io-convert').
user:opt_insecure(output).
:- blip('io-convert',
        'converts from one format to another: see io module',
        [term([to,t],ToFormat),
         atom([o,output],OutFile)],
        FileL,
        (   
          maplist(load_biofile,FileL),
          write_biofile(ToFormat,OutFile))).


:- blip('io-diff',
        'compares two files at the prolog fact level',
        [atom(db,Mod,user),
         bool(label,IsLabel),
         term(match,MatchGoal,_)],
        FileL,
        (   
            (   var(Mod), \+FileL=[]
            ->  throw(option_error('Must specify a module with -db for these files: ~w',[FileL]))
            ;   true),
            Opts=[isLabel(IsLabel)],
            forall(member(File,FileL),
                   load_factfile(File,Mod)),
            db_facts(Mod,AllFacts),
            findall(Fact,(member(Fact,AllFacts),
                          Fact=MatchGoal),
                    Facts),
            io_diff(Mod,Facts,Opts))).

io_diff(Mod,Facts,Opts):-
        findall(File-Fact,(member(Fact,Facts),
                           clause(Mod:Fact,_,ClauseID),
                           clause_property(ClauseID,file(File))),
                FFSet),
        setof(File,Fact^member(File-Fact,FFSet),Files),
        debug(blip,'Files=~w',[Files]),
        (   Files=[File1,File2]
        ->  setof(Fact,member(File1-Fact,FFSet),Facts1),
            setof(Fact,member(File2-Fact,FFSet),Facts2),
            sort(Facts1,Facts1S),
            sort(Facts2,Facts2S),
            compare_two_lists(Facts1S,Facts2S,UL1,UL2),
            format('Comparing: ~w vs ~w~n',[File1,File2]),
            forall(member(U,UL1),
                   (   format('Unmatched [1]: '),
                       show_factrow(Opts,U))),
            forall(member(U,UL2),
                   (   format('Unmatched [2]: '),
                       show_factrow(Opts,U))),
            nl
        ;   throw(error(files('must be sourced from two files; you had: ',Files)))).


:- blip('source-viz',
        'visualize source dependencies',
        [term([to,t],ToFormat),
         atom([o,output],OutFile)],
        Sources,
        (   
            draw_source_dependencies(Sources,ToFormat,OutFile))).

draw_source_dependencies(Sources,_ToFormat,_OutFile) :- % TODO
        ensure_loaded(library(prolog_xref)),
        ensure_loaded(bio(dotwriter)),
        forall(member(Source,Sources),
               xref_source(bio(Source))),
        findall(node(Mod,[label=Mod]),
                (   %member(Source,Sources),
                    xref_module(Source,Mod)),
                SNodes),
        findall(Node,
                (   xref_defined(S,Goal,_),
                    xref_exported(S,Goal),
                    functor(Goal,P,_),
                    %findall(Param,system:graphviz_pathway_param(node(ID),Param,Edges),NodeParams),
                    Node=node(P,[label=P])),
                Nodes),
        findall(edge(InP,OutP,[]),
                (   xref_called(S,Out,In),
                    xref_exported(S,In),
                    xref_exported(_,Out),
                    functor(Out,OutP,_),
                    functor(In,InP,_)),
                Edges),
        flatten([SNodes,Nodes,Edges],Terms),
        G=graph(g,[],Terms),
        findall(N-Mod,
                (   xref_defined(S,Goal,_),
                    xref_exported(S,Goal),
                    functor(Goal,N,_),
                    xref_module(Source,Mod)),
                NestPairs),
        writeln(NestPairs),
        graph_nest_pairs(G,GX,NestPairs),
        %graph_to_dot_atom(GX,A),
        %writeln(A).
        graph_display(GX,open).


% should this go in metadata_db module?
:- blip('map-synonym-to-id',
        'converts synonyms to IDs using metadata_db:entity_synonym',
        [number(num_columns,NumColumns,1),
         bool(show_unmapped,ShowUnmapped)],
        FileL,
        (   
            maplist(load_biofile,FileL),
            functor(SynPred,id,NumColumns),
            SynPred=..[_,Syn|Rest],
            forall((SynPred,\+ entity_synonym(ID,Syn)),
                   (   ShowUnmapped=1
                   ->  writecols([Syn|Rest]),
                       nl
                   ;   format(user_error,'Could not map:~w~n',[Syn]))),
            forall((SynPred,entity_synonym(ID,Syn)),
                   (   writecols([ID|Rest]),
                       nl)))).


:- blip('map-ids',
        'maps forward identifiers',
        [atom([to,t],ToFormat),
         atoms(key,Keys),
         atom([o,output],OutFile)],
        FileL,
        (   
            maplist(load_biofile,FileL),
            ensure_loaded(bio(metadata_util)),
            forall(member(Key,Keys),
                   (   atom_to_term(Key,(Module:Pred/KeyIndex/NumArgs),[]),
                       map_identifiers(Module,Pred,KeyIndex,NumArgs))),
            write_biofile(ToFormat,OutFile))).

:- blip('multisim',
        'calculates similarity using the simmatrix_multiset module',
        [
	 bool(label,IsLabel),
         bool(write_prolog,IsProlog),
	 number(min_ic, MinIC, 3.5),
	 atom(cache,CacheFile),
	 atom([feature1,f1],F1),
         atom([feature2,f2],F2)
	],
	FileL,
        (   
            maplist(load_biofile,FileL),
            ensure_loaded(bio(simmatrix_multiset)),
	    Opts=[isProlog(IsProlog),
                  isLabel(IsLabel)],
	    create_sim_index(CacheFile),
	    %Goal=feature_pair_attx_pair_LCS(F1,F2,_S1,_S2,_LCS),
	    %forall((Goal),
	    Goal=feature_pair_attx_pair_LCS_IC(F1,F2,_S1,_S2,_LCS,IC),
	    forall((Goal, IC >= MinIC),
		   show_factrow(Opts,Goal)))).

:- blip('sim',
        'calculates similarity using the simmatrix module',
        [
	 bool(label,IsLabel),
         bool(write_prolog,IsProlog),
	 atom(metric,Metric),
	 term(simmatrix_hook,Hook),
	 number(min_score, MinScore, 0.5),
	 number(min_overlap, MinOverlap, 20),
	 %atom(cache,CacheFile),
	 atom([feature1,f1],F1),
         atom([feature2,f2],F2)
	],
	FileL,
        (   
            maplist(load_biofile,FileL),
            ensure_loaded(bio(simmatrix)),
	    Opts=[isProlog(IsProlog),
                  isLabel(IsLabel)],
	    ensure_loaded(bio(index_util)),
	    forall(simmatrix:generate_term_indexes_hook(Hook),
		   debug(sim,'generated: ~w',[Hook])),
	    (	var(Metric)
	    ->	Goal=feature_pair_ci_cu_simj(F1,F2,CI,_,S),
		Where= (CI>MinOverlap, S>MinScore)
	    ;	Goal=compare_feature_pair(F1,F2,S,[metric(Metric)]),
		Where= (S>MinScore)),
	    forall((Goal,Where),
		   show_factrow(Opts,Goal)))).

:- blip('sim-all',
        'calculates similarity using the simmatrix module; all by all from 2 files',
        [
	 bool(label,IsLabel),
         bool(write_prolog,IsProlog),
	 atom(metric,Metric),
	 term(fa_pred,Pred),
	 number(min_score, MinScore, 0.5),
	 number(min_overlap, MinOverlap, 20)
	],
	FileL,
        (   
            maplist(load_biofile,FileL),
            ensure_loaded(bio(simmatrix)),
	    Opts=[isProlog(IsProlog),
                  isLabel(IsLabel)],
	    ensure_loaded(bio(index_util)),
            Term =.. [Pred,A,B],
	    generate_term_indexes(A,B,Term),
	    (   var(Metric)
	    ->	Goal=feature_pair_ci_cu_simj(F1,F2,CI,_,S),
		Where= (CI>MinOverlap, S>MinScore)
	    ;   (   Metric=maxIC
                ->  Goal=compare_feature_pair(F1,F2,S-_Attrs,[metric(Metric)]),
                    Where= (S>MinScore)
                ;   Goal=compare_feature_pair(F1,F2,S,[metric(Metric)]),
                    Where= (S>MinScore))),
	    forall((pick_feature_pair(F1,F2,Pred),
                    %feature_exists(F1),
                    %feature_exists(F2),
                    %id_idspace(F1,S1),
                    %id_idspace(F2,S2),
                    %S1@<S2,
                    %F1\=F2,
                    Goal,
                    Where),
		   show_factrow(Opts,Goal)))).

pick_feature_pair(F1,F2,Pred) :-
        feature_exists(F1),
        feature_exists(F2),
        F1 @< F2,
        \+ features_from_same_source(F1,F2,Pred).

features_from_same_source(F1,F2,Pred) :-
        !,
        Term1 =.. [Pred,F1,_],
        Term2 =.. [Pred,F2,_],
        clause_source(Term1,S),
        clause_source(Term2,S).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   NCBI Remote fetching
   (                should this go in separate blipkit module?)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

blipkit:example('blip ncbi-fetch -db homologene 5276 -to homol_db:pro',
                'fetch homologene entry for ID 5276 from NCBI, writes out result as a homol_db prolog database').
opt_description(db,'Name of database').

:- blip('ncbi-fetch',
        'loads IDs from NCBI',
        [atom([to,t],ToFormat),
         atom([db],DB,pubmed),
         atom([o,output],OutFile)],
        IDs,
        (
         ensure_loaded(bio(web_fetch_ncbi)),
         web_fetch_ncbi_db_by_ids(DB,IDs),
         write_biofile(ToFormat,OutFile))).

blipkit:example('blip ncbi-search -db omim cancer',
                'search OMIM for OMIM IDs matching search term "cancer"').
blipkit:example('blip ncbi-search -db homologene "BRCA1[gene symbol]"',
                'search homologene for gene symbol "BRCA1" and write out homologene IDs').
blipkit:example('blip ncbi-search -fetch -db homologene "BRCA1[gene symbol]" -to homol_db:pro',
                'search homologene for gene symbol "BRCA1" and then fetch IDs, write results as homol_db database').
opt_description(fetch,'Also fetches ID from remote database').

:- blip('ncbi-search',
        'search for IDs from NCBI; optionally does an additional fetch on these IDs and writes',
        [atom([to,t],ToFormat),
         atom([db],DB,pubmed),
         bool([fetch],FetchRequested),
         atom([o,output],OutFile)],
        Searches,
        (
         ensure_loaded(bio(web_fetch_ncbi)),
         concat_atom(Searches,' ',SearchTerm),
         web_search_ncbi(DB,SearchTerm,IDs),
         maplist(format(user_error,'~w~n'),IDs),
         (   FetchRequested=1
         ->  web_fetch_ncbi_db_by_ids(DB,IDs),
             write_biofile(ToFormat,OutFile)
         ;   true))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   findall/setof
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
blipkit:example('blip -r obo/cell -u ontol_db findall synonym/3',
                'finds all solutions to an unground synonym(_,_,_) query - see ontol_db module for details; eg SELECT * FROM synonym').
blipkit:example('blip -r obo/cell -u ontol_db findall -label synonym/3',
                'as above, but uses entity_label/2 to attach labels to IDs').
blipkit:example('blip -r obo/cell -u ontol_db findall synonym(_,exact,_)',
                'finds all solutions to  synonym(_,exact,_) query - see ontol_db module for details; eg SELECT * FROM synonym WHERE col2="exact"').
blipkit:example('blip -r obo/cell -u ontol_db findall synonym(ID,T,_) -select ID-T -where "not(T=exact)"',
                'finds all solutions to  synonym(_,exact,_) query - see ontol_db module for details; eg SELECT * FROM synonym WHERE col2!="exact"').
blipkit:example('blip findall bioresource/2 bioresource/3 bioresource/4',
                'show all bioresources').

:- blip('findall',
        'finds all solutions of a predicate. Must be specified in Name/Arity form',
        [atoms(consult,Consults),
         atom(select,SelectAtom,true),
         bool(label,IsLabel),
         bool(use_tabs,IsUseTabs),
         bool(no_pred,IsNoPred),
         bool(write_prolog,IsProlog),
         atom(where,WhereAtom,true)],
        PredAtoms,
        (   trust_current_user,
            forall(member(File,Consults),
                   consult(File)),
            Opts=[isProlog(IsProlog),
		  isNoPred(IsNoPred),
		  isUseTabs(IsUseTabs),
                  isLabel(IsLabel)],
            maplist(show_findall(Opts,WhereAtom,SelectAtom),PredAtoms))).

show_findall(Opts,WhereAtom,SelectAtom,PredAtom):-
        ensure_loaded(bio(dbmeta)),
        sformat(Atom,'all((~w),(~w),(~w))',[WhereAtom,SelectAtom,PredAtom]),
        atom_to_term(Atom,all(Where,Select1,Pred1),_Bindings),
        (   Pred1=_/_
        ->  pred_to_unground_term(Pred1,Pred)
        ;   Pred=Pred1),
        (   nonvar(Select1),
            Select1=true
        ->  Select=Pred
        ;   Select=Select1),
        forall(user:meta(Pred),show_factrow(Opts,Pred)), % column headings
        (   member(distinct(true),Opts)
        ->  solutions(Select,(Pred,Where),Rows),
            maplist(show_factrow(Opts),Rows)
        ;   forall((Pred,Where),show_factrow(Opts,Select))).

:- blip('solutions',
        'finds all unique solutions of a predicate. Must be specified in Name/Arity form',
        [atoms(consult,Consults),
         atom(select,_SelectAtom,true), % TODO
         bool(write_prolog,IsProlog),
         bool(label,IsLabel)],
        [Pred],
        (   forall(member(File,Consults),
                   consult(File)),
            once_or_die(concat_atom([PredName,AritySym],'/',Pred),'Predicate must be of form Pred/Arity'),
            atom_number(AritySym,Arity),
            functor(Template,PredName,Arity),
            solutions(Template,Template,Templates),
            Opts=[isProlog(IsProlog),
                  isLabel(IsLabel)],
            maplist(show_factrow(Opts),Templates))).

%% show_factrow(+Opts,+Term)
%   writes a predicate out as a tab delimited line, with predicate name as first column
% @param Opts isLabel(1)
% @param Opts isProlog(1)

show_factrow(_,T) :-
        var(T),
        print_message(warning,blip(attempt_to_write_var(T))),
        !.
show_factrow(Opts,(T1,T2)):-
        % conjunctions
        !,
        show_factrow(Opts,T1),
        show_factrow(Opts,T2).
show_factrow(Opts,T):-          % write prolog syntax facts
        member(isProlog(1),Opts),
        \+member(isUseTabs(1),Opts),
        !,
        writeq(T),write('.'),
        (   member(isLabel(1),Opts)
        ->  T=..[_|L],write(' % '),show_terms(Opts,L)
        ;   true),
        (   member(noNewline(1),Opts)
        ->  true
        ;   nl).
show_factrow(Opts,T):-
        member(isNoPred(1),Opts),
        T=..[_|L],
        show_terms(Opts,L),
        nl.
show_factrow(Opts,T):-
        T=..L,
        show_terms(Opts,L),
        nl.

% show a list of terms separated by tabs on one line
% this is typically a prolog fact transformed into a list via ..=/2
show_terms(_Opts,[]).
show_terms(Opts,[H]):-
        !,
	show_term(Opts,H).
show_terms(Opts,[H|L]):-
        !,
        show_term(Opts,H),
        atom_codes(Del,[9]), % tab
        write(Del),
        show_terms(Opts,L).

show_term(Opts,T):-             % Pl+Tabular
        member(isProlog(1),Opts),
        member(isUseTabs(1),Opts),
        !,
	format('~q',[T]).
show_term(Opts,T):-             % Label+Tabular
        member(isLabel(1),Opts),
        member(isUseTabs(1),Opts),
        \+ is_list(T),
        \+ member(isHeader(1),Opts),
        !,
        (   atom(T),
	    entity_label(T,Label)
        ->  true
	;   Label=''),
	format('~w\t~w',[T,Label]).
show_term(Opts,T):-             % Label+atom, not tabular or pl
        member(isLabel(1),Opts),
        atom(T),
        entity_label(T,Label),
        Label\=T,
        !,
        write(T-Label).
show_term(Opts,L):-             % show a list in pl-like syntax
        member(isLabel(1),Opts),
        is_list(L),
        L=[_|_],
        !,
        write('[ '),
        forall(member(T,L),
               (   show_term(Opts,T),
                   write(' '))),
        write(']').
        %maplist(entity_label,L,L2),
        %!,
        %write(L-L2).
show_term(Opts,Term):-          % pl-like syntax for (a,b)
        member(isLabel(1),Opts),
        Term=(H,Rest),
        !,
        show_term(Opts,H),
        write(','),
        show_term(Opts,Rest).
show_term(Opts,Term):-          % pl-like syntax for key-val pairs
        member(isLabel(1),Opts),
        Term=(H-Rest),
        !,
        write(H),
        write('-'),
        show_term(Opts,Rest).
show_term(_,T):- write(T).      % default - just write it


opt_description(pred,'Name of predicate - defaults to process_line/1').
:- blip('iterate',
        'iterates over a file calling an unary predicate on each line',
        [atom(pred,Pred,process_line),
         atoms(consult,Consults)],
        FileL,
        (   forall(member(File,Consults),
                   consult(File)),
            forall(member(File,FileL),
                   iterate_over_file(File,Pred)))).

:- mode iterate_over_file(+,+) is det.
iterate_over_file(F,P):-
        open(F,read,IO),
        repeat,
        read_line_to_codes(IO,CL),
        (   CL=end_of_file
        ->  !
        ;   Goal =.. [P|[CL]],
            (   Goal
            ->  true
            ;   format(user_error,'Problem',[])),
            fail),
        close(IO).



% TODO: move to separate module - otherwise conflicts with pldoc_web
/*
:- use_module(library('http/http_dispatch')).
%:- use_module(library(pldoc)).
:- use_module(library(settings)).
:- setting(http:prefix, atom,'/blipdoc/', prefix).
:- http_handler(pldoc('module'),
                http_redirect(moved, pldoc_object),
                []).
*/

:- blip('pldoc-server',
        'use tools/pldoc-server instead',
        [number(port,Port,4000),
         number(workers,Workers,1),
         atom(root,Root,'blipdoc'),
         terms(opts,ServerOpts),
         bool([bg,background],Bg)],
        Files,
        (   ensure_loaded(library(pldoc)),
            doc_server(Port,[workers(Workers),prefix(Root),root(Root)|ServerOpts]),
            findall(File,(member(File,Files),
                          \+ exclude_file_from_pldoc(File)),
                    OKFiles),
            maplist(compile,OKFiles),
            compile(bio(sql_compiler)), % seems to be necessary...?
            (   Bg=1
            ->  background
            ;   true),
            prolog
        )).

exclude_file_from_pldoc(File):- sub_atom(File,_,_,_,'/attic/').
exclude_file_from_pldoc(File):- sub_atom(File,_,_,_,'blipkit.pro').


background:-
        repeat,
        fail.




% -- GENERAL --

% TODO: use time_goal
time_action(Goal,Time,Module):-
        statistics(cputime,T1),
        Module:Goal,
        statistics(cputime,T2),
        Time is T2-T1.

:- mode run_user_command(+,+,+) is det.
run_user_command(Cmd,InArgs,Tags):-
        (   trusted_command(Cmd)
        ->  true
        ;   (   trust_current_user
            ->  true
            ;   throw(permission('You do not have sufficient permission to run this command',Cmd)))),
        (   main(Cmd,_,Module,OptSpec,RemainingArgs,Modules,Action)
        ->  true
        ;   main(Cmd,_,OptSpec,RemainingArgs,Modules,Action), % deprec?
            Module=blipkit),
        !,
        (   member(help(1),Tags)
        ->  show_command(Cmd),
            show_optspec('Options specific to this command',OptSpec)
        ;   (   getopt(OptSpec,InArgs,RemainingArgs)
            ->  true
            ;   throw(error(getopt(OptSpec,InArgs,RemainingArgs)))),
            maplist(ensure_loaded,Modules),
            (   time_action(Action,Time,Module),
                debug(time,'action: ~w in ~w',[Action,Time])
            ->  true
            ;   throw(error(command_failed(Cmd))))).
run_user_command(Cmd,_,_):-
        format(user_error,'No such command: ~w~n',[Cmd]).
        
usage(Spec):-
        writeln('Usage: blip <OPTIONS> COMMAND <COMMAND-OPTIONS> <COMMAND-ARGS>'),
        show_optspec('General Options',Spec),
        nl,
        write('Options marked [*] can be specified multiple times'),
        nl,
        show_commands,
        show_examples.
        
show_optspec(Type,Spec):-
        format('~w:~n',[Type]),
        maplist(show_opt,Spec).

fmt_opt(Opt,bool,Fmt):-
        concat_atom(['-',Opt],Fmt),
        !.
fmt_opt(Opt,atom,Fmt):-
        concat_atom(['-',Opt,' ATOM'],Fmt),!.
fmt_opt(Opt,atoms,Fmt):-
        concat_atom(['-',Opt,' ATOM'],Fmt),
        !.
fmt_opt(Opt,number,Fmt):-
        concat_atom(['-',Opt,' NUMBER'],Fmt),!.
fmt_opt(Opt,Type,Fmt):-
        upcase_atom(Type,TypeUC),
        concat_atom(['-',Opt,' ',TypeUC],Fmt),!.

is_plural_type(atoms).
is_plural_type(numbers).

show_opt(X):-
        X =.. [Type,Opt|_Rest],
        (is_list(Opt) -> Opts=Opt ; Opts=[Opt]),
        findall(Fmt,
                (   member(OptX,Opts),
                    fmt_opt(OptX,Type,Fmt)),
                Fmts),
        concat_atom(Fmts,', ',Out),
        write(Out),
        (is_plural_type(Type) -> write(' [*]') ; true),
        nl,
        (   member(OptX,Opts),
            opt_description(OptX,Desc)
        ->  format('    ~w~n',[Desc])
        ;   true).
        
show_commands:-
        format('~nCommands:~n',[]),
        forall(main(Cmd,_,_,_,_,_,_),
               show_command(Cmd)),
        nl.

show_command(Cmd):-
        main(Cmd,Desc,_,_,_,_,_),
        format('~w ~n    ~w~n~n',[Cmd,Desc]).
                      
show_examples:-
        format('~nExamples:~n',[]),
        forall(example(Ex),
               format(' ~w~n~n',[Ex])),
        forall(example(Ex,Notes),
               format(' ~w~n    ~w~n~n',[Ex,Notes])),
        nl.

once_or_die(G,M):-
        (   G
        ->  true
        ;   throw(user_error(M,G))).
        
prolog:message(welcome) -->
               ['%  ::: Welcome to blip (Version ',V,') :::'],
               {program_info(package(_,V));V=unknown}.

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.19 $
  @date  $Date: 2006/02/10 23:29:38 $
  @license LGPL

  ---+ Name
  ---++ blipkit
- simple interface to blip module functionality

  ---+ Synopsis

  From the command line:

  ==
  blip -h
  @\cl

  ---+ Description

  ---+ Examples

  To be run from the shell:
  
  ==
  blip -h ontol-subset
  blip -r go ontol-subset -n 'oxygen transport'
  blip -f obo -i my_ont.obo ontol-subset -id MY:0000001
  
  blip -h io-convert
  ==

  Interacting with the prolog shell:

  ==
  blip -r cell
  <type prolog here>
  <ctrl-D>
  /==

  ---++ debug options

  
  * blip
  general debug messages
  * time
  shows time taken to execute main command
  

  enabling debug may remove tail recursion causing stack overflow in
some rare circumstances??
  */
