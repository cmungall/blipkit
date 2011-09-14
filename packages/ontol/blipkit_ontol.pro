/* -*- Mode: Prolog -*- */


:- module(blipkit_ontol,[]).

:- use_module(bio(mode)).
:- use_module(bio(blipkit)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_segmenter)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_lookup)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_text)).
:- use_module(bio(ontol_writer_obo)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(graph)).
:- use_module(bio(io)).

blipkit:opt_description(id,'Query on entity unique identifier, eg GO:0008045').
blipkit:opt_description(name,'Query on entity name, eg "exon", "Rab1"').
blipkit:opt_description(synonyms,'If set, display synonyms').
blipkit:opt_description(reltype,'Show only this relationship type, eg part_of').
blipkit:opt_description(relation,'Name of relation to traverse').
blipkit:opt_description(stem,'Perform porter stemming on both input and ontologies').
blipkit:opt_description(showrel,'write out this relation ("all" for all relations). Does not show in denorm tree').

blipkit:example('blip -r obop/biological_process ontol-query -n "%neuron%"',
                'show all classes with names containing "neuron"').
blipkit:example('blip -r obop/go ontol-query -n "%transporter%" -stem',
                'search using wildcard and porter stemming').
blipkit:example('blip -r obop/biological_process ontol-query -queryfile names.txt -showdefs -showsyns',
                'search all in newline-separated file of names').
blipkit:example('blip -r obop/biological_process ontol-subset -n "T cell costimulation"',
                'show DAG subset using indented tree format').
blipkit:example('blip -r obop/biological_process ontol-subset -id GO:0051251 -to dot | display -',
                'show DAG subset using graphviz dot format piped to a dot viewer').
blipkit:example('blip -r fma -n Cell -rel has_part -reverse',
                'show has_part explosion for Cell (placing Cell at root of tree)').
blipkit:example('blip -r fly_anatomy ontol-subset -n ommatidium -rel part_of -sib',
                'show inferred part_of lineage for anatomical part, and its part_of siblings').
blipkit:example('blip -u ontol_sqlmap_obd -r rdb/obd ontol-subset -n apoptosis',
                'query relational database with OBD schema for apoptosis graph').
blipkit:example('blip -r obo/go -r go_assoc/fb ontol-subset -n apoptosis -showannots focus -table_pred ontol_db:parentT/2',
                'query relational database with OBD schema for apoptosis graph').
blipkit:example('blip -r zebrafish_anatomy ontol-subset -n "neural tube" -rel develops_from -down 1 -showrel start -showrel end',
                'show developmental lineage for anatomical part, show start/end relations but do not use them for graph structure').
blipkit:example('blip -r taxnames -r taxnodes -use taxon_bridge_to_ontol ontol-subset -n "Homo sapiens"',
        'show human lineage. use a bridge to make a taxonomy look like an ontology').
blipkit:example('blip -r taxnames -r taxnodes -use taxon_bridge_to_ontol ontol-coverset NCBITaxon:7227 NCBITaxon:9606 ',
        'show most specific clade containing fly, human').
blipkit:example('blip -r obop/go -include ontology//cellular_component  io-convert -to obo -o cc.obo',
                'write GO to obo format file, but only include the cellular component ontology').
blipkit:example('blip -r obop/go ontol-coverset [list of GO IDs]').

:- blip('ontol-validate',
        'basic ontology validation',
        [],
        _,
        validate_ontols).

shownode(ID):- write_class(text,ID),nl.
shownode(T,ID,Opts):- write_class(text,ID,[prefix(T)|Opts]),nl.
shownode_nonl(T,ID,Opts):- write_class(text,ID,[prefix(T)|Opts]).
                                

%:- mode validate_ontols is det.
validate_ontols:-
        closure_contains_cycle(subclass,IDs),
        !,
        format('Cannot check: ontology has subclass cycles: ~w~n',[IDs]).
validate_ontols:-
        class(ID),
        parent_cycle(ID,Cycle),    % todo - be more lenient with cyclic rels
        !,
        format('Cannot check: ontology has link cycles: ~w~n',[Cycle]).
        
validate_ontols:-
        !,
        forall_distinct(redundant_subclass(ID,PID,ViaID),
               (   format('Redundant subclass: ~n',[]),
                   shownode(-term,ID,[]),
                   shownode(-ancestor,PID,[]),
                   shownode(-redundant_via,ViaID,[]))),
        forall_distinct(redundant_parent(ID,T,PID,ZID,TL),
               (   format('Redundant relationship: ~n',[]),
                   shownode(-from,ID,[]),
                   shownode(-T,PID,[]),
                   shownode(-is_a_ancestor,ZID,[]),
                   writeln(-subsuming_path=TL))),
        format('Validation complete~n').
        
        
:- blip('ontol-stats',
        'various statistics on one ontology',
        [],
        _,
        (   setof(Ont,ID^belongs(ID,Ont),Onts),
            load_bioresource(relationship),
            forall(member(Ont,Onts),
                   blipkit_ontol:summarise_ontol(Ont)))).

in_obo_rel(subclass):- !.
in_obo_rel(T):- belongs(T,relationship),!.
in_obo_rel(T):- property(ID,T),belongs(ID,relationship). % weak test; same name does not automatically mean same relation

% (?Name,?Ont,?ID,?Goal)
ontol_stat(N,O,ID,G):-
        ontol_stat(N,O,ID,G,_).
ontol_stat(classes,Ont,ID,(belongs(ID,Ont),class(ID)),
           'total number of non-obsolete classes (terms) in ontology').
ontol_stat(relations,Ont,ID,(belongs(ID,Ont),property(ID,_)),
           'total number of distinct relations defined').
ontol_stat(with_definitions,Ont,ID,(belongs(ID,Ont),class(ID),def(ID,_)),
           'total non-obsolete classes with text definitions').
ontol_stat(without_definitions,Ont,ID,(belongs(ID,Ont),class(ID),\+def(ID,_)),
           'total non-obsolete classes WITHOUT text definitions').
ontol_stat(with_definition_xrefs,Ont,ID,(belongs(ID,Ont),class(ID),def_xref(ID,_)),
           'total text definitions with provenance').
% too slow on FMA2:
%ontol_stat(violating_univocity,Ont,ID,(belongs(ID,Ont),class(ID),entity_pair_is_non_univocal(ID,_)),
%           'total classes violating univocity (having same name as another class)').
ontol_stat(distinct_relations_used,Ont,T,
           (belongs(ID,Ont),( restriction(ID,T,_)
                            ; subclass(ID,_),T=is_a)),
           'all relations used').
% this is too slow; and of questionable use
%ontol_stat(distinct_relations_not_in_obo_rel,Ont,T,
%           (belongs(ID,Ont),restriction(ID,T,_),\+in_obo_rel(T)),
%           'list of relations used not in OBO Relations ontology').
ontol_stat(relationships,Ont,ID-PID,
           (belongs(ID,Ont),parent(ID,_,PID)),
           'total relations between classes').
ontol_stat(logical_definitions,Ont,ID-GID,
           (belongs(ID,Ont),genus(ID,GID)),
           'total classes with computable genus-differentia definitions (cross-products)').
ontol_stat(subclass_orphans,Ont,ID,(   belongs(ID,Ont),
                                         \+ subclass(ID,_),
                                         restriction(ID,_,_)),
           'total non-obsolete classes with missing is_a parent, excluding root node').
ontol_stat(only_children,Ont,ID,(   belongs(ID,Ont),
                                    subclass(ID,PID),
                                    \+ ((subclass(XID,PID),
                                         XID\=ID))),
           'total classes that are the only is_a child of their parent').
ontol_stat(obsoletes,Ont,ID,obsolete(ID,_,Ont),
           'total number of obsoleted/retired classes').
ontol_stat(isa_cycles,Ont,P,(belongs(ID,Ont),subclass_cycle(ID,P)),
           'number of cycles in is_a in ontology (should be zero)').
ontol_stat(cycles,Ont,P,(belongs(ID,Ont),parent_cycle(ID,P)),
           'number of cyclic paths in ontology').
%ontol_stat(def_xref_idspaces,Ont,DBNumPairs,
%           (   belongs(ID,Ont),count_by(ID,DB,(def(ID,X),split_id(X,DB,_)),DBNumPairs)),
%           'all distict ID short prefixes (eg GO,CL) used in ontology').
ontol_stat(redundant_relationships,Ont,PID,
           (belongs(ID,Ont),redundant_parent(ID,_,PID,_,_)),
           'number of class to class relations that can be safely removed').
ontol_stat(redundant_subclasses,Ont,ID-PID,
           (belongs(ID,Ont),redundant_subclass(ID,_,PID)),
           'number of subclass/is_a relations that can be removed').
ontol_stat(distinct_idspaces,Ont,DB,
           (belongs(ID,Ont),split_id(ID,DB,_)),
           'all distict ID short prefixes (eg GO,CL) used in ontology').
ontol_stat(distinct_subsets,Ont,Subset,
           (belongs(ID,Ont),entity_partition(ID,Subset)),
           'all distinct subsets (partitions, views, slims) in ontology').

% only the following are listed - the others are just counts
show_as_list(distinct_relations_used).
show_as_list(distinct_relations_not_in_obo_rel).
show_as_list(distinct_idspaces).
show_as_list(distinct_subsets).

% per-class statistics - aggregate operations are performed on
% these

% TODO - exclude meaningless stats; eg do not count root or leaf nodes for parent/child counts
ontol_class_stat(N,ID,Template,Goal):-
        ontol_class_stat(N,ID,Template,Goal,_).
ontol_class_stat(non_is_a_parents,ID,PID,restriction(ID,_,PID),
                 'number of parents (not of type is_a) per class').
ontol_class_stat(is_a_parents,ID,PID,subclass(ID,PID),
                 'number of is_a parents per class').
ontol_class_stat(parents,ID,PID,parent(ID,_,PID),
                 'number of parents (all relations) per class').
ontol_class_stat(is_a_children,ID,CID,subclass(CID,ID),
                 'number of is_a children per class').
ontol_class_stat(all_children,ID,CID,((\+ \+ parent(_,_,ID)),parent(CID,_,ID)),
                 'number of children (all relations) per class').
%ontol_class_stat(non_is_a_children,ID,CID,restriction(CID,_,ID), -- need a faster way...
%                 'number of children (non isa) per class').
ontol_class_stat(synonyms,ID,S,entity_synonym(ID,S),
                 'number of synonyms per class').

ontol_class_stat_exclude(is_a_children,ID):- \+ subclass(_,ID).

% (+,?) det
get_stat_expl(N,E):-
        ontol_stat(N,_,_,_,E),
        !.
get_stat_expl(N,E):-
        ontol_class_stat(N,_,_,E),
        !.
get_stat_expl(N,N).

split_id(ID,DB,LocalID):-
        concat_atom([DB|Rest],':',ID),
        Rest=[_|_],
        concat_atom(Rest,':',LocalID).

% check ontology has at least one class - or is relations ontology
summarise_ontol(Ont):-
        (   belongs(ID,Ont),
            class(ID)
        ;   Ont=relationship),
        !,
        summarise_ontol1(Ont).
summarise_ontol(_). % do nothing

summarise_ontol1(Ont):-
        format('Ontology: ~w~n',[Ont]),
        
        % total number of X in ontology
        findall(Stat-Num,
                (   ontol_stat(Stat,Ont,ID,Goal),
                    debug(blipkit,'stat [count]: ~w [~w]',[Stat,ID/Goal]),
                    time_goal(setof_count(ID,Goal,Num),Time),
                    debug(blipkit,'stat [count]: ~w [~w] = ~w in ~w',[Stat,ID/Goal,Num,Time])
                ),
                Stats),
        forall((member(Stat-Num,Stats),get_stat_expl(Stat,Expl)),
               format('  number_of_~w: ~w  ## ~w~n',[Stat,Num,Expl])),

        % list all X in ontology
        findall(Stat-L,
                (   ontol_stat(Stat,Ont,ID,Goal),
                    show_as_list(Stat),
                    debug(blipkit,'stat: ~w [~w]',[Stat,Goal]),
                    time_goal(dsetof(ID,Goal^Goal,L),Time),
                    debug(blipkit,'stat: ~w [~w] = ~w in ~w',[Stat,Goal,L,Time])
                ),
                LStats),
        forall((member(Stat-L,LStats),get_stat_expl(Stat,Expl)),
               format('  all_~w: ~w ## ~w~n',[Stat,L,Expl])),

        % min,max,avg Xs per class in ontology
        forall((   ontol_class_stat(Stat,ID,X,Goal),
                   get_class_stats(Stat,Ont,ID,X,Goal,Min,Max,Avg),
                   format('  avg_number_of_~w_per_class: ~w~n',[Stat,Avg]),
                   format('  min_number_of_~w_per_class: ~w~n',[Stat,Min]),
                   format('  max_number_of_~w_per_class: ~w~n',[Stat,Max])),
               true),

        % path stats
        findall(ID,(belongs(ID,Ont),noparent(ID)),RootIDs),
        (   RootIDs=[RootID|Rest]
        ->  (   Rest=[]
            ->  true
            ;   format('  multiple_roots: true~n',[])),
            solutions(parent_over_nr(R)-R,is_transitive(R),RRNs),
            forall(member(R-RN,
                          [subclass-'is_a',
                           parent-'all_relations'|RRNs
                           ]),
                   (   ontol_pathstats(Ont,RootID,R,
                                       stats(AvgD,MaxD,NPaths,AvgPaths,MaxPaths,NumRoots))
                   ->  format('  average_distance_from_root_over_~w: ~w~n',[RN,AvgD]),
                       format('  number_of_roots_over_~w: ~w~n',[RN,NumRoots]),
                       format('  max_distance_from_root_over_~w: ~w~n',[RN,MaxD]),
                       format('  total_paths_to_root_over_~w: ~w~n',[RN,NPaths]),
                       format('  average_paths_to_root_over_~w: ~w~n',[RN,AvgPaths]),
                       format('  max_paths_to_root_over_~w: ~w~n',[RN,MaxPaths])
                   ;    true))
        ;   format('  no_roots: true~n',[])),
        nl.

get_class_stats(Stat,Ont,ID,X,Goal,Min,Max,Avg):-
        !,
        findall(Num,
                (   belongs(ID,Ont),
                    setof_count(X,Goal,Num),
                    \+ ontol_class_stat_exclude(Stat,ID)),
                Nums),
        list_max(Nums,Max),
        list_min(Nums,Min),
        list_avg(Nums,Avg).
        
% TODO: also define for relations such as part_of, where..
% (+ID,+RootID,+RelPred,stats(ID,?,?,?,?))
class_pathstats(ID,_RootID,R,stats(ID,AvgD,MaxD,NumPaths)):-
        closure_to_edgelist(R,ID,Edges),
        findall(D,(dag_path_to_root(Edges,ID,_,P),length(P,D),D>0),Ds),
        length(Ds,NumPaths),
        list_avg(Ds,AvgD),
        list_max(Ds,MaxD).

% (+ID,+RootID,+RelPred,?Stats)
ontol_pathstats(Ont,RootID,R,stats(AvgD,MaxD,NPaths,AvgPaths,MaxPaths,NumRoots)):-
        \+ closure_contains_cycle(subclass),
        \+ closure_contains_cycle(R),
        findall(S,(belongs(ID,Ont),class_pathstats(ID,RootID,R,S)),Ss),
        \+ Ss=[],
        findall(Root,(belongs(Root,Ont),
                      class(Root),
                      \+call(R,Root,_)),Roots),
        length(Roots,NumRoots),
        findall(AvgD,member(stats(_,AvgD,_,_),Ss),AvgDs),
        findall(MaxD,member(stats(_,_,MaxD,_),Ss),MaxDs),
        findall(NumPaths,member(stats(_,_,_,NumPaths),Ss),AllNumPaths),
        sumlist(AllNumPaths,NPaths),
        list_avg(AllNumPaths,AvgPaths),
        list_max(AllNumPaths,MaxPaths),
        list_avg(AvgDs,AvgD),
        list_max(MaxDs,MaxD).


:- blip('ontol-coverset',
        'LCA. given ids, find minimal id(s) that cover this',
        [atom([relation,rel],Rel,parent),
         bool(inverse,Inverse)],
        IDs,
        (   forall(member(ID,IDs),
                   (   class(ID)
                   ->  blipkit_ontol:shownode(input,ID,[])
                   ;   die(no_such_class(ID)))),
            (Inverse=1 -> Rel2=inverse(ontol_db:Rel) ; Rel2=ontol_db:Rel),
            (   setof(MSID,
                      graph:minimal_spanning_node(closure(Rel2),IDs,MSID),
                      MSIDs)
            ->  writeln('Coverset:'),
                forall(member(ID,MSIDs),
                       (   blipkit_ontol:shownode(ms,ID,[])))
            ;   die(no_covering_nodes(Rel,IDs))))).

link(ID,PID,T):- parent(ID,T,PID).
link(ID,PID):- link(ID,PID,_).

:- blip('ontol-xp-matrix',
        'shows cross-product matrix',
        [bool(flip,Flip),
         atom(to,Format,text)],
        FileL,
        (   maplist(load_biofile,FileL),
            solutions(G,(genus(_,G1),subclassRT(G1,G)),Gs),
            solutions(DC,(differentium(G,_,DC1),subclassRT(DC1,DC)),DCs),
            Opts=[format(Format),row(RowFor)],
            (   Flip=0
            ->  RowFor=genus,
                show_xp_matrix(Gs,DCs,Opts)
            ;   RowFor=diff,
                show_xp_matrix(DCs,Gs,Opts)))).
            
:- mode show_xp_matrix(+,+,+) is det.
show_xp_matrix(RowClasses,ColClasses,Opts):-
        closure_to_edgelist(subclass,RowClasses,RowEdges),
        edgelist_to_trees(RowEdges,RowTrees),
        member(format(Format),Opts),
        show_xp_matrix_1(Format,RowTrees,ColClasses,Opts).

:- mode show_xp_matrix_1(+,+,+,+) is det.
show_xp_matrix_1(html,RowTrees,ColClasses,Opts):-
        !,
        user:ensure_loaded(bio(serval)),
        user:consult(amigo_src(amigo_term)),
        write_sterm(onto_xp_matrix(RowTrees,ColClasses,Opts)).
        
show_xp_matrix_1(text,RowTrees,ColClasses,Opts):-
        !,
        show_xp_matrix_header(ColClasses,Opts),
        forall(member(RowTree,RowTrees),
               show_xp_matrix_row(0,RowTree,ColClasses,Opts)).

:- mode show_xp_matrix_header(+,+) is det.
show_xp_matrix_header(ColClasses,Opts):-
        (   member(row(genus),Opts)
        ->  write('rows=genus\\cols=diff')
        ;   write('rows=diff\\cols=genus')),
        write('|'),
        forall(member(ColClass,ColClasses),
               (   show_class(ColClass),write('|'))),
        nl.
        
:- mode show_xp_matrix_row(+,+,+,+) is det.
show_xp_matrix_row(Indent,node(_,RowClass,SubNodes),ColClasses,Opts):-
        Indent1 is Indent+1,
        writetab(Indent1),
        show_class(RowClass),
        format(' |'),
        member(row(RowFor),Opts),
        forall(member(ColClass,ColClasses),
               show_xp_matrix_cell(RowClass,ColClass,RowFor)),
        nl,
        forall(member(Node,SubNodes),
               show_xp_matrix_row(Indent1,Node,ColClasses,Opts)).

:- mode show_xp_matrix_cell(+,+,+) is det.
show_xp_matrix_cell(RowClass,ColClass,genus):-
        !,
        show_xp_matrix_cell_1(RowClass,ColClass).
show_xp_matrix_cell(RowClass,ColClass,_):-
        show_xp_matrix_cell_1(ColClass,RowClass).

:- mode show_xp_matrix_cell_1(+,+) is det.
show_xp_matrix_cell_1(Genus,DiffClass):-
        forall((genus(DefinedClass,Genus),
                differentium(DefinedClass,R,DiffClass)),
               (   write('['),
                   show_class(DefinedClass),
                   format(' (~w)',[R]),
                   forall((differentium(DefinedClass,R2,DiffClass2),
                           \+ ((R2=R,DiffClass=DiffClass2))),
                          (   format('; also: ~w(',[R2]),
                              show_class(DiffClass2),
                              format(')'))),
                   format(']'))),
        format('|').

        

:- blip('ontol-def',
        'shows cross-product definition of a class',
        [atom([id],ID1),
         atom([name,n],N1)],
        FileL,
        (   load_factfiles(FileL),
            class(ID1,N1),
                                %setof(PID,subclassRT(ID1,PID),PIDs),
            setof(PID,subclass(ID1,PID),PID1s),
            PIDs=[ID1|PID1s],
            solutions(XID,(member(PID,PIDs),
                           (genus(PID,XID) ; differentium(PID,T,XID))),
                      XIDs),
            solutions(XPID,(member(XID,XIDs),
                            subclassRT(XID,XPID)),
                      XPIDs),
            
            ensure_loaded(bio(graphviz)),
            Style=[prop(style=filled)],
            solutions(Terms,
                      (   (   member(ID,PIDs)
                          ;   member(ID,XIDs)
                          ;   member(ID,XPIDs)),
                          class(ID,N),
                          Main=node(ID,[label=N,fillcolor=red,shape=box]),
                          solutions(edge(ID,PID,[label=is_a,weight=100]),
                                    subclass(ID,PID),
                                    Edges1),
                          solutions(edge(ID,PID,[label=genus,color=blue,weight=200]),
                                    genus(ID,PID),
                                    Edges2),
                          solutions(edge(ID,PID,[label=diff(T),color=blue,style=dashed,weight=200]),
                                    differentium(ID,T,PID),
                                    Edges3),
                          flatten([Main,Edges1,Edges2,Edges3],
                                  Terms)),
                      TermsL),
            flatten(TermsL,AllTerms),
            graph_to_dot(graph(g,[Style|AllTerms]),Dot),
            writeln(Dot),
            nl)).

:- blip('ontol-diff',
        'performs a diff on two ( or more ?) files',
        [],
        InFiles,
        (   ensure_loaded(bio(dbmeta)),
            ensure_loaded(bio(ontol_db)),
            db_facts(ontol_db,OrigFacts),
            %length(OrigFacts,OrigFactsLen),
            %writeln(origfacts=OrigFactsLen),
            maplist(load_biofile,InFiles),
            db_facts(ontol_db,AllFacts),
            %length(AllFacts,AllFactsLen),
            %writeln(allfacts=AllFactsLen),
            %solutions(Fact,(member(Fact,AllFacts),\+ member(Fact,OrigFacts)),Facts),
            list_subtract(AllFacts,OrigFacts,Facts),
            %length(Facts,FactsLen),
            %writeln(newfacts=FactsLen),
            !,
            findall(File-TrFact,(member(Fact,Facts),
                                 trfact(Fact,TrFact),
                                 clause(ontol_db:Fact,_,ClauseID),
                                 clause_property(ClauseID,file(FileFull)),
                                 concat_atom(Parts,'/',FileFull),
                                 last(Parts,File)),
                    FFSet),
            solutions(File,member(File-Fact,FFSet),Files),
            solutions(ID,member(_-parent(ID,_,_),FFSet),IDs),
            forall(member(ID,IDs),
                   (   blipkit:show_factrow([isLabel(1)],comparing(ID)),
                       forall(member(File,Files),
                              ontol_diff(ID,File,FFSet)))))).


ontol_diff(ID,File,FFSet):-
        % find ALL other FFSets
        findall(File2-Fact2,
                (   member(File2-Fact2,FFSet),File2\=File),
                FFSet2),
        % compare against all (TODO: one at a time?)
        forall((member(File-Fact,FFSet),Fact=parent(ID,_,_)),
               show_all_ontol_diff_fact(File,Fact,FFSet2)).

trfact(subclass(X,Y),parent(X,subclass,Y)).
trfact(restriction(X,R,Y),parent(X,R,Y)).


show_all_ontol_diff_fact(File,Fact,FFSet):-
        forall(ontol_diff_fact(Fact,FFSet,Detail),
               (   Fact=..L1,
                   Detail=..L2,
                   append(L1,L2,L),
                   Row=..[cmp,File|L],
                   blipkit:show_factrow([isLabel(1)],Row))).

ontol_diff_fact(Fact,FFSet,equal):-
        % first step is purely for optimization: if we have an exact match
        % we can find it faster using member/2. If no exact match, choose
        % any member deterministically
        member(_-Fact,FFSet),
        !.
ontol_diff_fact(parent(X,R,Y),FFSet,rel_mismatch(R2\=R,F)):-
        member(F-parent(X,R2,Y),FFSet),
        !.
ontol_diff_fact(parent(X,R,Y),FFSet,make_target_more_general(Z,F)):-
        member(F-parent(X,R,Z),FFSet),
        parentT(Y,Z),
        \+ ((        member(_-parent(X,R,Z2),FFSet),
                     parentT(Y,Z2),
                     parentT(Z2,Z))),
        !.
ontol_diff_fact(parent(X,R,Z),FFSet,make_target_more_specific(Y,F)):-
        member(F-parent(X,R,Y),FFSet),
        parentT(Y,Z),
        \+ ((        member(_-parent(X,R,Z2),FFSet),
                     parentT(Y,Z2),
                     parentT(Z2,Z))),
        !.
ontol_diff_fact(parent(X,R,Y),FFSet,make_subject_more_general(Z,F)):-
        member(F-parent(Z,R,Y),FFSet),
        parentT(X,Z),
        !.
ontol_diff_fact(parent(X,R,Y),FFSet,make_subject_more_specific(Z,F)):-
        member(F-parent(Z,R,Y),FFSet),
        parentT(Z,X),
        !.
ontol_diff_fact(_,_,nomatch).


% ============================================================ %

% queries by ID, name or synonym
matching_class(ID,N,_Opts):-
        nonvar(ID),
        entity_synonym(ID,N),
	!.
matching_class(ID,N,Opts):-
        (   member(stem(1),Opts)
        ->  Stem=1
        ;   Stem=0),
        (   setof(ID-N,entity_label_or_synonym(ID,N),Pairs) % exact match?
        ->  true
        ;   (	var(ID),
		setof(N-N,class(N),Pairs) % ID=N -
	    ->	true
            ;	setof(ID-N,lookup_class(search(N,Stem),ID),Pairs))),
        !,
        member(ID-N,Pairs).

% deprecated
xxxmatching_class(ID,N,Opts):-
        (   member(stem(1),Opts)
        ->  Stem=1
        ;   Stem=0),
        (   setof(ID-N,entity_label(ID,N),Pairs) % exact match?
        ->  true
        ;   (   nonvar(N),
                setof(ID-N,synonym(ID,_,N),Pairs)
            ->  true
            ;   var(ID),
                setof(N-N,class(N),Pairs) % ID=N -
            ->  true
            ;   setof(ID-N,lookup_class(search(N,Stem),ID),Pairs))),
        !,
        member(ID-N,Pairs).
        



:- blip('ontol-augment',
        'searches against one ontology via another',
        [
         bool(showsyns,WithSynonyms),
         bool([stem],Stem),
         bool(showcomments,WithComments),
         bool(showisa,ShowIsA),
         atoms(showrel,ShowRels),
         bool(hideids,HideID),
         bool([instances,showinstances],WithInstances),
         bool(showdefs,ShowDefs)],
        [OntSubj,OntObj],
        (   
          Opts=[stem(Stem),
                with_synonyms(WithSynonyms),
                showcomments(WithComments),
                showisa(ShowIsA),
                showrels(ShowRels),
                hideid(HideID),
                showinstances(WithInstances),
                showdefs(ShowDefs)],
          ensure_nonvar(WithComments,0),
          ensure_nonvar(WithSynonyms,0),
          ensure_nonvar(WithInstances,0),
          ensure_nonvar(ShowDefs,0),
          ensure_nonvar(ShowIsA,0),

          forall(   (belongs(Class,OntObj),class(Class,_)),
                    augment_ont_using_class(OntSubj,Class,Opts)))).

augment_ont_using_class(Ont,Class,Opts):-
        class_by_name_or_synonym(Query,Class),
        debug(blipkit,'Searching for "~w" in ~w',[Query,Ont]),
        matching_class(SubjClass,Query,Opts),
        debug(blipkit,'Testing for "~w" in ~w',[SubjClass,Ont]),
        belongs(SubjClass,Ont),
        !,
        class(SubjClass,SubjName),
        % subject ontology has class - but does it have a def?
        (   def(SubjClass,_)
        ->  format('ok: ~w~n',[SubjClass]) % subj is good
        ;   (   def(Class,Def)
            ->  format('def: ~w (~w) "~w"~n',[SubjClass,SubjName,Def])
            ;   true)).
augment_ont_using_class(_,Class,Opts):-
        shownode('',Class,Opts).

:- blip('map-labels',
        'reads a file swapping labels for IDs',
        [],
        Files,
        (   maplist(map_labels,Files))).

map_labels(File):-
        format(user_error,'Reading File: ~w~n',[File]),
        open(File,read,IO),
        atom_codes(Del,[9]),
        repeat,
        read_line_to_codes(IO,Codes),
        (   Codes=end_of_file
        ->  !
        ;   atom_codes(LineAtom,Codes),
            concat_atom(Fields,Del,LineAtom),
            findall(X,(member(Field,Fields),map_field(Field,X)),Xs),
            writecols(Xs),
            nl,
            fail).

map_field(Label,ID):-
        entity_label(ID,Label),
        !.
map_field(X,X).

blipkit:trusted_command('ontol-query').
blipkit:opt_description(mireot,'An IDSpace for a source ontology used to build a minimal MIREOT ontology').
user:opt_insecure(queryfile).
user:opt_insecure(query).
:- blip('ontol-query',
        'queries classes from an ontology',
        [atoms([id],IDs),
         atoms([name,n],QueryNames),
         bool([stem],Stem),
         atom([queryfile,qf],TokensFile),
         atom(query,OntolQueryAtom),
         atom(mireot,Mireot),
         atom(mireot_ext,MireotExt),
         atom([to,t],OutFmt,textnl),
         bool(showsyns,WithSynonyms),
         bool(showcomments,WithComments),
         bool(showisa,ShowIsA),
         bool(showobsoletes,ShowObs),
         atoms(showrel,ShowRels),
         bool(showxp,ShowXP),
         bool(showsubsets,IsShowSubsets),
         bool(showxrefs,ShowXrefs),
         bool(showinvxrefs,ShowInvXrefs),
	 atom([showannots,showannot],ShowAnnot,no),
         atoms(subset,Subsets),
         bool(hideids,HideID),
         bool([instances,showinstances],WithInstances),
         bool(showdefs,ShowDefs)],
        FileL,
        (   
            (   (   var(QueryNames),var(TokensFile),var(OntolQueryAtom),IDs=[])
            ->  QueryNames=FileL
            ;   load_factfiles(FileL)),
            Opts=[stem(Stem),
                  with_synonyms(WithSynonyms),
                  showcomments(WithComments),
                  showisa(ShowIsA),
                  showrels(ShowRels),
                  showxp(ShowXP),
                  showsubsets(IsShowSubsets),
                  showxrefs(ShowXrefs),
                  showinvxrefs(ShowInvXrefs),
                  subsets(Subsets),
                  showobsoletes(ShowObs),
                  hideid(HideID),
                  showinstances(WithInstances),
		  showannots(ShowAnnot),
                  showdefs(ShowDefs)],
            ensure_nonvar(WithComments,0),
            ensure_nonvar(WithSynonyms,0),
            ensure_nonvar(WithInstances,0),
            ensure_nonvar(ShowDefs,0),
            ensure_nonvar(ShowXP,0 ),
            ensure_nonvar(IsShowSubsets,0 ),
            ensure_nonvar(ShowIsA,0),
            (   OutFmt=obo
            ->  ontol_writer_obo:write_header(OutFmt)
            ;   true),
                                % find all classes matching search criterion
            (   nonvar(TokensFile)
            ->  ontol_query_from_tokens_file(TokensFile,OutFmt,Opts)
            ;   nonvar(OntolQueryAtom)
            ->  atom_to_term(OntolQueryAtom,OntolQuery,Bindings),
                member('ID'=ID,Bindings),
                ontol_run_query(OntolQuery,ID,OutFmt,Opts)
            ;   nonvar(Mireot)
            ->  ontol_run_query(idspace_mireot(Mireot,ID,MireotExt),ID,OutFmt,Opts)
            ;   (   IDs=[]
                ->  format(user_error,'::Query ~w~n',[QueryNames]),
                    solutions(ID,(member(QueryName,QueryNames),matching_class(ID,QueryName,Opts)),MatchingIDs)
                ;   MatchingIDs=IDs),
                forall(member(ID,MatchingIDs),
                       write_class(OutFmt,ID,Opts)),
                (   OutFmt=obo
                ->  solutions(R,(member(ID,MatchingIDs),restriction(ID,R,_),property(R)),Rs),
                    forall(member(R,Rs),
                           write_property(OutFmt,R))
                ;   true)
            ))).

ontol_run_query(OntolQuery,ID,OutFmt,Opts) :-
        debug(blip,'Query=~w',OntolQuery),
        solutions(ID,OntolQuery,QIDs),
        forall(member(ID,QIDs),
               (   class(ID)
               ->  write_class(OutFmt,ID,Opts)
               ;   inst(ID)
               ->  write_instance(OutFmt,ID,Opts)
               ;   true)),

        (   memberchk(showobsoletes(1),Opts)
        ->  forall((member(ID,QIDs),obsolete_class(ID,_)),
                   write_class(OutFmt,ID,Opts))
        ;   true),

        % write all referenced properties (if obof)
        (   OutFmt=obo
        ->  solutions(R,(member(ID,QIDs),parent(ID,R1,_),property(R1),subclassRT(R1,R)),Rs),
            forall(member(R,Rs),
                   write_property(OutFmt,R))
        ;   true).

ontol_query_from_tokens_file(File,OutFmt,Opts):-
        format(user_error,'Reading File: ~w~n',[File]),
        open(File,read,IO),
        repeat,
        read_line_to_codes(IO,Codes),
        (   Codes=end_of_file
        ->  !
        ;   atom_codes(Query,Codes),
            format(user_error,'! Query :: "~w" ~n',[Query]),
            (   setof(ID,matching_class(ID,Query,Opts),IDs)
            ->  forall(member(ID,IDs),
                       %shownode('',ID,Opts))
                       write_class(OutFmt,ID,Opts))
            ;   format(user_error,'! Not_found: ~w~n',[Query])),
            fail).
        
% todo - only jump one step for non-transitive
blipkit:trusted_command('ontol-subset').
user:opt_insecure(query).
:- blip('ontol-subset',
        'exports a subset of an ontology - as indented text or other formats (formatted, flat, dot, obo, pro)',
        [atoms([id],QueryIDsIn),
         atom([ids,idlist],QueryIDsAtom),
         atom([idfile],QueryIDsFile),
         atoms([name,n],QueryNames),
         atom([to,t],OutFmt,formatted),
         atom([tabchar],TabChar,' '),
         number(down,MaxDown,0),
         number(up,MaxUp),
         bool([sib,sibs,siblings],Sibs),
         atom(query,OntolQueryAtom),
         atom(collapse_pred,CollapsePred,fail),
         atom([showannots,showannot],ShowAnnot,no),
         bool([stem],Stem),
         bool(reverse,_IsReverse), % todo
         bool(inverse,IsInverse),
         bool(showsyns,WithSynonyms),
         bool([instances,showinstances],WithInstances),
         bool(showcomments,WithComments),
         bool(showdefs,ShowDefs),
         %bool(cluster_by_ontology,IsClusterByOntology),
         atoms([cr,containment_relation],ContainmentRelations),
         bool(showxp,ShowXP),
         bool(showxrefs,ShowXrefs),
         bool(showinvxrefs,ShowInvXrefs),
         bool(showsubsets,ShowSubset),
         atom(rankdir,RankDir,'BT'), % for dot/graphviz: TB LR BT RL
         atoms(subset,Subsets),
         atoms(constraint,Constraints),
         atoms([relation,rel],Rels),
         atoms([exclude_relation,xrel],ExcRels),
         atoms([showrel],ShowRels)
        ],
        FileL,
        (   load_factfiles(FileL),
            ensure_nonvar(WithSynonyms,0 ),
            ensure_nonvar(WithComments,0 ),
            ensure_nonvar(ShowDefs,0 ),
            ensure_nonvar(ShowXP,0 ),
            ensure_nonvar(ShowSubset,0 ),
            ensure_nonvar(IsInverse,0 ),
            ensure_nonvar(WithInstances,0 ),
            Opts1=[stem(Stem),
                   maxdown(MaxDown),
                   maxup(MaxUp),
                   tabchar(TabChar),
                   siblings(Sibs),
                   relations(Rels),
                   exclude_relations(ExcRels),
		   rankdir(RankDir),
                   cluster_pred(belongs(X,Ontol),X,Ontol),
                   containment_relations(ContainmentRelations),
                   with_synonyms(WithSynonyms),
                   showcomments(WithComments),
                   showinstances(WithInstances),
                   showdefs(ShowDefs),
                   showxp(ShowXP),
                   showxrefs(ShowXrefs),
		   showinvxrefs(ShowInvXrefs),
                   showsubsets(ShowSubset),
                   subsets(Subsets),
                   showannots(ShowAnnot),
                   showrels(ShowRels)],
            (   Constraints=[]
            ->  true
            ;   ensure_loaded(bio(ontol_vizlayout)),
                forall(member(ConstraintF,Constraints),
                       load_viz_constraints(bio(ConstraintF)))),

            (   nonvar(QueryIDsAtom)
            ->  concat_atom(QueryIDs,' ',QueryIDsAtom)
            ;   nonvar(QueryIDsFile)
            ->  read_file_to_tokens(QueryIDsFile,QueryIDs)
            ;   QueryIDs=QueryIDsIn),

            % find all classes matching search criterion
            solutions(QueryID,(   (   nonvar(OntolQueryAtom)
                                  ->  atom_to_term(OntolQueryAtom,OntolQuery,Bindings),
                                      member('ID'=QueryID,Bindings),
                                      OntolQuery
                                  ;   member(QueryID,QueryIDs)
                                  ;   member(QueryName,QueryNames),
                                      matching_class(QueryID,QueryName,Opts1))),
                      MatchIDs),

            solutions(ID-N,(member(ID,MatchIDs),entity_label(ID,N)),IDNs),
            format(user_error,'::Result ~w ~w {over ~w}~n',[MatchIDs,IDNs,Rels]),
            Opts=[focus(MatchIDs)|Opts1], % TODO
            ontology_segment(MatchIDs,Edges,OutNodes,[collapse_predicate(CollapsePred)|Opts]),
            debug(blipkit,'Edges=~w',[Edges]),
            debug(blipkit,'Nodes=~w',[OutNodes]),
            show_ontol_subset_by_edges(OutFmt,Edges,Opts))).

% tree_node_ids(+N,?IDSet) is semidet
% IDSet has no dupes
% N=node(_,ID,SubNodes)
tree_node_ids(N,IDs):-
        tree_node_id_list(N,IDs1),
        list_to_set(IDs1,IDs).

% tree_node_ids(+N,?IDList)t
% IDList may have dupes
tree_node_id_list(node(_,ID,Nodes),[ID|IDs]):-
        findall(SubIDs,(member(Node,Nodes),tree_node_id_list(Node,SubIDs)),SubIDsL),
        flatten(SubIDsL,IDs).

tree_node_ids_with_dist(node(_,ID,Nodes),[D-ID|IDs],D):-
        D2 is D+1,
        findall(SubIDs,(member(Node,Nodes),tree_node_ids_with_dist(Node,SubIDs,D2)),SubIDsL),
        flatten(SubIDsL,IDs).
        
%% show_ontol_subset_by_edges(+Fmt,+Edges,+Opts) is det
show_ontol_subset_by_edges(dot,Edges,Opts):-
        !,
        ensure_loaded(bio(ontol_writer_dot)),
        edges_to_dot(Edges,Dot,Opts),
        writeln(Dot).

show_ontol_subset_by_edges(png,Edges,Opts):-
        !,
        ensure_loaded(bio(ontol_writer_dot)),
        write_edges_to_image(Edges,Opts).

show_ontol_subset_by_edges(display,Edges,Opts):-
        !,
        ensure_loaded(bio(ontol_writer_dot)),
        edges_to_display(Edges,Opts).

show_ontol_subset_by_edges(grapher,Edges,Opts):-
        !,
        ensure_loaded(library(pce_grapher)),
        ensure_loaded(bio(ontol_writer_grapher)),
        edges_to_grapher(Edges,Terms,Opts),
        writeln(Terms),
        grapher(Terms),
        prolog.

show_ontol_subset_by_edges(jsontree,Edges,Opts):-
        !,
        ensure_loaded(bio(ontol_writer_jsontree)),
        edges_to_jsontree_atom(Edges,A,Opts),
        writeln(A).

show_ontol_subset_by_edges(svg,Edges,_Opts):-
        !,
        ensure_loaded(bio(ontol_vizlayout)),
        ensure_loaded(library(sgml_write)),
        graph_to_semantic_svg_xml(Edges,XMLNode),
        sgml_write(XMLNode,[]).

show_ontol_subset_by_edges(xptable,Edges,_Opts):-
        !,
        solutions(ID,(member(edge(ID,_,_),Edges),\+member(edge(_,ID,_),Edges)),IDs), % leafs
        solutions(DC,(member(ID,IDs),
                      differentium(ID,_,DC)),
                  DCs),
        solutions(G,(member(ID,IDs),
                      genus(ID,G)),
                  Gs),
        writetab,
        %Hdr=..[''|Gs],
        Hdr=..Gs,
        WOpts = [isLabel(1)],
        blipkit:show_factrow(WOpts,Hdr),
        %writecols([''|Gs]),
        %nl,
        forall(member(DC,DCs),
               (   blipkit:show_term(WOpts,DC),
                   writetab,
                   forall(member(G,Gs),
                          (   forall(genus_differentium_match(G,R,DC,XP,[onlyPairwise(0)]),
                                     (   blipkit:show_term(WOpts,XP),
                                         format('<sub>[[~w]]</sub>\\n',[R])
                                     )),
                              writetab)),
                   nl)),
        nl.

                  

show_ontol_subset_by_edges(obo,Edges,Opts):-
        !,
        ensure_loaded(bio(ontol_writer_obo)),
        write_header(obo),
        edgelist_to_trees(Edges,Trees),
        debug(blipkit,'Trees=~w',[Trees]),
        show_ontol_subset_by_trees(obo,Trees,Opts).

% show multiple trees
show_ontol_subset_by_edges(OutFmt,Edges,Opts):-
        edgelist_to_trees(Edges,Trees),
        debug(blipkit,'Trees=~w',[Trees]),
        forall(member(Tree,Trees),
               show_ontol_subset_by_tree(OutFmt,Tree,Opts)).

% better to have onlyPairwise(0) or there are confusing omissions; eg nuclear membrane
genus_differentium_match(G,R,DC,XP,Opts):-
        genus(XP,G),
        differentium(XP,R,DC),
        (   member(onlyPairwise(1),Opts)
        ->  \+ ((differentium(XP,_,DC2),DC2\=DC))
        ;   true).


show_ontol_subset_by_trees(obo,Trees,_Opts):-
        !,
        ensure_loaded(bio(ontol_writer_obo)),
        debug(blipkit,'Converting trees to IDs: ~w',[Trees]),
        solutions(ID,
                  (   member(Tree,Trees),
                      tree_node_ids(Tree,IDs1),
                      member(ID,IDs1)),
                  IDs),
        debug(blipkit,'IDs from tree to write: ~w',[IDs]),
        forall_distinct((member(ID,IDs),class(ID)),
                        (   write_class(obo,ID)
                        ->  true
                        ;   format(user_error,'Cannot write obo class: ~w~n',[ID]))),
        forall_distinct((member(ID,IDs),inst(ID)),
                        (   write_inst(obo,ID)
                        ->  true
                        ;   format(user_error,'Cannot write obo inst: ~w~n',[ID]))),
        forall_distinct((member(ID,IDs),property(ID)),
                        (   write_property(obo,ID)
                        ->  true
                        ;   format(user_error,'Cannot write obo inst: ~w~n',[ID]))),
        % assumes curation_db
        solutions(Annot,(member(ID,IDs),curation_statement(Annot,ID,_,_)),Annots),
        ensure_loaded(bio(curation_writer_obo)),
        %forall_distinct(member(ID,Annots),
        %                (   write_inst(obo,ID)
        %                ->  true
        %                ;   format(user_error,'Cannot write obo inst: ~w~n',[ID]))),
        forall_distinct(member(ID,Annots),
                        (   write_curation(curation_obo,ID)
                        ->  true
                        ;   format(user_error,'Cannot write obo inst: ~w~n',[ID]))),
        solutions(R,(member(ID,IDs),node_link(ID,R,_),property(R),R\=is_a,R\=subclass),Rs),
        debug(blipkit,'Relations to write: ~w',[Rs]),
        forall(member(R,Rs),
               (   write_property(obo,R)
               ->  true
               ;   format(user_error,'Cannot write obo property: ~w~n',[R]))).
                 
show_ontol_subset_by_tree(formatted,Tree,Opts):-
        !,
        show_dagtree(root,Tree,0,Opts).

show_ontol_subset_by_tree(flat,Tree,Opts):-
        !,
        tree_node_ids_with_dist(Tree,Pairs,0),
        sort(Pairs,SPairs),
        forall(member(D-ID,SPairs),
               shownode(dist(D),ID,Opts)).
show_ontol_subset_by_tree(pro,Tree,_Opts):-
        !,
        tree_node_ids(Tree,IDs),
        ensure_loaded(bio(dbmeta)),
        ensure_loaded(bio(curation_db)),
        forall(member(ID,IDs),
               (   write_dbsubset_facts(_,ID,curation_db),
                   write_dbsubset_facts(_,ID,ontol_db),
                   write_dbsubset_facts(_,ID,metadata_db))).
show_ontol_subset_by_tree(owl,_,_Opts):-
        throw(error(not_implemented)).

:- blip('mireot-by-annotations',
        'extracts mireoted subset based on annotations',
        [atoms([gaf],GAFs),
         bool(remove_dangling,IsRD)],
        _,
        (   
            ensure_loaded(bio(curation_db)),
            ensure_loaded(bio(ontol_management)),
            maplist(load_biofile(go_assoc),GAFs),
            solutions(X,(   curation_statement(_,_,_,X)
                        ;   curation_subject_property_value(_,_,_,X)),Xs),
            solutions(ID,(member(X,Xs),
                          bf_parentRT(X,ID)),
                      IDs),
            (   IsRD=1
            ->  solutions(C,(class(C),\+member(C,IDs)),Cs),
                maplist(delete_class,Cs),
                remove_dangling_facts
            ;   true),
            forall(member(ID,IDs),
                   write_class(obo,ID,[])),
            solutions(P,property(P),Ps),
            forall(member(P,Ps),
                   write_property(obo,P)))).

:- blip('ontol-collapse-to-slim',
        'collapses ontology into a subset/slim',
        [bool(add_root,IsAddRoot),
         atom([subset,slim],Slim),
         atom([to,t],ToFormat,obo),
         atom([o,output],OutFile)],
        [],
        (   
            ensure_loaded(bio(ontol_management)),
            (   IsAddRoot=1
            ->  forall(noparent(X),
                       assert(entity_partition(X,Slim)))
            ;   true),
            extract_slim(Slim),
            write_biofile(ToFormat,OutFile))).

:- blip('ontol-reasoner',
        'reasons over ontology via forward-chaining, finding full deductive closure',
        [atom([to,t],ToFormat),
         atoms(explain,ExplainRules),
         atom(explain_format,ExplainFormat,text),
         bool(explain_all,ExplainAll),
         bool(skip,Skip),
         atoms(show_idspace,ShowIDSpaces),
         bool(show_redundant,ShowRedundant),
         bool(remove_redundant,RemoveRedundant),
         bool(show_unsatisfiable,ShowUnsatisfiable),
         bool(show_abduced,ShowAbduced),
         atom(explain_file,ExplainFile),
         atom([o,output],OutFile)],
        FileL,
        (   
            maplist(load_biofile,FileL),
            ensure_loaded(bio(ontol_reasoner)),

            (   Skip=0
            ->  find_all_entailments
            ;   true),
            (   RemoveRedundant=1
            ->  retractall_redundant
            ;   true),
            (   var(ExplainFile) % this is a bit hacky but it will do for now...
            ->  true
            ;   tell(ExplainFile)),
            (   ExplainAll=1
            ->  show_all_explanations(ExplainFormat)
            ;   true),
            (   ShowRedundant=1
            ->  show_redundant_links
            ;   true),
            (   ShowAbduced=1
            ->  show_abduced_links
            ;   true),
            (   ShowUnsatisfiable=1
            ->  show_unsatisfiable
            ;   true),
            (   ShowIDSpaces=[]
            ->  true
            ;   forall((subclass(A,B),
                        id_idspace(A,AS),
                        member(AS,ShowIDSpaces),
                        id_idspace(B,BS),
                        member(BS,ShowIDSpaces)),
                       format('~q~n.',[subclass(A,B)]))),
            maplist(show_all_explanations(ExplainFormat,true),ExplainRules),
            (   var(ExplainFile)
            ->  true
            ;   told),
            (   var(ToFormat)
            ->  true
            ;   write_biofile(ToFormat,OutFile)))).
show_redundant_links:-
        forall((asserted_fact(G),entailed_by(G,Rule)),
               show_explanation(G,Rule)).

show_abduced_links:-
        forall((Goal=subclass(X,_Y),genus(X,_),asserted_fact(Goal),\+entailed_by(Goal,_)),
               show_explanation(Goal,_Rule)).

show_unsatisfiable:-
        forall((Goal=is_unsatisfiable(_),Goal),
               show_explanation(text,Goal,_Rule)).

% must be pre-reasoned
go:go_rule(A1,A) :-
        ontol_db:parent(A1,A).
go:go_rule(A1,A) :-
        ontol_db:restriction(A1,regulates,A2),
        ontol_db:parent(A2,A).

:- blip('ontol-enrichment',
        'performs class enrichment',
        [atom(idfile,IDFile),
         term(index_goal,IndexGoal),
         atom(cache_file,CacheFile,''),
         atoms(ontology,OntFiles),
         atoms(gaf,GafFiles),
         bool(pre_reasoned,IsPreReasoned),
         number([num_genes,num_entities],NumGenes),
         number(max_p,MaxP,0.01)],
        InIDs,
        (   
            ensure_loaded(bio(enrichment)),
            ensure_loaded(bio(curation_db)),
            ensure_loaded(bio(tabling)),
            maplist(load_biofile(obo),OntFiles),
            maplist(load_biofile(go_assoc),GafFiles),
            table_pred(ontol_db:parentT/2),
            debug(enrichment,'indexing... cachefile: ~w',[CacheFile]),
            (   var(IndexGoal)
            ->  (   IsPreReasoned=1
                ->  index_goal((curation_db:curation_statement(_,I,_,A1),go:go_rule(A1,A))-I-A,
                               (go:go_rule(AX,AY))-AX-AY,
                               CacheFile)
                ;   index_goal((curation_db:curation_statementT(_,I,_,A1))-I-A1,
                               (ontol_db:parentT(AX,AY))-AX-AY,
                               CacheFile))
            ;   IndexGoal=G-SG,
                index_goal(G,SG,CacheFile)),
            (   nonvar(IDFile)
            ->  load_biofile(tbl(id),IDFile),
                setof(ID,id(ID),IDs)
            ;   IDs=InIDs),
            %findall(X,(id(ID),lookup_feature(ID,X)),Xs),
            lookup_features(IDs,Xs,NotFoundL),
            (   NotFoundL=[]
            ->  writeln('# all input ids found')
            ;   format('# not found: ~w~n',[NotFoundL])),
            length(Xs,NumXs),
            format('# number found: ~w~n',[NumXs]),
            forall(member(X,Xs),
                   format('#    input: ~w~n',[X])),
            itemset_attribute_enrichment(Xs,Stats,NumGenes),
            format('# max P: ~w~n',[MaxP]),
            forall((member(P-C-Stat,Stats),
                    P<MaxP),
                   show_factrow([isLabel(1)],hit(P,C,Stat))))).
        


:- blip('curation-fmatch-simj',
        'compares two features using Jaccard similarity over annotations',
        [],
        Features,
        (   

            ensure_loaded(bio(simmatrix)),
            generate_term_indexes(F,A,
                                  (   member(F,Features),
                                      curation_statementT(_,F,_,A,_))),
            forall((member(F1,Features),member(F2,Features),F1@<F2),
                   (   feature_pair_simj(F1,F2,SimJ),
                       format('~w ~w :: ~w~n',[F1,F2,SimJ]))))).

blipkit:example('blip -r go -r go_assoc_local/mgi curation-fmatch MGI:MGI:98371 MGI:MGI:98358',
                'finds similarity between two genes').
:- blip('curation-fmatch',
        'compares a feature against a set of features or performs a search',
        [],
        Features,
        (   
            ensure_loaded(bio(simmatrix)),
            debug(fmatch,'indexing inferred annots',[]),
            forall(curation_statementT(_,F,_,A,_),
                   assert(simmatrix:fm(F,A))),
            debug(fmatch,'indexing direct annots',[]),
            forall(curation_statement(_,F,_,A),
                   assert(simmatrix:feature_attribute_direct(F,A))),
            debug(fmatch,'indexing subsumption hierarchy',[]),
            forall(parentT(A,B),
                   assert(simmatrix:attribute_subsumer(A,B))),
            debug(fmatch,'compiling db',[]),
            simmatrix:compile_predicates([simmatrix:fm/2,
                                          simmatrix:feature_attribute_direct/2,
                                          simmatrix:attribute_subsumer/2]),
            debug(fmatch,'generating indexes',[]),
            generate_term_indexes(F,A,simmatrix:fm(F,A)),
            debug(fmatch,'finding matches for ~w',[Features]),
            Opts=[], % add scores here
            (   Features=[F1]
            ->  feature_matches(F1,ScoreList,[metric(simj)]), % search
                findall(Match,member(_-Match,ScoreList),MatchList)
            ;   Features=[F1|MatchList]), % match against set
            forall(member(F2,MatchList),
                   compare_feature_pair(F1,F2,Opts)))).

:- blip('sim',
        'uses simmatrix.pro to find similarities between features. relies on ready-made fm/2 predicate',
        [],
        _FileL,
        (   (   ensure_loaded(bio(simmatrix)),
                debug(sim,'generating term indexes',[]),
                generate_term_indexes(F,A,fm(F,A)),
                simmatrix:feature_ix(X,_),
                debug(sim,'feature: ~w',[X]),
                (   entity_label(X,XN)
                ->  true
                ;   XN='?'),
                Scores=[simTO(_),simj(_),simGIC(_),avgICCSnr(_,_),maxICnr(_,_)],
                format('#Query: ~w ~w~n',[X,XN]),
                feature_matches(X,ScoreList,[metric(simj),limit(500)]),
                findall(Match,member(_-Match,ScoreList),MatchList),
                forall((member(Y,MatchList),X\=Y),
                       compare_feature_pair(X,Y,Scores)),
                fail)
        ;   true)).

compare_feature_pair(X,Y,Scores) :-
        debug(sim,'comparing: ~w ~w using: ~w',[X,Y,Scores]),
        feature_pair_all_scores(X,Y,Scores),
        debug(sim,'feature pair: ~w ~w scores: ~w',[X,Y,Scores]),
        (   entity_label(X,XN) -> true ; XN='?'),
        (   entity_label(Y,YN) -> true ; YN='?'),
        format('~w\t~w\t~w\t~w',[X,XN,Y,YN]),
        forall(member(Score,Scores),
               show_score_cols(Score)),
        nl.

show_score_cols(maxIC(MaxIC,L)) :-
        !,
        format('\tmaxIC\t\~w\t',[MaxIC]),
        maplist(labelify,L,L2),
        write(L2).
show_score_cols(maxICnr(MaxIC,L)) :-
        !,
        format('\tmaxICnr\t\~w\t',[MaxIC]),
        maplist(labelify,L,L2),
        write(L2).
show_score_cols(avgICCSnr(MaxIC,L)) :-
        !,
        format('\tavgICCSnr\t\~w\t',[MaxIC]),
        findall(S2,
                (   member(S,L),
                    maplist(labelify,S,S2)),
                L2),
        write(L2).
show_score_cols(Score) :-
        Score=..STL,
        forall(member(ST,STL),
               format('\t~w',[ST])).

labelify(X,A) :-
        entity_label(X,N),
        !,
        sformat(A,'~w "~w"',[X,N]).
labelify(X,X).

:- blip('ontol-q',
        'ontology querying using oborel_ql',
        [],
        _,
        (   user:use_module(bio(oborel_ql)),
            prolog)).

:- blip('ontol-info-classes',
        'lists classes in an ontology',
        [],
        FileL,
        (   load_factfiles(FileL),
            ontol_info_classes)).

:- blip('ontol-write-emacs-lookup',
        'lookup table for emacs-list',
        [],
        _,
        (   format('(setq obo-lookup-table ''('),
            forall(class_label(ID,N,_),
                   format('("~w" . "~w")~n',[ID,N])),
            format('))'))).

% blip -i http://obo.cvs.sourceforge.net/viewvc/*checkout*/obo/obo/website/cgi-bin/ontologies.txt -f 'tagval(obo_metadata)' ontol-generate-rdf-register-ns
:- blip('ontol-generate-rdf-register-ns',
	'bioconf_rdf',
	[],
	_,
	(   
	    forall((obo_metadata:namespace(ID,NS),
                    NS\='',
		    \+ obo_metadata:uriprefix(ID,_),
                    sformat(URI,'http://purl.org/obo/owl/~w#~w_',[NS,NS])),
		   format(':- rdf_register_ns(~q,~q).~n',[NS,URI])),
	    forall((obo_metadata:namespace(ID,NS),
		    obo_metadata:uriprefix(ID,URI)),
		   format(':- rdf_register_ns(~q,~q).~n',[NS,URI])))).

blipkit:example('blip -i ~/ontologies/biopax-level2.owl ontol-schema  -local biopax2 -ns "http://www.biopax.org/release/biopax-level2.owl#"',
                'generates a prolog/datalog schema from a knowledge model').
:- blip('ontol-schema',
        'generates a schema from an OWL model. E.g. -local biopax2 -ns "http://www.biopax.org/release/biopax-level2.owl#"',
        [atom(ns,NS),
         atom(local,Local),
         atom(prefix,Prefix,Local),
         bool(prolog_properties,FixProps)],
        _,
        (   nonvar(NS),
            nonvar(Local),
            ensure_loaded(bio(ontol_bridge_to_schema)),
            write_schema(Local,NS,[prefix(Prefix),prolog_properties(FixProps)]))).

:- blip('ontol-serql-server',
        'starts serql server',
        [atom(entailment,_Entailment,rdfs),
         atom(lang,_Lang,serql)],
        _Qs,
        (   
            %prolog_load_context(directory, Dir),
            asserta(user:file_search_path(library, serql(lib))),
            use_module(library(settings)),
            %ensure_loaded(serql(serql)),
            %ensure_loaded(serql(sparql)),
            load_files([ serql(load)
                       ],
                       [ silent(false)
                       ]),
            serql_server,
            prolog)).

:- blip('dbpedia',
        'query DBPredia endpoints',
        [bool(write_prolog,IsProlog)],
        [Q],
        (   ensure_loaded(bio(sparql_util)),
            forall(dbpedia_query_links(Q,Row,1000,[]),
		   show_factrow([isProlog(IsProlog)],Row)))).

blipkit:example('blip ontol-sparql-remote "SELECT * WHERE {  ?x rdf:type <http://dbpedia.org/ontology/AnatomicalStructure> }"',
		'remote sparql query (defaults to dbpedia)').
blipkit:trusted_command('ontol-sparql-remote').
:- blip('ontol-sparql-remote',
        'ontology querying using iterative Sparql',
        [atom(host,Host,'dbpedia.org'),
	 atom(path,Path,'/sparql/'),
	 atom(port,Port,80),
	 number(limit,Limit,10000),
	 number(offset,Offset,1000),
	 bool(write_prolog,IsProlog)],
        [Q],
        (   ensure_loaded(semweb(sparql_client)),
	    %ensure_loaded(serql(no_entailment)),
	    ensure_loaded(bio(sparql_util)),
	    sparql_set_server([host(Host),port(Port),path(Path)]),
	    forall(iterative_sparql_query(Q,Row,Offset,[limit(Limit)]),
		   show_factrow([isProlog(IsProlog)],Row)))).

blipkit:example('blip -r obop/biological_process ontol-serql "SELECT * FROM {X} rdfs:label {L} WHERE label(L)=\'apoptosis\'"',
                'query ontology using SeRQL').
blipkit:trusted_command('ontol-serql').
:- blip('ontol-serql',
        'ontology querying using SeRQL',
        [atom(entailment,Entailment,rdfs),
         atom(lang,Lang,serql)],
        Qs,
        (   ensure_loaded(serql(serql)),
            ensure_loaded(serql(sparql)),
            use_module(serql(rdf_entailment),[]),
            use_module(serql(rdfs_entailment),[]),
            use_module(serql(rdfslite_entailment),[]),
            ensure_loaded(bio(ontol_bridge_to_serql)),
            rdf_init_db(rdfs),
            %owl_assertall,
            (   Qs=[]
            ->  iterate_over_input('Enter SeRQL: ',
                                   Q,
                                   safe_serql(Entailment,Q))
            ;   maplist(safe_serql(Lang,Entailment),Qs)))).

safe_serql(serql,Entailment,Q):-
        format('QUERY: ~w~n',[Q]),
        forall(catch(serql_query(Q,R,[entailment(Entailment)]),
                     E,
                     format(user_error,'Caught: ~w~n',[E])),
               writeln(R)).        
safe_serql(sparql,Entailment,Q):-
        format('QUERY: ~w~n',[Q]),
        forall(catch(sparql_query(Q,R,[entailment(Entailment)]),
                     E,
                     format(user_error,'Caught: ~w~n',[E])),
               writeln(R)).        

rdf_init_db(Type) :-
        ensure_loaded(semweb(rdf_db)),
	%rdf_reset_db,
	absolute_file_name(serql(Type),
			   [ access(read),
			     extensions([rdfs,rdf,owl])
			   ],
			   RDFS),
	rdf_load(RDFS).

ontol_info_classes:-
        solutions(parent_over_nr(R)-R,is_transitive(R),RRNs),
        forall(class(ID),
               ontol_info_class(ID,[subclass-is_a,parent-all|RRNs])).

ontol_info_class(ID,RRNs):-
        forall(member(R-RN,RRNs),
               ontol_info_class(ID,R,RN)).

ontol_info_class(ID,R,RN):-
        closure_to_edgelist(R,ID,Edges),
        forall(dag_path_to_root(Edges,ID,_,P),
               (   length(P,D),
                   writecols([dist_to_root,ID,RN,D]),nl)).

%??
:- blip('ontol-thea',
        'THEA wrapper',
        [atom(entailment,Entailment,rdfs),
         atom(lang,Lang,serql)],
        Qs,
        (   ensure_loaded(ontol(theawrapper)),
            rdf_init_db(rdfs),
            %owl_assertall,
            (   Qs=[]
            ->  iterate_over_input('Enter SeRQL: ',
                                   Q,
                                   safe_serql(Entailment,Q))
            ;   maplist(safe_serql(Lang,Entailment),Qs)))).

:- blip('ontol-owl-to-thea',
        'writes thea prolog from owl',
        [atom([output,o],Out),
         term(unfold,Unfolds,fail),
	 term([imports],Imports)],
        Files,
        (   ensure_loaded(bio(thea_wrapper)),
            (   Unfolds
            ->  unfolds_owl_to_thea(Files,[])
            ;   forall(member(File,Files),
                       thea_parse(File,[imports(Imports)])),
                write_biofile(thea,Out)))).

unfolds_owl_to_thea([],_).
unfolds_owl_to_thea([File|Files],Parsed):-
        unfolds_owl_to_thea(File,NewFiles,Parsed),
        append(NewFiles,Files,NextFiles),
        unfolds_owl_to_thea(NextFiles,[File|Parsed]).

unfolds_owl_to_thea(File,[],Parsed):-
        member(File,Parsed),
        debug(ontol,'already asserted ~w',[File]),
        !.
unfolds_owl_to_thea(File,URLs,_Parsed):-
        thea_parse(File,[clear(complete),imports(false)]),
        url_cachepath(File,CachePath),
        atom_concat(CachePath,'.thea',Out),
        debug(ontol,'writing to ~w',[Out]),
        write_biofile(thea,Out),
        findall(URL,rdf(_,'http://www.w3.org/2002/07/owl#imports',URL),URLs),
        debug(ontol,'  Imports: ~w',[URLs]).
        %unfolds_owl_to_thea(URLs,[File|Parsed]).

blipkit:example('blip-ddb -debug load -r obo_meta ontol-refresh-cache -set_data_cache /local/blip_cache',
                'refresh contents of cache used by ontol_restful').
:- blip('ontol-refresh-cache',
        'refresh contents of blip data_cache using ontology metadata',
        [atoms([extra],Extras),
         number(age_threshold,MaxAge,3600)],
        _,
        (   solutions(Ont,
                      (   inst_sv(X,namespace,Ont,_),
                          \+ inst_sv(X,is_obsolete,_,_)),
                      Onts),
            assert(user:max_cached_file_age_seconds(MaxAge)),
            forall(member(Extra,Extras),
                   refresh_ont(Extra)),
            forall(member(Ont,Onts),
                   refresh_ont(Ont)))).

refresh_ont(Ont) :-
        ensure_loaded(bio(ontol_restful)),
        ensure_loaded(bio(dbmeta)),
        idspace_url_format(Ont,URL,Fmt),
        catch(load_biofile(Fmt,url(URL)),
              _,
              true),
        (   dynamic_db(ontol_db)
        ->  delete_all_facts(ontol_db)
        ;   true),
        (   dynamic_db(metadata_db)
        ->  delete_all_facts(metadata_db)
        ;   true),
        !.
refresh_ont(_,_).




blipkit:trusted_command('ontol-rest').
:- blip('ontol-rest',
        'RESTful interface',
        [atoms([path],Paths1),
         atoms(param,ParamAtoms)],
        Paths2,
        (   append(Paths1,Paths2,Paths),
            load_bioresource(obo_meta),
            load_bioresource(xrf),
            ensure_loaded(bio(ontol_restful)),
            maplist(eqatom_param,ParamAtoms,Params),
            debug(ontol_rest,' Params=~w',[Params]),
            current_prolog_flag(encoding,Enc),
            debug(ontol_rest,' enc=~w',[Enc]),
            %set_prolog_flag(encoding,utf8),
            set_prolog_flag(encoding,text),
            forall(member(Path,Paths),
                   ontol_page(Path,Params)))).

eqatom_param(A,P=V):-    concat_atom([P,V],'=',A),!.
eqatom_param(A,P=V):-    concat_atom([P|L],'=',A),!,concat_atom(L,'=',V).
eqatom_param(A,A).

/*
:- blip('gaf-filter',
        'GAFs',
        [atom(taxon,Taxon)]
       Files,
        (   (   nonvar(Taxon)
            ->  load_bioresources(taxonomy)
            ;   true),
	    ensure_loaded(bio(process_streams)),
	    forall(member(File,Files),
		   process_file_lines(Rule,File)))).
*/            
       


/** <module> simple interface to blip module functionality
  @author Chris Mungall
  @version  $Revision: 1.29 $
  @date  $Date: 2006/02/21 01:52:17 $
  @license LGPL

  ---+ Synopsis

  ==
  blip -u blipkit_ontol -help
  ==

  ---+ Description

  this is a submodule of blipkit for handling ontology related data*/
