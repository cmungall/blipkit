/* -*- Mode: Prolog -*- */


:- module(blipkit_ontol_metadata,[]).

:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_lookup)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_text)).
:- use_module(bio(ontol_writer_obo)).
:- use_module(bio(graph)).
:- use_module(bio(io)).
:- use_module(bio(annotator)).

:- blip('annotate',
        'NER annotation',
        [atom([to],Fmt),
         atoms([x,exclude],Excludes)],
        Files,
        (   forall(member(File,Files),
                   annotate_file(File,[to(Fmt),excludes(Excludes)])
                  ))).

:- blip('ontol-annotate',
        'NER annotation on ontology classes',
        [atom([to],Fmt)],
        _,
        (   annotate_ontology([to(Fmt)]))).



blipkit:example('blip -r obo_meta -r obo_meta_xp  $GO_XP_ARGS -r go -u blipkit_ontol_metadata ontol-xp-metadata -ont biological_process -ont cellular_component -ont molecular_function -ont cell -ont chebi -ont uberon -ont quality',
	       'draws a wikitable summarizing XPs for specified ontologies').
:- blip('ontol-xp-metadata',
        'draws metadata table',
        [atoms([ontology,ont],Onts)],
        _,
        (   mktable(Table,[onts(Onts)]),
            showtable(Table))).

p(molecular_function,bioprocess).
p(biological_process,bioprocess).
p(cellular_component,anatomic).
p(po_anatomy,anatomic).
p(cell,anatomic).
p(uberon,anatomic).
p(chebi,chemical).

p(anatomic,spatial).
p(chemical,spatial).
p(bioprocess,dynamic).

row_col_data(A,B,[href(Link,XP),' ',Count,'\n'],Opts):-
        extends(XP,A),
        (   member(onts(Onts),Opts)
        ->  member(A,Onts)
        ;   true),
        uses(XP,B),
        (   member(onts(Onts),Opts)
        ->  member(B,Onts)
        ;   true),
        setof_count(X,(genus(X,_),differentium(X,_,Y),belongs(X,A),belongs(Y,B)),Count),
        documentation(XP,Link).

mktable(Table,Opts):-
        findall(cell(A,B,Data),
%                row_col_data(B,A,Data,Opts),
                row_col_data(A,B,Data,Opts),
                Cells),
        Table=table(Cells,[]).

showtable(Table):-
        Table=table(Cells,[]),
        solutions(Row,member(cell(Row,_,_),Cells),Rows),
        solutions(Col,member(cell(_,Col,_),Cells),Cols),
        rhierarchy(Rows,RowRanks,OrderedRows),
        rhierarchy(Cols,ColRanks,OrderedCols),
        debug(ontol,'colranks: ~w',[ColRanks]),
        table(OrderedRows,OrderedCols,Cells,[colranks(ColRanks),rowranks(RowRanks)],Toks,[]),
        %table(OrderedRows,OrderedCols,Cells,[],Toks,[]),
        concat_atom(Toks,'',A),
        writeln(A).

rhierarchy(IDs,Ranks,OrderedIDs):-
        hierarchy(IDs,RanksRev,OrderedIDs),
        reverse(RanksRev,Ranks).

% bottom-up construction of hierarchy
hierarchy(IDs,Ranks,OrderedIDs):-
        findall(ID-1,member(ID,IDs),IDSizePairs),
        hierarchy(IDs,IDSizePairs,Ranks,OrderedIDs).
hierarchy(IDs,IDSizePairs,Ranks,OrderedIDs):-
        debug(ontol,'IDs=~w   ::: sizes=~w',[IDs,IDSizePairs]),
        length(IDs,_NumElts),
        (   group_by(PID,(member(ID,IDs),supercat(ID,PID)),ID,Groups),
            length(Groups,NumGroups),
            NumGroups > 1
        ->  findall(PID,member(PID-_,Groups),PIDs),
            findall(PID-Size,
                    (   member(PID-ChildIDs,Groups),
                        findall(ChildSize,(member(ChildID,ChildIDs),member(ChildID-ChildSize,IDSizePairs)),
                                ChildSizes),
                        debug(ontol,'   childSizes=~w for ~w < ~w',[ChildSizes,PID,ChildIDs]),
                        sumlist(ChildSizes,Size)),
                    PIDSizePairs),
            hierarchy(PIDs,PIDSizePairs,PRanks,OrderedPIDs),
            findall(ID,(  member(PID,OrderedPIDs),
                          member(PID-ChildIDs,Groups),
                          member(ID,ChildIDs)),
                    OrderedIDs),
            debug(ontol,'   ordered=~w',[OrderedIDs]),
            findall(PID-Size-ChildIDs,
                    (   member(PID,OrderedPIDs),
                        member(PID-ChildIDs,Groups),
                        member(PID-Size,PIDSizePairs)),
                    Rank),
            Ranks=[Rank|PRanks]
        ;   Ranks=[],           % no parents
            OrderedIDs=IDs).    % any order

supercat(ID,PID):- p(ID,PID).
supercat(ID,null):- \+ p(ID,_).


tree_width([],0).
tree_width([],0).

                    
                    

table(Rows,Cols,Cells,Opts) -->
        ['{|class="wikitable" style="text-align:center border="1"'],newline,
        ['|+ caption'],newline,
        colranks(Cols,Opts),
        ['|-'],
        newline,
        headercols([''|Cols]),
        rows(Rows,Cols,Cells),
        ['|}'].

colranks(_Cols,Opts) --> {member(colranks(ColRanks),Opts)},!,colranks(ColRanks).
colranks(_,_) --> [].

colranks([]) --> [].
colranks([Rank|Ranks]) --> colrank(Rank),colranks(Ranks).
colrank(Elts) -->
        ['|-'],newline,
        ['! '],newline,
        colrank_elts(Elts).
colrank_elts([]) --> [].
colrank_elts([H|T]) --> colrank_elt(H),colrank_elts(T).

colrank_elt(Name-Size-_) -->
        ['! colspan="',Size,'" | '],[Name],newline.


headercols([]) --> [].
headercols([H|L]) --> headercol(H),headercols(L).
headercol(H) --> ['! '],[H],newline.

        

rows([Row|Rows],Cols,Cells) -->
        row(Row,Cols,Cells),
        rows(Rows,Cols,Cells).
rows([],_,_) --> [].

row(Row,Cols,Cells) -->
        ['|-'],newline,
        ['! '],[Row],newline,
        colvals(Row,Cols,Cells),
        ['\n'].

colvals(_,[],_) --> [].
colvals(Row,[Col|Cols],Cells) -->
        colval(Row,Col,Cells),
        colvals(Row,Cols,Cells).


colval(Row,Col,Cells) -->
        ['| '],
        {findall(Elt,member(cell(Row,Col,Elt),Cells),Elts)},
        cell_elements(Elts),
        newline.

cell_elements([]) --> [].
cell_elements([Elt|Elts]) -->
        cell_element(Elt),
        cell_elements(Elts).

cell_element(href(URL,Label)) --> !,['[',URL,' ',Label,']'].
cell_element(L) --> {is_list(L)},cell_elements(L).
cell_element(Elt) --> [Elt].

newline --> ['\n'].

/** <module> writes tables from OBO metadata

  ---+ Synopsis

==
:- use_module(bio(blipkit_ontol_metadata)).

% 
demo:-
  nl.
  

==

---+ Details



@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
