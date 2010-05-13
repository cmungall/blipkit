/* -*- Mode: Prolog -*- */


:- module(parser_obo,
          [
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- [bio(parser_general)].

:- dynamic default_namespace/1.

% TODO - handle obsoletes

io:parse_stream(obo_native,IO):-
        read_blocks(IO).
io:parse_stream(obo,IO):-
        read_blocks(IO).
io:parse_stream_with_cache(obo_native,IO,File):-
        read_blocks_with_cache(IO,File).
io:parse_stream_with_cache(obo,IO,File):-
        read_blocks_with_cache(IO,File).

read_blocks_with_cache(IO,File):-
        tell(File),
        read_blocks(IO,on),
        told.

read_blocks(IO):-
        read_blocks(IO,off).
read_blocks(IO,CacheMode):-
        read_block(IO,HBlock),
        parse_block(header,HBlock,_),
        repeat,
        (   read_block(IO,Block)
        ->  parse_block(body,Block,Facts),
            forall(member(Fact,Facts),
                   assert(ontol_db:Fact)),
            (   CacheMode=on
            ->  forall(member(Fact,Facts),
                       format('~q.~n',[Fact]))
            ;   true),
            fail
        ;   true).

read_block(IO,Block):-
        \+ at_end_of_stream(IO),
        read_line_to_codes(IO,Block,Tail),
        read_block(IO,Block,Tail).

% (+IO,[+Block|?Tail],?Tail)
read_block(_IO,"\r\n",[]) :- !.
read_block(_IO,"\n",[]) :- !.
read_block(_IO,"\r",[]) :- !.
read_block(_IO,"",[]) :- !.
read_block(IO,_Block,Tail):-
        read_line_to_codes(IO,Tail,NewTail),
        read_block(IO,Tail,NewTail).

parse_block(header,Codes,[]):-
        obo_header(Info,Codes,[]),
        !,
        (   member('default-namespace'=NS,Info)
        ->  retractall(default_namespace(_)),
            assert(default_namespace(NS))
        ;   true).
parse_block(body,Codes1,Facts):-
        !,
        escape_comments(Codes1,Codes),
        stanza(ID^Type^Facts1,Codes,[]),
        (   member(belongs(_,_),Facts1)
        ->  Facts=Facts1
        ;   (   get_namespace(Type,NS)
            ->  Facts=[ontol_db:belongs(ID,NS)|Facts1]
            ;   throw(error(no_namespace(Facts))))),
        !.
parse_block(X,Codes,_):-
        throw(cannot_parser(X,Codes)).

parse_block(Codes):-
        atom_codes(A,Codes),
        throw(error(cannot_parse_stanza(A))).

get_namespace(_,NS):-
        default_namespace(NS),
        !.
get_namespace(property,relationship).

obo_header( Info ) --> header_tagvals(Info),newline.
header_tagvals( [TV|TVs] ) -->
        header_tagval(TV),
        !,
        header_tagvals(TVs).
header_tagvals( [] ) --> [].

header_tagval(T=V) --> token(T^[ws|"[]:"]),":",!,ws_star,basic_val(V),newline.

% -- grammar --
stanza( ID^Type^Facts ) -->
        stanza_head(Type),
        stanza_body(ID^Type^Facts).
stanza_head( Type ) -->
        "[",
        !,
        word(StanzaType),
        "]",
        newline,
        {stanza_type(StanzaType,Type)}.
        
stanza_body(ID^Type^Facts) -->
        tagval(Term),!,opt_comment,newline,stanza_body(ID^Type^Facts1),
        {tagval_term_to_fact(Term,ID,Type,Fact),
         merge_facts(Fact,Facts1,Facts)}.
stanza_body(_^_^[]) --> newlines,!.

tagval(id(ID)) --> "id:",!,ws_star,obo_id(ID).
tagval(name(N)) --> "name:",!,ws_star,basic_val(N).
tagval(belongs(N)) --> "namespace:",!,ws_star,basic_val(N).
tagval(restriction(P,To)) --> "relationship:",!,ws_star,obo_id(P),ws_star,obo_id(To).
tagval([def(N)|XTerms]) --> "def:",!,ws_star,quoted_val(N),ws_star,xrefset(Xs),{findall(def_xref(X),member(X,Xs),XTerms)}.
tagval(synonym(T,V)) --> "synonym:",ws_star,quoted_val(V),ws_plus,obo_word(Tuc),ws_plus,obo_word(_),xrefset(_),!,{downcase_atom(Tuc,T)}. % obo-1.2 style
tagval(synonym(exact,V)) --> "exact_synonym:",!,ws_star,quoted_val(V),ws_star,xrefset(_).
tagval(synonym(narrow,V)) --> "narrow_synonym:",!,ws_star,quoted_val(V),ws_plus,xrefset(_).
tagval(synonym(related,V)) --> "related_synonym:",!,ws_star,quoted_val(V),ws_plus,xrefset(_).
tagval(synonym(related,V)) --> "synonym:",!,ws_star,quoted_val(V),ws_plus,xrefset(_).
tagval(generic(T,V)) --> word(T),":",!,ws_star,basic_val(V).

basic_val(V) --> token(V^[nl|"!"]).
obo_id(ID) --> not_ws_atom(ID).
obo_word(X) --> not_ws_atom(X).

opt_quoted_val(V) --> quoted_val(V),!.
opt_quoted_val('') --> [].

quoted_val(V) --> """",!,token(V^""""),"""".

opt_comment --> ws_star,"!",!,not_newlines(_).
opt_comment --> [].

xrefset(Xs) --> "[",!,xrefs_star(Xs),"]".
xrefs_star(Xs) --> xrefs(Xs),!.
xrefs_star([]) --> [].
xrefs([X|Xs]) --> xref(X),",",!,ws_star,xrefs(Xs).
xrefs([X]) --> xref(X).
xref(X) --> token(X^[nl|",]"]),!,ws_star,opt_quoted_val(_).

lines([]) --> [].
lines([Line|Lines]) --> line(Line),lines(Lines).

line(Cs) --> not_newlines(Cs).
vals([V]) --> val(V).
vals([V|Vs]) --> val(V),vals(Vs).

val(V) --> not_ws(Codes),{atom_codes(V,Codes)}.

stanza_type('Term',class).
stanza_type('Typedef',property).


% tagval_term_to_fact(+Term,?ID,+Type,?Fact) d
% tagval_term_to_fact(+Terms,?ID,+Type,?Facts) d
tagval_term_to_fact([],_,_,[]):- !.
tagval_term_to_fact([T|TL],ID,Type,[F|FL]):-
        !,
        tagval_term_to_fact(T,ID,Type,F),
        tagval_term_to_fact(TL,ID,Type,FL).
tagval_term_to_fact(id(ID),ID,_,null):- !.
tagval_term_to_fact(name(N),ID,Type,Fact):-
        !,
        Fact =.. [Type,ID,N].
tagval_term_to_fact(generic(T,V),ID,_,Fact):-
        !,
        (   tag_pred(T,T2)
        ->  true
        ;   T2=T),
        Fact =.. [T2,ID,V].
tagval_term_to_fact(Term,ID,_,Fact):-
        !,
        Term =.. [Functor|Vals],
        Fact =.. [Functor,ID|Vals].

tag_pred(xref_analog,class_xref).
tag_pred(is_a,subclass).

merge_facts(null,L,L):- !.
merge_facts(L1,L2,L):-
        is_list(L1),
        !,
        append(L1,L2,L).
merge_facts(F,L,[F|L]):- !.

% pre-processing is necessary for obo files - removes anything
% between ! and end of line (retaining newline char)
%  also removes trailing whitespace
escape_comments(CL,CL2):-
        escape_comments(CL,[],CL2).
escape_comments([],_,[]).
escape_comments([C|CL],_,[C|CL2]):- % ditch trailing whitespace
        newline(C),
        !,
        escape_comments(CL,[],CL2).
escape_comments([C|CL],BL,CL2):- % retain whitespace in buffer
        ws(C),
        !,
        escape_comments(CL,[C|BL],CL2).
escape_comments([0'\\,0'!|CL],BL,[0'\\,0'!|CL2]):- % preserve \!s
        !,
        escape_comments(CL,BL,CL2).
escape_comments([0'!|CL],_,CL2):- % ditch trailing ws and skip to nl
        !,
        skip_until_newline(CL,CL1),
        escape_comments(CL1,[],CL2).
escape_comments([C|CL],BL,CL2):- % retain intermediate ws
        !,
        escape_comments(CL,[],CL1),
        append(BL,[C|CL1],CL2).

skip_until_newline([],[]).
skip_until_newline([C|CL],[C|CL]):-
        newline(C),
        !.
skip_until_newline([_|CL],CL2):-
        skip_until_newline(CL,CL2).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test_obo)=
      ((true,load_biofile(obo_test,'test2.obo')))/[]).
        
unittest(test(load_obo_file,
             [_=load(test_obo)],
            (   ensure_loaded(bio(ontol_db))),
            (1=24))).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/08/24 23:51:06 $
  @license LGPL

  ---+ Name
  ---++ parser_obo
- parses obo 1.0 and 1.2 ontology formats

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).

  demo:-
    load_biofile(obo_native,'sofa.obo'),
    write_biofile(obo,'sofa.obo'),
    class(ID,'transcript'),
    setof(ParentID,subclassRT(ID,ParentID),ParentIDs),
    writeln(parents=ParentIDs).    
  ==

  ---+ Description

  parses obo format files - stores data in the ontol_db data module,
as predicates class/2, subclass/2, restriction/3, etc
  
  ---++ See also
  
  <http://www.geneontology.org> GO

  */
