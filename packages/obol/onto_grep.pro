:- module(onto_grep,[]).

:- use_module(library(porter_stem)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(mode)).
:- use_module(bio(io)).
:- use_module(bio(av_db)).
:- use_module(bio(blipkit)).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(bioprolog_util),[solutions/3]).

%% atom_markup(+Atom,?MarkupList,+Opts) is nondet
% currently greedy: [(a b) c d e f] matches before [a (b c d e f)]
atom_markup(A,L,Opts):-
        debug(obol,'Marking up ~w',[A]),
        atom_to_stem_list(A,Toks),
        Toks\=[],
        tokens_markup(Toks,L,Opts).

%% tokens_markup(+Toks,?MarkupList,+Opts) is nondet
tokens_markup([],[],_):- !.
tokens_markup(Toks,[class(C,LT)|ML],Opts):-
        debug(obol,'toks=~w',[Toks]),
        class_lindex(C,Toks,Len,LT,Tail),
        \+ member(exclude(C),Opts),
        % check there is no better match
        \+ ( (    class_lindex(C2,Toks,Len2,_,_),
                  Len2>Len,
                  \+ member(exclude(C2),Opts),
                  debug(obol, '  better match: ~w',[C2]))),
        debug(obol,'  ~w toks=~w',[C,Toks]),
        !, % todo : alternatives
        tokens_markup(Tail,ML,Opts).
tokens_markup([Tok|Toks],[Tok|ML],Opts):-
        tokens_markup(Toks,ML,Opts).

%% class_lindex(?Class,+Toks,?Len,?Label-Type,?Tail) is nondet
% class_lindex(+Class,+Toks,?Len,?Label-Type,?Tail) is det
class_lindex(C,Toks,Len,Label-Type,Tail):-
        entity_label_scope(C,Label,Type),
        atom_to_stem_list(Label,HeadToks),
        length(HeadToks,Len),
        Len>0,                  % post-stemming chemical symbols may be empty 
        (   HeadToks=[W1]       % e.g. CHEBI 'OF'
        ->  atom_length(W1,W1Len),
            W1Len>3
        ;   true),
        append(HeadToks,Tail,Toks).

:- blip('onto-grep-phrase',
        'given a phrase, markup ontology terms',
        [
        ],
        Atoms,
        (   forall((member(Atom,Atoms),rewrite_atom(Atom,Atom2)),
                   (   atom_markup(Atom2,L,[]),
                       writeln(L))))).

:- blip('onto-grep-predicate',
        'given a predicate spec, markup ontology terms',
        [
         atom(query,QAtom)
        ],
        _,
        (
         atom_to_term(QAtom,Q,Bindings),
         member('Text'=Text,Bindings),
         forall(Q,
                (   rewrite_atom(Text,Text2),
                    atom_markup(Text2,L,[]),
                    format('~q.~n',[markup(Q,L)]))))).

:- blip('onto-grep-defs',
        'markup ontology terms in ontology definitions',
        [
         atom(id,ID),
         bool(optimize,IsOptimize),
         atom(query,QAtom1),
         atom(lookup,LookupAtom)
        ],
        QAtoms,
        (   
            (   IsOptimize=1
            ->  table_pred(class_lindex/5),
                debug(obol,'tabled',[])
            ;   true),
            solutions(ID,
                      (   member(QAtom,[QAtom1|QAtoms]),
                          nonvar(QAtom),
                          atom_to_term(QAtom,Q,Bindings),
                          member('ID'=ID,Bindings),
                          debug(obol,'Q=~w',[Q]),
                          Q),
                      IDs1),
            solutions(ID,
                      (   nonvar(LookupAtom),
                          atom_to_term(LookupAtom,Lookup,[]),
                          lookup_class(Lookup,ID)),
                      IDs2),
            append(IDs1,IDs2,IDs),
            length(IDs,NumIDs),
            debug(obol,'ids to parse [~w]: ~w',[NumIDs,IDs]),
            forall((member(ID,IDs),class(ID,N),def(ID,Def)),
                   (   atom_markup(Def,ML,[]),
                       format('~w ~w :: ',[ID,N]),
                       markup_write(ML))))).

blipkit:example('obol -debug obol -u onto_grep -r cell onto-grep -query "belongs(ID,_)" -text_template "C,N,ontol_db:class(C,N)"',
                '').
blipkit:example('obol -debug obol -u onto_grep -r cell onto-grep -query "belongs(ID,_)" -text_template "C,N,metadata_db:entity_label_scope(C,N,_)"',
                '').
:- blip('onto-grep',
        'markup ontology terms by query',
        [
         atom(id,ID),
         bool(optimize,IsOptimize),
         term(text_template,TextGoalTerm,(C,Def,def(C,Def))),
         atom(query,QAtom1),
         atom(lookup,LookupAtom)
        ],
        QAtoms,
        (   
            (   IsOptimize=1
            ->  table_pred(class_lindex/5),
                debug(obol,'tabled',[])
            ;   true),
            TextGoalTerm=..GL,
            debug(obol,'template: ~w // ~w',[TextGoalTerm,GL]),
            (   TextGoalTerm=(_,_,_)
            ->  true
            ;   format('Must be Class-Text-Goal: ~w',[TextGoalTerm])),
            solutions(ID,
                      (   member(QAtom,[QAtom1|QAtoms]),
                          nonvar(QAtom),
                          atom_to_term(QAtom,Q,Bindings),
                          member('ID'=ID,Bindings),
                          debug(obol,'Q=~w',[Q]),
                          Q),
                      IDs1),
            solutions(ID,
                      (   nonvar(LookupAtom),
                          atom_to_term(LookupAtom,Lookup,[]),
                          lookup_class(Lookup,ID)),
                      IDs2),
            append(IDs1,IDs2,IDs),
            length(IDs,NumIDs),
            debug(obol,'ids to parse [~w]: ~w',[NumIDs,IDs]),
            forall((member(ID,IDs),class(ID,N),TextGoalTerm=(ID,Text,Goal),Goal,rewrite_atom(Text,Text2)),
                   (   atom_markup(Text2,ML,[exclude(ID)]),
                       format('~q - ',[class(ID,N,Text)]),
                       markup_write(ML))))).

markup_write(ML):- writeq(ML),nl.

rewrite_atom(T,T2):-
        tokenize_atom(T,Toks),
        maplist(rewrite_token,Toks,Toks2),
        concat_atom(Toks2,' ',T2),
        T2\=T.
rewrite_atom(T,T).

rewrite_token(T,T2):-
        relational_adj_ra(T,T2,_).
rewrite_token(T,T).

okscope(label,_).
okscope(exact,_).
okscope(S,Opts):- member(allow(S),Opts).


class_toks(ID,Toks,Type):-
        entity_label_scope(ID,N,Type),
        atom_to_stem_list(N,Toks).

        
%% matches(+ID1,+ID2,+Opts) is semidet
matches(ID1,ID2,Opts):-
	entity_nlabel_scope_stemmed(ID1,Lab,Sc1,true),
	okscope(Sc1,Opts),
	entity_nlabel_scope_stemmed(ID2,Lab,Sc2,true),
	okscope(Sc2,Opts).

:- blip('onto-exact-align',
        'align matching classes. uses stemming',
        [
         bool(optimize,IsOptimize),
         terms(disp,DispOpts), % e.g. allow(related)
         bool(exclude_xref,ExcludeXref),
         bool(intra,IsIntra),
         bool(exclude_xref_strict,ExcludeXrefStrict),
         atom(ont1,Ont1),
         atom(ont2,Ont2)
        ],
        _,
        (   
            (   IsOptimize=1
            ->  table_pred(class_toks/3),
                debug(obol,'tabled',[])
            ;	true),
            load_bioresource(obol_av),
	    materialize_index(metadata_nlp:entity_nlabel_scope_stemmed(1,1,0,0)),
	    materialize_index(metadata_nlp:token_syn(1,0)),
	    debug(obol,'Onts: ~w ~w',[Ont1,Ont2]),
            forall((  belongs(ID1,Ont1),
                      \+ obsolete_class(ID1,_),
		      debug(obol,'Checking: ~w',[ID1]),
                      matches(ID1,ID2,DispOpts),
                      ID1\=ID2,
		      belongs(ID2,Ont2),
                      (   IsIntra=1
                      ->  true
                      ;   Ont1\=Ont2),
                      \+ obsolete_class(ID2,_),
                      debug(obol,'  Candidate match: ~w [~w] ~w [~w]',[ID1,Ont1,ID2,Ont2]),
                      \+ ((ExcludeXrefStrict=1, class_xref(ID2,IDx),belongs(IDx,Ont1))),
                      \+ ((ExcludeXrefStrict=1, class_xref(IDx,ID1),belongs(IDx,Ont2))),
                      \+ ((ExcludeXref=1, (class_xref(ID2,ID1) ; class_xref(ID1,ID2))))),
                   show_xref(ID2,ID1,DispOpts)))).

show_xref(ID2,ID1,Opts):-
        member(format(obo),Opts),
        !,
        debug(obol,'Found match: ~w ~w',[ID1,ID2]),
        (   entity_label(ID1,N1)
        ->  true
        ;   N1='?'),
        (   entity_label(ID2,N2)
        ->  true
        ;   N2='?'),
        (   class(ID2)
        ->  Stanza='Term'
        ;   property(ID2)
        ->  Stanza='Typedef'
        ;   Stanza='Term'),
        format('[~w]~nid: ~w ! ~w~nxref: ~w ! ~w~n~n',[Stanza,ID2,N2,ID1,N1]).


show_xref(ID2,ID1,Opts):-
        show_factrow([isLabel(1)|Opts],entity_xref(ID2,ID1)).

:- blip('onto-3-way-align',
        'align matching classes',
        [
         bool(optimize,IsOptimize),
         terms(disp,DispOpts),
         %boolean(exclude_xref,ExcludeXref),
         %boolean(exclude_xref_strict,ExcludeXrefStrict),
         atom(ont1,Ont1),
         atom(ont2,Ont2),
         atom(ont3,Ont3)
        ],
        _QAtoms,
        (   
	    % deprecated
            (   IsOptimize=1
            ->  table_pred(class_lindex/5),
                debug(obol,'tabled',[])
            ;   true),
            load_bioresource(obol_av),
	    ensure_loaded(bio(metadata_nlp)),
	    materialize_index(metadata_nlp:entity_nlabel_scope_stemmed(1,1,0,0)),
	    materialize_index(metadata_nlp:token_syn(1,0)),
            forall((  belongs(ID1,Ont1),
                      matches(ID1,ID2,DispOpts),
                      belongs(ID2,Ont2),
		      debug(obol,'candidate: ~w',[ID1-ID2]),
                      ID1\=ID2,
                      id_idspace(ID1,S1),
                      id_idspace(ID2,S2),
                      S1\=S2,
                      \+ class_xref(_,ID1),
                      \+ class_xref(_,ID2),
                      \+ ((  matches(ID1,ID3,DispOpts),
                             belongs(ID3,Ont3))),
                      \+ ((  matches(ID2,ID3,DispOpts),
                             belongs(ID3,Ont3))),
                      true),
                   show_new_term(ID2,ID1,[newclass(1)|DispOpts])))).


show_new_term(ID2,ID1,_Opts):-
        debug(obol,'hit: ~w',[ID1-ID2]),
        class(ID1,N1),
        class(ID2,N2),
        concat_atom([ID1,ID2],'-',IDx),
        concat_atom(Toks,':',IDx),
        concat_atom(Toks,'_',ID),
        !,
        format('[Term]~nid: UBERON:~w~nname: ~w~n',[ID,N1]),
        (   solutions(PID-PN,
                      (   xref_subclass(ID1,PID,PN)
                      ;   xref_subclass(ID2,PID,PN)),
                      Ps),
            Ps\=[]
        ->  true
        ;   solutions(PID-PN,
                      (   xref_subclassTnr(ID1,PID,PN)
                      ;   xref_subclassTnr(ID2,PID,PN)),
                      Ps)),
	(   def(ID1,Def)
	->  format('def: "~w" [~w]~n',[Def,ID1])
	;   def(ID2,Def)
	->  format('def: "~w" [~w]~n',[Def,ID2])
	;   true),
        forall(member(PID-PN,Ps),
               format('is_a: ~w ! ~w~n',[PID,PN])),
        forall(xref_rel(ID1,R,PID,PN),
               format('relationship: ~w ~w ! ~w~n',[R,PID,PN])),
        forall(xref_rel(ID2,R,PID,PN),
               format('relationship: ~w ~w ! ~w~n',[R,PID,PN])),
        forall(xref_synonym(ID1,S,T),
               format('synonym: "~w" ~w [~w]~n',[S,T,ID1])),
        forall(xref_synonym(ID2,S,T),
               format('synonym: "~w" ~w [~w]~n',[S,T,ID2])),
        format('xref: ~w ! ~w~nxref: ~w ! ~w~n~n',[ID2,N2,ID1,N1]).

show_new_term(ID1,ID2,_Opts):-
        print_message(error,bad_match(ID1,ID2)).


xref_synonym(ID,S,T):-
        entity_synonym_scope(ID,S,Td),
        upcase_atom(Td,T).



xref_subclass(IDx,PID,PN):-
        subclass(IDx,PIDx),
        class_xref(PID,PIDx),
        class(PID,PN),
        belongs(PID,uberon). % TODO

xref_subclassT(IDx,PID,PN):-
        subclassT(IDx,PIDx),
        class_xref(PID,PIDx),
        class(PID,PN),
        belongs(PID,uberon). % TODO

xref_subclassTnr(IDx,PID,PN):-
        xref_subclassT(IDx,PID,PN),
        \+ ((xref_subclassT(IDx,XID,_),
             subclassT(XID,PID))).

xref_rel(IDx,R,PID,PN):-
        restriction(IDx,R,PIDx),
        class_xref(PID,PIDx),
        class(PID,PN),
        belongs(PID,uberon). % TODO

:- blip('onto-grep-parse-output',
        'markup ontology terms by query',
        [
        ],
        Files,
        (
         maplist(parse_grepfile,Files))).

parse_grepfile(File):-
        open(File,read,In,[]),
        debug(obol,'Parsing ~w',[File]),
        repeat,
        read_line_to_codes(In,Codes),
        (   Codes=end_of_file
        ->  !
        ;   atom_codes(A,Codes),
            atom_to_term(A,class(X,_,_) - L,[]),
            assert(class_matches(X,L)),
            fail),
        debug(obol,'Parsed ~w',[File]),
        solutions(X,class_matches(X,_),Xs),
        solutions(X-Ys,
                  (   member(X,Xs),
                      solutions(Y,
                                (   class_matches(X,L),
                                    member(class(Y,_),L)),
                                Ys)),
                  XYs),
        debug(obol,'Showing ~w',[File]),
        solutions(X-Y,(member(L,XYs),member(Y,L)),Pairs),
        forall(member(XY,XYs),
               show_greps(XY,Pairs)),
        debug(obol,'Done: ~w',[File]).

%show_greps([]).
show_greps(X-L,Pairs):-
        solutions(Y,
                  (   
                      member(Y,L),
                      \+ subclassRT(X,Y),
                      \+ ((member(Y2,L),
                           subclassT(Y2,Y))),
                      \+ ((member(X2-Y,Pairs),
                           subclassT(X,X2)))),
                  Ys),
        forall(member(Y,Ys),
               show_factrow([isLabel(1)],xref(X,Y))).

%        show_greps(XYs).


/*
:- begin_tests(onto_grep).
:- use_module(bio(onto_grep)).

test(reverse) :-
        reverse([a,b], [b,a]).

:- end_tests(onto_grep).
*/

/** <module> finds occurrences of terms in atoms

  ---+ Synopsis

==
:- use_module(bio(onto_grep)).

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
