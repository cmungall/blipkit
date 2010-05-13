:- module(parser_gff3,[]).

:- use_module(bio(seqfeature_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(bioprolog_util),[solutions/3]).

:- multifile io:parse_stream/2.
io:parse_stream(gff3,IO):-
        load_gff3(IO).
io:parse_stream(gff,IO):-
        load_gff3(IO).

:- dynamic gff_fact/1.

load_gff3(IO):-
        retractall(gff_fact(_)),
        parse_gff_stream(null(_),IO),
        findall(Fact,gff_fact(Fact),Facts),
        insert_facts(Facts,[]),
        retractall(gff_fact(_)).

%% parse_gff_stream(+LastFeat,+IO)
% asserts gff_fact/1 for features in stream.
% parses one line at a time. keeps track of previous feature,
% because features in GFF may be split over multiple lines
parse_gff_stream(LastFeat,IO):-
        read_line_to_codes(IO,Codes),
        Codes\=end_of_file,
        !,
        atom_codes(A,Codes),
        debug(gff3,'Line: ~w',[A]),
        parse_gffline(A,ThisFeat),
        collate_feature(LastFeat,ThisFeat,NextFeat),
        parse_gff_stream(NextFeat,IO).
parse_gff_stream(LastFeat,_) :-
        emit_feature(LastFeat).

%% collate_feature(+LastFeat,+ThisFeat,?NewFeat)
% if LastFeat and ThisFeat share the same column values, then these are
% merged into a multi-location feature
collate_feature(feature(Seq,Src,Type,LastLocs,AttVals),
                feature(Seq,Src,Type,Loc,AttVals),
                feature(Seq,Src,Type,[Loc|LastLocs],AttVals)). % collate
collate_feature(LastFeat,
                feature(Seq,Src,Type,Loc,AttVals),
                feature(Seq,Src,Type,[Loc],AttVals)):-
        emit_feature(LastFeat).               % new spanset, emit last and make new
collate_feature(LastFeat,NextFeat,NextFeat) :-  % non-feature row (e.g. seq)
        emit_feature(LastFeat).

% store the parsed feature.
emit_feature(Feat) :-
        gffterm_facts(Feat,Facts),
        forall(member(Fact,Facts),
               assert(gff_fact(Fact))).
%        insert_facts(Facts,[]).

%parse_and_load_line(A):-
%        gffline_facts(A,Facts),
%        insert_facts(Facts,[]).


%gffline_facts(A,Facts):-
%        debug(gff3,'Parsing: ~w',[A]),
%        parse_gffline(A,Row),
%        gffterm_facts(Row,Facts).

parse_gffline(Atom,F):-
        concat_atom(Vals,'\t',Atom),
        process_vals(Vals,Vals2),
        Vals2=[Seq,Src,Type,S,E,Score,Strand,Phase,AttrAtom],
        !,
        parse_atts(AttrAtom,AttVals),
        F=feature(Seq,Src,Type,loc(S,E,Score,Strand,Phase),AttVals).
parse_gffline(_Atom,null(_)).

%gffterm_facts(+FeatTem,?Facts:list) is det
% FeatTerm = feature(Seq,Src,Type,Locs,AttVals)
% Facts = list of facts of type:
%  * feature/1 (1)
%  * feature_type/2 (1)
%  * featureloc/5 (n)
%  * others (see attval_fact/3)
gffterm_facts(feature(Seq,Src,Type,Locs,AttVals),
              [seqfeature_db:feature(F),
               seqfeature_db:feature_type(F,Type)|Rest]):-
        maplist(gff_to_featureloc,Locs,FLocs),
        (   member(id=F,AttVals)
        ->  true
        ;   F=f(Seq,Src,Type,FLocs)), % skolemize: split features are collated
        % generate featureloc/5 facts
        findall(seqfeature_db:featureloc(F,Seq,S,E,Strand),
                member(loc(S,E,Strand,_Score,_Phase),FLocs),
                FLocFacts),
        debug(gff3,'  AttVals ~w',[AttVals]),
        solutions(Fact,
                  (   member(A=V,AttVals),attval_fact(A=V,F,Fact)),
                  AttValFacts),
        append(FLocFacts,AttValFacts,Rest),
        !.
gffterm_facts(null(_),[]):- !.
gffterm_facts(F,[]):-
        format(user_error,'Cannot parse: ~w~n',[F]).


gff_to_featureloc(loc(S1A,E1A,Score,Strand1,Phase),loc(S,E,Strand,Score,Phase)):-
        S1A\='.',
        E1A\='.',
        atom_number(S1A,S1),
        atom_number(E1A,E1),
        map_coordsystem(gff(S1,E1,Strand1),directional_interbase(S,E,Strand)).


%% attval_fact(+TagValPair,+Feature,?Fact) is det
% map reified attribute-value pair to model predicate;
% if no mapping can be found, keep reified as featureprop/3
attval_fact(id=_,_,_):- !,fail. % process and do nothing
attval_fact(name=V,F,metadata_db:entity_label(F,V)):- !.
attval_fact(alias=V,F,metadata_db:entity_synonym(F,V)):- !.
attval_fact(parent=V,F,seqfeature_db:feature_relationship(F,V,part_of,0)):- !.
attval_fact(derives_from=V,F,seqfeature_db:feature_relationship(F,V,derives_from,0)):- !.
attval_fact(target=V,F,FLoc):- !,parse_target(V,F,FLoc).
attval_fact(dbxref=V,F,seqfeature_db:feature_dbxref(F,V)):- !.
attval_fact(ontology_term=V,F,seqfeature_db:feature_cvterm(F,V)):- !.
%attval_fact(ontology_term=V,F,curation_db:curation_statement(_,F,has_role,V)):- !.
attval_fact(_='.',_,_):- fail.
attval_fact(A=V,F,seqfeature_db:featureprop(F,A,V)):- !.

% gff Target column (e.g. for matches)
parse_target(A,F,seqfeature_db:featureloc(F,Seq,Start,End,Strand)):-
        concat_atom([Seq,StartA,EndA|Rest],' ',A),
        atom_number(StartA,Start),
        atom_number(EndA,End),
        (   Rest=[Strand|_]
        ->  true
        ;   Strand='.').


% col9 of GFF is a ';' separated list of tag=value pairs
parse_atts(A,AVs):-
        concat_atom(Blobs,';',A),
        !,
        findall(AV,(member(Blob,Blobs),parse_att(Blob,AV)),AVs).
parse_atts(A,_):-
        throw(bad_atts(A)).

%% parse_att(+AttAtom,?TagValPair) is nondet
% TagValPair = Tag=Val
% succeeds multiple times for ','-separated lists
parse_att(A,T=V):-
        concat_atom([T1,V1],'=',A),
        !,
        downcase_atom(T1,T),
        concat_atom(Vs1,',',V1),
        member(V2,Vs1),
        debug(gff3,'  ~w=~w',[T,V2]),
        unesc(V2,V).
parse_att('',_):- !, fail.
parse_att(A,_):-
        throw(bad_att(A)).

process_vals(X,X).
unesc(X,X).

%unesc(X,Xd):-
%        downcase_atom(X,Xd).



        

/** <module> 

  ---+ Synopsis

==
:- use_module(bio(parser_gff3)).

% 
demo:-
  nl.
  

==

---+ benchmarks


1568112 lines in this GFF file; 1k per feature

---+ Details

http://www.sequenceontology.org/gff3.shtml

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
