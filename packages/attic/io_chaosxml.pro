% highly experimental...

:- module(io_chaosxml,[
                       chaosxml_index/1
                      ]).

:- use_module(library(sgml)).
:- dynamic
        location/3.                     % Id, File, Offset

load_chaos(File):-
        load_xml_file(File,XML),
        xf.

xml_pred(XML,[feature(ID,N,Type),Flocs]):-
        xmlq(XML,element(feature,
                         [],
                         Elts)),
        xmlq(Elts,[name(N),id(ID),type(Type)]),
        findall(Floc,(  xmlq(Elts,element(featureloc,[],LocElts)),
                        xmlq(LocElts,[nbeg(Beg),
                                      nend(End),
                                      strand(Strand),
                                      srcfeature_id(Src)]),
                        Floc=featureloc(ID,Src,Beg,End,Strand)),Flocs).

xml_pred(XML,[feature_relationship(Subj,Obj,Type)]):-
        xmlq(XML,element(feature_relationship,[],Elts)),
        xmlq(Elts,[subject_id(Subj),object_id(Obj),type(Type)]).

xmlq([element(Elt,AL,Elts)|_],element(Elt,AL,Elts)).
xmlq([element(Elt,AL,Elts)|_],Q):-
        xmlq(Elts,Q).
xmlq([_|XML],Q):-
        xmlq(XML,Q).
xmlq(_,[]).
xmlq(XML,[Q|QL]):-
        Q =.. [Elt,Val],
        member(XML,element(Elt,[],Val)),
        xmlq(XML,QL).


chaosxml_index(File) :-
        retractall(location(_,_)),
        open(File, read, In, [type(binary)]),
        new_sgml_parser(Parser, []),
        set_sgml_parser(Parser, file(File)),
        set_sgml_parser(Parser, dialect(xml)),
        sgml_parse(Parser,
                   [ source(In),
                     call(cdata, on_cdata)
                   ]),
        close(In).

on_begin(Element, Attributes, Parser) :-
        writeln(el(Element,Attributes)),
        memberchk('r:id'=Id, Attributes),
        get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
        assert(location(Id, File, Offset)).
on_end(Element, Parser) :-
        writeln(el(Element,Attributes)),
        memberchk('r:id'=Id, Attributes),
        get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
        assert(location(Id, File, Offset)).
on_cdata(Element, Parser) :-
        writeln(el(Element,Attributes)),
        memberchk('r:id'=Id, Attributes),
        get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
        assert(location(Id, File, Offset)).
