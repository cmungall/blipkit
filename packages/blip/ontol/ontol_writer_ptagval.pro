/* -*- Mode: Prolog -*- */



:- module(ontol_writer_ptagval, [
                                 ]).

:- multifile ontol_writer:write_class/2.

:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(serval)).

io:redirect_stdout(ptagval).
        
io:write_all(ptagval,_,_):-
        %write_header(ptagval),
        forall_distinct(class(C),
                        write_entity(ptagval,C)).

:- mode write_entity(+,+) is det.
write_entity(ptagval,ID):-
        user:ensure_loaded(bio(ontol_db)),   % serval runs in user space...
        user:ensure_loaded(bio(metadata_db)),   % serval runs in user space... TODO; inherit this?
        write_sterm([],xml([]),entity(ID)).
                                %maplist(write,Tokens).

entity(ID) =>
 xref(ID,X) forall_unique entity_xref(ID,X),
 term_genus(ID,Genus) forall_unique genus(ID,Genus).

term_genus(ID,Genus) =>
 term(ID),
 'EquivalentTo= ',
 genus_diff(ID,Genus),
 newline.

xref(ID,X) =>
 term(ID),
 comment(N) where entity_label(ID,N),
 tagval('Xref',X),
 tagval('XrefName',quoted(XN)) where entity_label(X,XN),
 newline.

term(ID) =>
 tagval('Term',ID),
 comment(N) where entity_label(ID,N).

newline => NL where atom_codes(NL,"\n").
tagval(Tag,Val) => Tag,'= ',Val,' '.
comment(C) => '/* ',C,' */ '.
quoted(X) => '"',escape_val(X),'"'.
escape_val(Val) => Esc where ontol_writer_obo:obo_escape(Val,Esc).

% lifted from ontol_writer_obo
genus_diff(Node,Genus)=>
 identifier(Genus),
 differentium(R,X) forall differentium(Node,R,X).

differentium(R,To)=>
  '^',
  property(R),
  '(',
  identifier(To),
  ')'.
identifier(Node) =>
  if( (is_anonymous(Node),
       ontol_writer_ptagval:obo_format_option(pheno_syntax),
       genus(Node,Genus)),
      then:
    genus_diff(Node,Genus),
      else:
    data(Node)).

        

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.14 $
  @date  $Date: 2006/03/27 17:57:51 $
  @license LGPL

  ---+ Name
  ---++ ontol_writer_ptagval
- generates obo format from ontol_db

  ---+ Synopsis

  ==
  

  ==

  ---+ Description

**/
