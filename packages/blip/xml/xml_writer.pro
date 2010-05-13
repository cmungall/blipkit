/* -*- Mode: Prolog -*- */



:- module(xml_writer,
          [
           doc_start/1,
           xmldoc_start/1,
           xmlnode_start/3,
           xmlnode_start/4,
           xmlnode_end/3,
           xmlnode_end/2,
           xmlnode/2,
           xmlnode/3,
           xmlnode_comment/2
          ]).
:- use_module(library(sgml)).            % used for escaping


writetab2(0).
writetab2(D):-
	write('  '),
	ND is D-1,
	ND >= 0,
	writetab2(ND).


%% doc_start(?Writer)
%   DEPRECATED SYNONYM for xmldoc_start/1
%*
doc_start(xml([])):-
	writeln('<?xml version="1.0"?>').

%% xmldoc_start(?Writer)
%  
%*
xmldoc_start(xml([])):-
	writeln('<?xml version="1.0"?>').

%% xmlnode_start(+WriterIn,-WriterOut,+Element)
%  det 
%
%*
xmlnode_start(xml(P),X,N):-
	!,
	xmlnode_start(xml(P),X,N,[]).
xmlnode_start(W,_,N):-
        throw(xmlnode_start(W,_,N)).
%% xmlnode_start(+WriterIn,-WriterOut,+Element,+AttL)
%  det 
%
%*
xmlnode_start(xml(P),xml([N|P]),N,AL):-
	!,
        nl,
	length(P,T),
	writetab2(T),
	write('<'),writename(N),
	write_attrl(AL),
	write('>').
xmlnode_start(W,_,N,AL):-
        throw(xmlnode_start(W,_,N,AL)).

%% xmlnode_end(+WriterIn,-WriterOut)
%  det 
%
%*
xmlnode_end(xml([N|P]),xml(P)):-
	!,
	write('</'),writename(N),
	write('>').
%% xmlnode_end(+WriterIn,-WriterOut,+Element)
%  det 
%
%*
xmlnode_end(xml([N|P]),X,N):-
	!,
	xmlnode_end(xml([N|P]),X).
xmlnode_end(xml(P),_,N):-
	throw(end_xmlnode_does_not_match_stack(P,N)).
xmlnode_end(W,_,N):-
        throw(xmlnode_end(W,_,N)).
xmlnode_end(W,_,N,AL):-
        throw(xmlnode_end(W,_,N,AL)).
%% xmlnode_end_trailing(+WriterIn,-WriterOut)
%  det 
%
% as xmlnode_end/2, but does not preceed closing tags with nl/tab
% (ie "trails" closing tag)
%*
xmlnode_end_trailing(xml([N|P]),xml(P)):-
	!,
	write('</'),writename(N),
	write('>').

xmlnode_cmt(xml(P),WL):-
        WL=[_|_],
        !,
        concat_atom(WL,' ',Cmt),
        xmlnode_cmt(xml(P),Cmt).
xmlnode_cmt(xml(P),Cmt):-
	!,
        nl,
	length(P,T),
	writetab2(T),
	write('<!-- '),write(Cmt),
	writeln(' -->').

%% xmlnode(+Writer,elt(Name,AttL,SubNodeL))
%  det
%
% Writer is unmodified; no writer out
%*
xmlnode(xml(P),elt(N,AL,NL)):-
	!,
	xmlnode_start(xml(P),X,N,AL),
	xmlnodes(X,NL),
        (is_list(NL) ->
            xmlnode_end(X,xml(P),N)
        ;
            % data elt; no newline/tab
            xmlnode_end_trailing(X,xml(P))
        ).
            
xmlnode(xml(P),elt(N,AL)):-
	!,
        nl,
	length(P,T),
	writetab2(T),
	write('<'),writename(N),
	write_attrl(AL),
	write('/>').
xmlnode(xml(_),D):-
	!,
	writebody(D).
xmlnode(W,N):-
        throw(xmlnode(W,N)).

xmlnode(xml(P),N,PCData):-
	!,
        xmlnode(xml(P),elt(N,[],PCData)).

%% xmlnodes(+Writer,EltL)
%  det
%
%*
xmlnodes(xml(_),[]):-!.
xmlnodes(xml(P),[N|NL]):-
	!,
	xmlnode(xml(P),N),
	xmlnodes(xml(P),NL).
xmlnodes(xml(P),D):-
        !,
	xmlnode(xml(P),D).
xmlnodes(W,N):-
        throw(xmlnodes(W,N)).

writename(NS:N):-
        !,
        write(NS),write(':'),write(N).
writename(N):- write(N).

xmlnode_comment(xml(P),Comment):-
	!,
        nl,
	length(P,T),
	writetab2(T),
	write('<!-- '),
	write(Comment),
	write(' -->').
        
writebody(X):-
        (   compound(X)
        ->  write(X)            % allow terms in body - just write out directly
        ;   xml_quote_cdata(X,Q),
            write(Q)).

%% write_attrl(AttrL)
%  det PRIV
%
%*
write_attrl([]).
write_attrl([A=noesc(V)|AL]):-
	write(' '),writename(A),write('='),write('"'),
        write(V),
        write('"'),
	write_attrl(AL).
write_attrl([A=V|AL]):-
        xml_quote_cdata(V,VEsc),
	write(' '),writename(A),write('='),write('"'),
        write(VEsc),
        write('"'),
	write_attrl(AL).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.11 $
  @date  $Date: 2006/01/04 11:49:05 $
  @license LGPL

  ---+ Name
  ---++ xml_writer
- simple xml export

  ---+ Synopsis

  ==
  :- use_module(bio(xml_writer)).

  demo:-
     doc_start(X),
     xmlnode_start(X,X2,note,[id=1]),
     xmlnode(X2,hello),
     xmlnode_end(X2,X).
  ==

  ---+ Description

  writes xml, including indentation - keeps track of the current stack

**/
