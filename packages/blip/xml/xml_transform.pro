/* -*- Mode: Prolog -*- */

:- module(xml_transform,
          [
           xml_full_to_compact/2,
           xml_compact_to_full/2,
           pxpath/3,
           apply_xmlpred/3,
           apply_xmlpred/4,
           reverse_xmlpred/3
          ]).
:- use_module(bio(mode)).

ensure_list(L,L):-
        var(L),!.               % NEW!! may not be instantiated at beginning..
ensure_list(L,L):-
	L=[], !.
ensure_list(L,L):-
	L=[_|_], !.
%ensure_list(L,L):-
%	is_list(L).
ensure_list(X,[X]) :- !.


remove_ns(_:N,N):-!.
remove_ns(N,N):-!.

xml_full_to_compact(element(Name,Atts,SubElements),Term):-
        !,
        maplist(xml_full_to_compact,SubElements,SubTerms),
        append(Atts,SubTerms,TermArgs),
        Term=.. [Name|TermArgs].
xml_full_to_compact(X,X).
        
%% xml_compact_to_full(+Term,?XmlElement)
%
%  turns compact syntax into sgml-module representations
%
%  eg foo([SubNodes]) => element(foo,[SubNodes])
%  eg a([SubNodes]) => element(foo,[SubNodes])
%  
:- mode xml_compact_to_full(+,?) is det.
xml_compact_to_full(Term,element(Name,Atts,SubElements)):-
        Term=.. [Name,TermArgs],
        collect_atts_and_subelements(TermArgs,Atts,SubElements).

collect_atts_and_subelements([],[],[]):- !.
collect_atts_and_subelements([Att=Val|TermArgs],[Att=Val|Atts],SubElements):-
        !,
        collect_atts_and_subelements(TermArgs,Atts,SubElements).
collect_atts_and_subelements([TermArg|TermArgs],Atts,[SubElement|SubElements]):-
        !,
        xml_compact_to_full(TermArg,SubElement),
        collect_atts_and_subelements(TermArgs,Atts,SubElements).
collect_atts_and_subelements(PCData,[],[PCData]):- atom( PCData ).

%% apply_xmlpred(+Module,+Node,?Preds)
%  mode: det
%   transform an XML tree into a list of flat terms that can
%  then be asserted as data predicates
%
%  requires xmlpred/4 to be defined in Module
apply_xmlpred(Mdl,El,Preds):-
        apply_xmlpred(Mdl,El,in,Preds).

% (+Module,+Node,+Mode,?Preds)
% Mode should be fully instantiated, must match
apply_xmlpred(_,[],_,[]):- !.
apply_xmlpred(Mdl,[El|Els],Mode,Preds):-
        apply_xmlpred(Mdl,El,Mode,Preds1),
        apply_xmlpred(Mdl,Els,Mode,Preds2),
        append(Preds1,Preds2,Preds),
        !.
apply_xmlpred(Mdl,El,Mode,Preds):- % match element; no xpath
        El = element(Name,_Atts,_SubEls),
        remove_ns(Name,UnqualifiedName),
        Mdl:xmlpred(UnqualifiedName,Mode,PredOrPreds,Code),
        ensure_list(PredOrPreds,Preds1),
        ensure_list(Code,Cmds),
        debug(process,'In: ~w Cmds: ~w',[Name,Cmds]),
        apply_cmds(Mdl,El,Mode,Cmds,Preds2),
        debug(process,'In: ~w Sub preds: ~w ;; apending ~w -> ~w',[Name,Preds2,Preds1,Preds]),
        append(Preds1,Preds2,Preds),
        debug(process,'In: ~w New preds:~w',[Name,Preds]),
        !.
apply_xmlpred(Mdl,El,Mode,Preds):- % match attribute xpath
        El = att(Att=Val),
        Mdl:xmlpred(XPath,Mode,PredOrPreds,Code),
        pxpath_nd(element(anon,[Att=Val],[]),XPath,ResultNode),
        node_value(ResultNode,Val),
        ensure_list(PredOrPreds,Preds1),
        ensure_list(Code,Cmds),
        debug(process,'In: ~w Cmds: ~w',[Name,Cmds]),
        apply_cmds(Mdl,El,Mode,Cmds,Preds2),
        debug(process,'In: ~w New preds:~w',[Name,Preds2]),
        append(Preds1,Preds2,Preds),
        !.
apply_xmlpred(Mdl,El,Mode,Preds):-
        throw(error(apply_xmlpred(Mdl,El,Mode,Preds))).
        
reverse_xmlpred(Mdl,El,Preds):-
        reverse_xmlpred(Mdl,El,_,Preds).
reverse_xmlpred(Mdl,El,Mode,Preds):-
        debug(xml,'in_reverse_xmlpred ~w', -(Mdl,El,Mode,Preds)),
        El = element(Name,_Atts,_SubEls),
        Mdl:xmlpred(Name,Mode,PredOrPreds,Code), % (?,?,?,?) nd++!
        ensure_list(PredOrPreds,Preds1),
        Preds1 \= [],
        append(Preds1,Preds2,Preds), % (+,?,+)
        ensure_list(Code,Cmds),
        reverse_cmds(Mdl,El,Mode,Cmds,Preds2). % (+,?,?,+,+)

% apply_cmds(+Mdl,+El,+Mode,+Cmds,?Preds) d
apply_cmds(_,_El,_Mode,[],[]):- !.
apply_cmds(Mdl,El,Mode,[Cmd|Cmds],Preds):-
        apply_cmd(Mdl,El,Mode,Cmd,Preds1),
        apply_cmds(Mdl,El,Mode,Cmds,Preds2),
        append(Preds1,Preds2,Preds),
        !.
apply_cmds(Mdl,El,Mode,Cmds,Preds):-
        throw(error(apply_cmds(Mdl,El,Mode,Cmds,Preds))).
        
% reverse_cmds(+,?,?,+,+)
reverse_cmds(_,_El,_Mode,[],[]).
reverse_cmds(Mdl,El,Mode,[Cmd|Cmds],Preds):-
        reverse_cmd(Mdl,El,Mode,Cmd,Preds1),
        reverse_cmds(Mdl,El,Mode,Cmds,Preds2),
        append(Preds1,Preds2,Preds).

/*
 apply_cmd(+Module,+El,+Mode,+Cmd,?Preds) d

  Cmd = translate/2 | let/1
*/

apply_cmd(_,El,_Mode,node(El),[]):- !. % assign node to var
% let(X=Query)
%  performs xpath and gets the pcdata of the result
apply_cmd(_,El,_Mode,Cmd,[]):-
        Cmd =.. [let|AV],
        ensure_list(AV,AVs),
        apply_lets(El,AVs),
        debug(process,' Assigned: ~w',[AVs]),
        !.
apply_cmd(_,El,_Mode,Cmd,[]):-
        Cmd =.. [mvlet|AV],
        ensure_list(AV,AVs),
        apply_mvlets(El,AVs),
        debug(process,' Assigned: ~w',[AVs]),
        !.
% if translate is called with 1 arg, then inherit current mode
apply_cmd(Mdl,El,Mode,translate(XPath),Preds):-
        apply_cmd(Mdl,El,Mode,translate(XPath,Mode),Preds),
        !.
apply_cmd(Mdl,El,_Mode,translate(XPath,NewMode),Preds):-
        debug(process,' Translating xpath: ~w',[XPath]),
        (pxpath(El,XPath,Vals)
        ->  apply_xmlpred(Mdl,Vals,NewMode,Preds)
        ;   Preds=[]),
        !.
apply_cmd(M,_,_,prolog(Prolog),[]):-
        M:call(Prolog),
        !.
apply_cmd(Mdl,El,Mode,Cmd,_):-
        throw(error(apply_cmd(Mdl,El,Mode,Cmd,_))).
        
% (+Node,+AVs)
%  AV = ?Att=+Path
apply_lets(_,[]).
apply_lets(El,[AV|AVs]):-
        apply_let(El,AV),
        apply_lets(El,AVs).
apply_let(El,Var=XPath):-
        pxpath(El,XPath,Vals),
        (Vals=[]
        ->  null_atom(Var)
        ;  (Vals=[ResultNode]
           ->  node_value(ResultNode,Var)
           ;   throw(expected(count(1,pcdata),got(Vals))))).

% multi-value
apply_mvlets(_,[]).
apply_mvlets(El,[AV|AVs]):-
        apply_mvlet(El,AV),
        apply_mvlets(El,AVs).
apply_mvlet(El,Var=XPath):-
        pxpath(El,XPath,Var).


/*
 reverse_cmd(+Module,?El,?Mode,+Cmd,?Preds) nd
*/
% Var is ground because it should appar in the Pred
reverse_cmd(_,El,_Mode,let(Var=XPath),[]):-
        reverse_pxpath(El,XPath,[element(_,_,[Var])]).
reverse_cmd(Mdl,El,Mode,translate(XPath),Preds):-
        reverse_cmd(Mdl,El,Mode,translate(XPath,Mode),Preds).
reverse_cmd(Mdl,El,_Mode,translate(XPath,NewMode),Preds):-
        (pxpath(El,XPath,Vals) % (+ish,+,?)
        ->  reverse_xmlpred(Mdl,Vals,NewMode,Preds)
        ;   Preds=[]).

%% node_value(+Element,?Val) is semidet.
% translates XML element to atomic value
node_value(element(_,_,[]),Null):- null_atom(Null).
node_value(element(_,_,[PCData]),PCData).
node_value(att(_=V),V).

%% pxpath(+El,+Query,?Results:list)
%
%  @param El
%  a node in XML tree
%
%  @param Query
%  a term representing the pxpath query
%
%  @param Results
%  a list of nodes matching query
%
%  
%  
%  XPath-like language (expressed using prolog syntax) for querying
%XML. Incomplete.
%
%  XPath maps to a query term like this:
%
%  
%  * a => =|pxpath(Node,a,ResultNodes)|=
%  * a/b/c => =|pxpath(Node,[a,b,c],ResultNodes)|=
%  * @* => =|pxpath(Node,'*',ResultNodes)|=
%  * // => =|pxpath(Node,'//',ResultNodes)|=
%  * a/b/@@c => =|pxpath(Node,[a,b,att(c)],ResultNodes)|=
%  * a/b/@* => =|pxpath(Node,[a,b,'*'],ResultNodes)|=
%  * a/b/@@* => =|pxpath(Node,[a,b,att('*')],ResultNodes)|=
%  * name(.) => =|pxpath(Node,name('.'),ResultNodes)|=
%  

pxpath(El,Q,Results):-
        findall(Result,pxpath_nd(El,Q,Result),Results).

% nondeterministic

% Q =[N1,N2,...] : equiv to N1/N2 in xpath; recurse down tree
pxpath_nd(El,[Q],Result):-
        pxpath_nd(El,Q,Result).
pxpath_nd(El,[Q|Qs],Result):-
        pxpath_nd(El,Q,SubEl),
        pxpath_nd(SubEl,Qs,Result).
% attribute query: what should it return? element? att term?
pxpath_nd(element(_,Atts,_),att(AttName),att(AttName=V)):-
        member(AttName=V,Atts).
pxpath_nd(element(_,Atts,_),att('*'),att(A=V)):-
        member(A=V,Atts).
% name of current node
pxpath_nd(element(Name,_,_),name('.'),element(anon,[],[Name])).
pxpath_nd(att(A=_),name('.'),element(anon,[],[A])).
% convert to number
pxpath_nd(El,number(Q),element(N,Atts,[Num])):-
        pxpath_nd(El,Q,element(N,Atts,[A])),
	debug(process,'atom->num ~w',[A]),
        atom_number(A,Num).
pxpath_nd(El,xml('.'),element(xml,[],[El])):- !.
% current node
pxpath_nd(El,'.',El).
% element query
pxpath_nd(element(_,_,SubEls),QueryEl,Result):-
        Result=element(QueryEl,_,_),
        member(Result,SubEls).
% basic element query over ns xml
pxpath_nd(element(_,_,SubEls),QueryEl,Result):-
        Result=element(_:QueryEl,_,_),
        member(Result,SubEls).
pxpath_nd(element(_,_,SubEls),'*',Result):-
        member(Result,SubEls).
% any or below
pxpath_nd(element(_,_,SubEls),'//',Result):-
        member(Result,SubEls).
pxpath_nd(element(_,_,SubEls),'//',Result):-
        member(Result1,SubEls),
        pxpath_nd(Result1,'//',Result).
pxpath_nd(El,xml_to_term(Q),element(anon,[],[ResultTerm])):-
        pxpath_nd(El,Q,ResultNode),
        xml_to_term(ResultNode,ResultTerm).
pxpath_nd(El,xml_to_atom(Q),element(anon,[],[A])):-
        format(user_error,'xml_to_atom~n',[]),
        pxpath_nd(El,Q,ResultNode),
        tmp_file(xml,XmlFile),
        open(XmlFile,write,IO,[dialect(xml)]),
        sgml_write(IO,ResultNode,[]),
        close(IO),
        read_file_to_codes(XmlFile,Codes,[]),
        atom_codes(A,Codes).
pxpath_nd(El,xml_to_file(Q,File),element(anon,[],[File])):-
        (   var(File)
        ->  tmp_file(xml,File)
        ;   true),
        pxpath_nd(El,Q,ResultNode),
        open(File,write,IO,[]),
        sgml_write(IO,ResultNode,[]),
        close(IO).

% gets rid of atts
xml_to_term(element(N,_,SubNodes),Term):-
        remove_ns(N,N2),
        (   is_list(SubNodes)
        ->  maplist(xml_to_term,SubNodes,SubTerms),
            Term =.. [N2,SubTerms]
        ;   SubTerms=SubNodes,
            Term =.. [N2]),
        !.
xml_to_term(Node,Node):-
        \+ compound(Node),
        !.
xml_to_term(Node,Term):-
        throw(error(xml_to_term(Node,Term))).

null_atom('').

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(bm18)=
      (   ensure_loaded(library(sgml)),
          File='BIOMD0000000018.xml',
          load_structure(File,[Node],[dialect(xmlns),space(remove)])
      )/Node).

unittest(load(p)=
      (   ensure_loaded(library(sgml)),
          File='personset.xml',
          load_structure(File,[Node],[dialect(xmlns),space(remove)])
      )/Node).

unittest(test(basic_path_search,
            [Node=load(bm18)],
            (   pxpath(Node,[model,notes],Results),
                length(Results,NumResults)),
            (NumResults=1))).

unittest(test(att_search,
            [Node=load(bm18)],
            pxpath(Node,[model,att(id)],Results),
            (Results=[att(id='MorrisonAllegra')]))).

unittest(test(att_wildcard_search,
            [Node=load(bm18)],
            (   pxpath(Node,[model,att('*')],Results),
                length(Results,NumResults)),
            (NumResults=3))).

unittest(test(other_search,
            [Node=load(bm18)],
            (   pxpath(Node,[model,annotation,'RDF'],Results),
                length(Results,NumResults)),
            (NumResults=1))).

unittest(test(null,
            [Node=load(p)],
            (   pxpath(Node,[person,notes],Results)),
            Results=[element(_,_,[])])).

unittest(test(compact,[],
            (   findall(X,xml_compact_to_full(foo([x=1,bar(y)]),X),Xs),
                maplist(writeln,Xs)),
            true)).

reverse_pxpath(Node,Path,ResultNodes):-
        writeln(reverse_pxpath(Node,Path,ResultNodes)),
        fail.
reverse_pxpath(Node,_Path,[Node]).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.8 $
  @date  $Date: 2005/09/30 16:23:45 $
  @license LGPL


  ---+ Name
  ---++ xml_transform
- xml queries and xml-to-fact transforms

  ---+ Synopsis

  ==
  % purpose: convert an xml file with 'person' elements
  % and nested 'address' elements to a flat term list
  :- use_module(bio(xml_transform)).
  :- use_module(library(sgml)).

  % define the mapping in the 'user' module
  user:xmlpred(personset,_,[],translate(person)).
  user:xmlpred(person,_,person(ID,Name),
               [let(ID=ssid),
                let(Name=fullname),
                prolog(M=in_person(ID)),
                translate(address,M)]).
  user:xmlpred(address,in_person(PID),person_address(PID,Street,City),
               [let(Street=street),
                let(City=city)]).

  % convert XML to prolog facts and write them out
  demo:-
    File='personset.xml',
    load_structure(File,Els,[dialect(xml),space(remove)]),
    apply_xmlpred(user,Els,Preds),
    forall(member(Pred,Preds),
           writeln(Pred)).
  ==

  ---+ Description

  This module allows querying and transformation of XML structures
(obtained from the SWI-Prolog sgml module)

  In particular, it is useful for providing a mapping between XML
models and the datalog style data predicates found in the blip db
modules

  See the source for the following modules for more details:

  
    * seqfeature_xmlmap_chaos
  

  ---++ TODO

  More advanced querying

  reversible: preds to XML

  check for nondet
  
  */
