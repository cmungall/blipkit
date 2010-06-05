/* -*- Mode: Prolog -*- */

:- module(kegg_xml,[]).

:- use_module(bio(xml_transform)).
:- use_module(bio(kegg_db)).
:- use_module(bio(metadata_db)).

io:xml_to_preds(keggxml,XML,PL):-
        apply_xmlpred(kegg_xml,XML,PL).

xmlpred(pathway,_,
	kpathway(Name,Org,Number,Title),
        [let(Name=att(name)),
	 let(Org=att(org)),
	 let(Number=att(number)),
	 let(Title=att(title)),
	 translate(entry,in(Name)),
	 translate(relation,in(Name))
	]).

xmlpred(entry,in(P),[knode(PID,Name,Type),knode_pathway(PID,P)|Xrefs],
	[let(ID=att(id)),
	 prolog(id_p_pid(ID,P,PID)),
	 let(Name=att(name)),
	 let(Type=att(type)),
	 prolog(name_xrefpreds(PID,Name,Xrefs)),
	 translate(component,in(P,PID)),
	 translate(graphics,in(PID))]).

xmlpred(component,in(P,PID),subcomponent_of(CID,PID),
	[let(ID=att(id)),
	 prolog(id_p_pid(ID,P,CID))]).

xmlpred(relation,in(P),[],
	[let(ID1=att(entry1)),
	 let(ID2=att(entry2)),
	 prolog(id_p_pid(ID1,P,PID1)),
	 prolog(id_p_pid(ID2,P,PID2)),
	 %let(Type=att(type)),
	 translate(subtype,kedge(PID1,PID2))]).

xmlpred(subtype,kedge(PID1,PID2),kedge(T,PID1,PID2),
	let(T=att(name))).

xmlpred(reaction,in(P),[],kreaction(UName,Type),
	[let(Name=att(name)),
	 prolog(id_p_pid(Name,P,UName)),
	 let(Type=att(type)), 
	 translate(substrate,r(P,UName)),
	 translate(product,r(P,UName))]).

xmlpred(substrate,r(P,R),ksubstrate(R,UName),
	[let(Name=att(name)),
	 prolog(id_p_pid(Name,P,UName))]).

xmlpred(product,r(P,R),kproduct(R,UName),
	[let(Name=att(name)),
	 prolog(id_p_pid(Name,P,UName))]).

	 

xmlpred(graphics,in(PID),graphics(PID,Name,X,Y,W,H),
	[let(Name=att(name)),
	 let(X=att(x)),
	 let(Y=att(y)),
	 let(W=att(width)),
	 let(H=att(height))]).

id_p_pid(ID,P,PID) :- concat_atom([P,'-',ID],PID).

name_xrefpreds(PID,N,L) :-
	concat_atom(Toks,' ',N),
	maplist(name_xrefpred(PID),Toks,L).

name_xrefpred(PID,N,metadata_db:entity_xref(PID,N)).

/** <module> translates KEGG XML to kegg_db model

---+ Details

http://www.genome.jp/kegg/xml/docs/

  See also kegg_bridge_to_pathway

*/
