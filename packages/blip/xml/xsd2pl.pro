
elts2clauseconj([Elt],Clause):-
        elt2clause(Elt,Clause).
elts2clauseconj([Elt|Elts],(Clause,Clause)):-
        elt2clause(Elt,Clause),
        elts2clauseconj(Elts,Clauses).

elt2clause(element('http://www.w3.org/2001/XMLSchema:attribute',Atts,[]), (member(Name,var('Atts'))) ) :-
        memberck(name=Name,Atts).

elt2clause(element('http://www.w3.org/2001/XMLSchema:element',Atts,Elts), (forall(member(element(Ref,var('Atts'),var('SubElts')),var('Elts')),MatchGoal)) :-
        memberck(ref=Ref,Atts),
        MatchGoal=..[Ref,var('Atts'),var('SubElts')].
        
top_elt2clause(element('http://www.w3.org/2001/XMLSchema:element',Atts,Elts), Clause) :-
        memberchk(name=Name,Atts),
        memberchk(element('http://www.w3.org/2001/XMLSchema:complexType',_,ComplexTypeElts),Elts),
        !,
        complexType2clause(Name,ComplexTypeElts,Clause).

complexType2clause(Name,Elts,Clause) :-
        elts2clauseconj(Elts,Clause).
        
