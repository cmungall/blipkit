:- module(ontol_bridge_from_rules,[
                                   rule_facts/3
                                  ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).


/*

  CarbonAtom(?x) ^ CarbonAtom(?y) ^ CarbonAtom(?z) ^ CarbonAtom(?w) ^ 
CarbonAtom(?u) ^ CarbonAtom(?v) ^  hasBondWith(?x,?y) ^ 
hasBondWith(?y,?z) ^   hasBondWith(?z,?w) ^  hasBondWith(?w,?u) ^  
hasBondWith(?u,?v) ^ hasBondWith(?v,?x) --> 
  RingAtom (?x) .  

  carbon(?x) ^ carbon(?y) ^ carbon(?z) ^ carbon(?w) ^ 
carbon(?u) ^ carbon(?v) ^  hasBondWith(?x,?y) ^ 
hasBondWith(?y,?z) ^   hasBondWith(?z,?w) ^  hasBondWith(?w,?u) ^  
hasBondWith(?u,?v) ^ hasBondWith(?v,?x) --> 
  carbon_ring .  

 intron(i) ^ start(i,s) ^ end(i,e) <-- exon(x1) ^ exon(x2) ^ end(x1,s) ^ start(x2,e) ^ consec(x1,x2) ^ i=i(x1-x2)

 intron(i) ^ starts-at-end-of(i,x1) ^ ends-at-start-of(i,x2) <-- exon(x1) ^ exon(x2) ^ consec(x1,x2) ^ i=i(x1-x2)

 intron
 isect: s-a-e-of i-x1
 isect: e-a-s-of i-x2

 i-x1
 isect: exon
 isect: consec i-x2
 
 i-x2
 isect: exon
 isect: consec' i-x1

 gnra <--
 g(b1) ^ n(b2) ^ purine(b3) ^ a(b4) ^ dn(b2,b1) ^ dn (b3,b2) ^ dn(b4,b3) ^ wc(b4,b1)
 
*/

% translates to ontol_db goals
ruleatom_goals( Term, [inst_of(I,C)]):-
        Term =.. [C,I].
ruleatom_goals( Term, [inst_rel(A,R,B)]):-
        Term =.. [R,A,B].
        
rule_goals( (Term,Terms), Facts):-
        !,
        ruleatom_goals(Term, Facts1),
        rule_goals(Terms,Facts2),
        append(Facts1,Facts2,Facts).

rule_goals( Term, Facts):-
        ruleatom_goals(Term, Facts).

% translates to an ontol_db class expression
ruleatom_facts( Head, Term, [class(IC),
                             differentium(HeadClass,card(has_part,1,1),IC),
                             restriction(IC,part_of,HeadClass),
                             genus(IC,G)]):-
        Term =.. [G,I],
        Head =.. [HeadClass|_],
        reifvar_classname( Head, I, IC).
ruleatom_facts( Head, Term, [differentium(AC,R,BC)]):-
        Term =.. [R,A,B],
        reifvar_classname( Head, A, AC),
        reifvar_classname( Head, B, BC).
        
rule_facts(Head, (Term,Terms), Facts):-
        !,
        ruleatom_facts(Head, Term, Facts1),
        rule_facts(Head, Terms,Facts2),
        append(Facts1,Facts2,Facts).

rule_facts( Head, Term, Facts):-
        ruleatom_facts(Head, Term, Facts).

% variables in a rule are local
reifvar_classname( Head, Var, Class):-
        Head =.. [Functor|_],
        concat_atom([Functor,Var],'_',Class).

assertall(ontol_db,rules):-
        assertall.

assertall:-
        findall(ontol_db:Fact,
                (   logicalformula(_,A,prolog),
                    atom_to_term(A,(Head :- Body),_),
                    rule_facts(Head,Body,Facts1),
                    member(Fact,Facts1)),
                Facts),
        insert_facts(Facts).


%assertall:-
%        forall((logicalformula(_,A,prolog),atom_to_term(A,(Head :- Body),_),rule_facts(Head,Body,Facts)),
%               ontol_db:maplist(assert,Facts)).

testrule(carbon_ring,
        (   carbon(cx), carbon(cy), carbon(cz), carbon(cw), 
            carbon(cu), carbon(cv),  hasBondWith(cx,cy), 
            hasBondWith(cy,cz), hasBondWith(cz,cw),  hasBondWith(cw,cu),  
            hasBondWith(cu,cv), hasBondWith(cv,cx))).

t(X):-
        ontol_bridge_from_rules:testrule(Head,Body),
        rule_facts(Head,Body,X).


/** <module> 

  ---+ Synopsis

==
:- use_module(bio(ontol_bridge_from_rules)).

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

        