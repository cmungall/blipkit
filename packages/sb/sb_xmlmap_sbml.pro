/* -*- Mode: Prolog -*- */



:- module(sb_xmlmap_sbml,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(sbml,XML,PL):-
        apply_xmlpred(sb_xmlmap_sbml,XML,PL).

xmlpred(sbml,_,[],translate(model)).
xmlpred(model,_,model(ID,N),
        [let(ID=att(id)),
         let(N=att(name)),
         prolog(M=in(ID)),
         translate([listOfRules,assignmentRule],M),
         translate([listOfCompartments,compartment],M),
         translate([listOfSpecies,species],M),
         translate([listOfReactions,reaction],M),
         translate([listOfParameters,parameter],M),
         translate([listOfUnitDefinitions,unitDefinition],M),
         translate(annotation,M)]).
xmlpred(annotation,M,[],
        [translate(['RDF','Description','relation','Bag',li],in_annotation(M)),
         translate(['RDF','Description','is','Bag',li],in_annotation(M)),
         translate(['RDF','Description','isVersionOf','Bag',li],in_annotation(M))
         ]).
xmlpred(li,in_annotation(in(ID)),annotation(ID,ResourceID,NS,LocalID),
        [
         let(ResourceID=att(_:resource)),
         prolog(debug(sbml,'in_annotation(~w) res=~w',[ID,ResourceID])),
         prolog(extract_miriam_id(ResourceID,NS,LocalID))
        ]).
xmlpred(unitDefinition,in(_MID),unitdef(ID,N),
        [let(ID=att(id),N=att(name)),
         translate([listOfUnits,unit],in(ID))]).
xmlpred(unit,in(ID),unit(ID,Exp,Kind,Scale),
        let(Exp=att(exponent),Kind=att(kind),Scale=att(scale))).
xmlpred(assignmentRule,in(_),assignment_rule(VarID,MathTerm),
        [let(VarID=att(variable)),
         translate(math,out(MathTerm))]).
xmlpred(kineticLaw,in(ID),kinetic_law(ID,MathTerm),
        [translate(math,out(MathTerm))
        %translate([listOfParameters,parameter],ID)
        ]).

xmlpred(math,out(MathTerm),[],
        [node(Node),
         prolog(Node=element(_,_,[_])),
         prolog(sb_xmlmap_sbml:mathml_term(Node,MathTerm))]).
xmlpred(compartment,in(_MID),compartment(ID,N),
        [let(ID=att(id),N=att(name)),
         translate(annotation,in(ID)),
         translate(att('*'),in(compartment,ID))]).
xmlpred(species,in(_MID),species(ID,N,CID),
        [let(ID=att(id),N=att(name),CID=att(compartment)),
         translate(annotation,in(ID)),
         translate(att('*'),in(species,ID))]).
xmlpred(parameter,in(_MID),parameter(ID,MetaID,Name,NumericValue),
        [let(ID=att(id),
             MetaID=att(metaid),
             Name=att(name),
             Value=att(value)),
	 prolog(atom_number(Value,NumericValue))]).
xmlpred(reaction,in(_M),reaction(ID,N),
        [let(ID=att(id),N=att(name)),
         translate([listOfProducts,speciesReference],in(reaction_product,ID)),
         translate([listOfReactants,speciesReference],in(reaction_reactant,ID)),
         translate([listOfModifier,speciesReference],in(reaction_modifier,ID)),
         translate(kineticLaw,in(ID)),
         translate(annotation,in(ID)),
         translate(att('*'),in(reaction,ID))]).
xmlpred(att('*'),in(_Type,ID),sbentity_param(ID,P,V2),
        [let(P=name('.'),V='.'),
	 prolog( catch(atom_number(V,V2),_,V2=V))]).
xmlpred(speciesReference,in(Relation,ID),[Pred],
        [let(SID=att(species)),
         let(_Stoi=att(stoichiometry)),
         prolog(debug(sbml,'in(~w,~w,~w)',[Relation,ID,SID])),
         prolog(Pred =.. [Relation,ID,SID]),
         prolog(debug(sbml,'Done ~w',[Pred]))]).
xmlpred(notes,in(ID),notes(ID,El),let(El='.')).

% MATHML
% (+DOMNode,?MathTerm)
mathml_term(element(_:math,_,[Node]),Term):-
        mathml_term(Node,Term).
%mathml_term(element(_:ci,_,[X]),ci(X)):- !.
mathml_term(element(_:ci,_,[X]),X):- !.
mathml_term(element(_:cn,_TypeAtts,[X]),Num):- atom_number(X,Num),!.
mathml_term(element(_:csymbol,_,[X]),X):- !.
mathml_term(element(_:apply,_,[element(_:Func,_,_)|ArgNodes]),MathTerm):-
        maplist(mathml_term,ArgNodes,ArgTerms),
        mathml_func(Func,FuncPred),
        MathTerm =.. [FuncPred,ArgTerms],
        !.
mathml_term(element(_:piecewise,_,ArgNodes),piecewise(ArgTerms)):-
        !,
        maplist(mathml_term,ArgNodes,ArgTerms).
mathml_term(element(_:piece,_,ArgNodes),piece(ArgTerms)):-
        !,
        maplist(mathml_term,ArgNodes,ArgTerms).
mathml_term(element(_:otherwise,_,ArgNodes),otherwise(ArgTerms)):-
        !,
        maplist(mathml_term,ArgNodes,ArgTerms).
mathml_term(Node,_):-
        throw(error(cannot_transform_mathml(Node))).
mathml_func(divide,/).
mathml_func(times,*).
mathml_func(minus,-).
mathml_func(plus,+).
mathml_func(power,**).
mathml_func(X,X).


extract_miriam_id(URI,NS,LocalID):-
        concat_atom([NS,LocalID],'#',URI).
extract_miriam_id(URI,NS,LocalID):-
        url_iri(URI,IRI),
        concat_atom(Parts,':',IRI),
        reverse(Parts,[LocalID,NS|_]).

        

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(bm18)=
      load_biofile(sbml,'BIOMD0000000018.xml')/[]).

unittest(test(load_file,
            [_=load(bm18)],
            (   ensure_loaded(bio(sb_db)),
                setof(ID,N^reaction(ID,N),IDs),
                length(IDs,NumIDs)),
             (NumIDs=47))).

unittest(test(mathml,
            [_=load(bm18)],
            (   ensure_loaded(bio(sb_db)),
                ID='MTX5deg',
                format('Finding kinetic law for ~w~n',[ID]),
                kinetic_law(ID,MathTerm)),
            MathTerm=times([_,_,_]))).

        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2006/01/14 22:23:02 $
  @license LGPL


  ==
  :- use_module(bio(sb_xmlmap_sbml)).
  ==

  bridging layer from sbml-xml to native sb model

  you should not need this module directly - handled by module io
  
  */