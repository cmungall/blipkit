:- rdf_register_ns(biopax2,'http://www.biopax.org/release/biopax-level2.owl#').

/************************************************************
  Biopax2
************************************************************/

biopax_name(ID,Name):- rdf_has(ID,biopax2:'NAME',Name).
biopax_synonyms(ID,Syn):- rdf_has(ID,biopax2:'SYNONYMS',Syn).
biopax_left(ID,X):- rdf_has(ID,biopax2:'LEFT',X).
biopax_right(ID,X):- rdf_has(ID,biopax2:'RIGHT',X).
biopax_controller(ID,X):- rdf_has(ID,biopax2:'CONTROLLER',X).
biopax_controlled(ID,X):- rdf_has(ID,biopax2:'CONTROLLED',X).
biopax_stoichiometric_coefficient(ID,X):- rdf_has(ID,biopax2:'STOICHIOMETRIC-COEFFICIENT',X).
biopax_cellular_location(ID,X):- rdf_has(ID,biopax2:'CELLULAR-LOCATION',X).
biopax_physical_entity(ID,X):- rdf_has(ID,biopax2:'PHYSICAL-ENTITY',X).
biopax_term(ID,X):- rdf_has(ID,biopax2:'TERM',X).

Biopax a biopax2:'entity' :: label ==>
  if(biopax_name(Biopax,Name),then:Name,else:Biopax).

Biopax a biopax2:'entity' :: summary ==>
  Biopax :: detail.

Event a biopax2:'conversion' :: detail ==>
    div(class=biopax_reaction,
        table(border=1,
              tr(td(colspan=3,Event :: link)),
              tr(td(colspan=3,Name :: link)) forall biopax_synonyms(Event,Name),
              tr(td(div(X :: summary) forall biopax_left(Event,X)),
                 td(div(X :: summary) forall biopax_right(Event,X))))).
                
%Pathway a biopax2:'xpathway' :: attached_detail ==>
%  foo.

Event a biopax2:'modulation' :: attached_detail ==>
    div(class=biopax_reaction,
        table(border=1,
              tr(td(colspan=3,Event :: link)),
              tr(td(colspan=3,Name :: link)) forall biopax_synonyms(Event,Name),
              tr(td(div(X :: summary) forall biopax_controller(Event,X))),
              tr(td(div(X :: summary) forall biopax_controlled(Event,X))))).
                
PEP a biopax2:'physicalEntityParticipant' :: link ==>
 zoom('*',PEP).

PEP a biopax2:'physicalEntityParticipant' :: summary ==>
 PEP :: link,
 span(Entity :: link) forall biopax_physical_entity(PEP,Entity),
 span('<', Loc :: link ,'>') forall biopax_cellular_location(PEP,Loc),   
 span(font(size = -2,Co :: link)) forall biopax_stoichiometric_coefficient(PEP,Co).

PEP a biopax2:'physicalEntityParticipant' :: attached_detail ==>
  table(tr(td(left),td(X :: detail) forall biopax_left(X,PEP)),
        tr(td(right),td(X :: detail) forall biopax_right(X,PEP)),
        tr(td(controller),td(X :: detail) forall biopax_controller(X,PEP))).
 
OCV a biopax2:openControlledVocabulary :: label ==>
  span(Term :: link) forall biopax_term(OCV,Term).
