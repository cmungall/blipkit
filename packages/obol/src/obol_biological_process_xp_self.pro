:- [bio(obol_biological_process_xp)].

:- multifile process/3,process5/3,term_label/3.

% TOOD: make this more general: BP or MF
% bp x bp
process5(P that regulates(RegulatedProcess)) --> regulation(P),[of],process(RegulatedProcess).
process5(P that negatively_regulates(RegulatedProcess)) --> negative_regulation(P),[of],process(RegulatedProcess).
process5(P that positively_regulates(RegulatedProcess)) --> positive_regulation(P),[of],process(RegulatedProcess).
def(process(P that regulates(RP))) -->
  def(process(P)),
  ['that modulates the frequency, rate or extent of'],
  process(RP).
def(process(P that negatively_regulates(RP))) -->
  def(process(P)),
  ['that stops, prevents or reduces the frequency, rate or extent of'],
  process(RP).
def(process(P that positively_regulates(RP))) -->
  def(process(P)),
  ['that activates, maintains or increases the frequency, rate or extent of'],
  process(RP).


activation( P) --> [activation],{class_label_exact(P,'biological regulation')}.
process5(P that activates(RegulatedProcess)) --> activation(P),[of],process(RegulatedProcess).

% regulation of transcription during G1 phase of mitotic cell cycle
process5(P that regulates(RP) and part_of(W)) --> regulation(P),[of],process(RP),[during],process(W).



%  regulation of timing of organ formation
process5(P that regulates_timing_of(RegulatedProcess)) --> regulation(P),[of,timing,of],process(RegulatedProcess).
process5(P that negatively_regulates_timing_of(RegulatedProcess)) --> negative_regulation(P),[of,timing,of],process(RegulatedProcess).
process5(P that positively_regulates_timing_of(RegulatedProcess)) --> positive_regulation(P),[of,timing,of],process(RegulatedProcess).

% induction of X
process5(P that positively_regulates(RegulatedProcess)) --> induction(P),[of],process(RegulatedProcess).
%induction(P) --> any_kind_of(P,induction).
induction(P) --> [induction],{class_label_exact(P,'biological regulation')}.

positive_regulation_like(P) --> positive_regulation(P).
positive_regulation_like(P) --> induction(P).

% should these be differentiated? integral part of?
% during sometimes used when not a direct part (pheromone-dependent signal transduction during conjugation with cellular fusion)
process(P that part_of(SuperProcess)) --> process5(P),[involved,in],process(SuperProcess).
process(P that happens_during(SuperProcess)) --> process5(P),[during],process(SuperProcess).
process(P that part_of(SuperProcess)) --> process5(P),[of],process(SuperProcess).
% positive regulation of transcription on exit from mitosis
process(P that part_of(SuperProcess)) --> process5(P),[on],process(SuperProcess).
process(P that precedes(NextProcess)) --> process5(P),[ending,in],process(NextProcess).
process(P that part_of(SuperProcess)) --> process5(SuperProcess),[specific],process(P).

def(process(P that part_of(SuperProcess))) -->
    def(process(P)),
    ['during'],
    process(SuperProcess).
def(process(P that precedes(NextProcess))) -->
    def(process(P)),
    ['ending in'],
    process(NextProcess).

%% ----------------------------------------
%% multi-organism processes
%% ----------------------------------------

% TODO - use these
regulation_rel(P,regulates) --> regulation(P).
regulation_rel(P,negatively_regulates) --> negative_regulation(P).
regulation_rel(P,positively_regulates) --> positive_regulation(P).

% positive regulation of G-protein gamma subunit-mediated signal transduction in response to host
%process(P that RD and response_to(Role)) --> regulation_rel(P,Rel),[of],process(RP),[in],[response],[to],biological_role(Role),{RD=..[Rel,RP]}. TODO: fix this. Reverse?
process(P that regulates(RP) and response_to(Role)) --> regulation(P),[of],process(RP),[in],[response],[to],biological_role(Role).
process(P that negatively_regulates(RP) and response_to(Role)) --> negative_regulation(P),[of],process(RP),[in],[response],[to],biological_role(Role).
process(P that positively_regulates(RP) and response_to(Role)) --> positive_regulation(P),[of],process(RP),[in],[response],[to],biological_role(Role).


% patterns:
% P by R of C
% P by R of C in R2
% P of R during P3
% P by R of C during P3
% P by R of C during P3
% P by R of C in R2 during P3 - 

% P can be modulation or +/- regulation

defense_related(P) --> [defense],['-'],[related],{class_label_exact(P,'defense response')}.
% modulation by organism of defense-related ethylene-mediated signal transduction pathway in other organism during symbiotic interaction
process5(P that happens_during(W)) --> defense_related(W),process(P),{class_label_exact(W,'defense response')}.

% TODO: defense-related as a qualifier

% modulation BY host OF symbiont / phagocytosis
process5(P that regulated_by(Role) and regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        regulation(P),[by],biological_role(Role),[of],biological_role(ModulateeRole),process(ModulatedProcess).
process5(P that regulated_by(Role) and negatively_regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        negative_regulation(P),[by],biological_role(Role),[of],biological_role(ModulateeRole),process(ModulatedProcess).
process5(P that regulated_by(Role) and positively_regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        positive_regulation(P),[by],biological_role(Role),[of],biological_role(ModulateeRole),process(ModulatedProcess).

% (modulation OF phagocytosis IN other organism) during symbiotic interaction
% we really want ternary relations here; though binary form is logically equivalent
process5(P that regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        regulation(P),[of],process(ModulatedProcess),[in],biological_role(ModulateeRole).
process5(P that negatively_regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        negative_regulation(P),[of],process(ModulatedProcess),[in],biological_role(ModulateeRole).
process5(P that induces(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        induction(P),[of],process(ModulatedProcess),[in],biological_role(ModulateeRole).

% induction BY organism OF {defense-related} symbiont / cell wall thickening
% positive regulation BY symbiont OF {defense-related} (host) (cell wall thickening)
process5(P that regulated_by(Role) and induces(ModulatedProcess) and regulates_process_in(ModulateeRole) and happens_during(DP)) -->
        induction(P),[by],biological_role(Role),[of],defense_related(DP),biological_role(ModulateeRole),process(ModulatedProcess).

% (modulation BY organism OF apoptosis OF/IN other organism) during symbiotic interaction
process5(P that regulated_by(Role) and regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        regulation(P),[by],biological_role(Role),[of],process(ModulatedProcess),role_prep,biological_role(ModulateeRole).
process5(P that regulated_by(Role) and negatively_regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        negative_regulation(P),[by],biological_role(Role),[of],process(ModulatedProcess),role_prep,biological_role(ModulateeRole).
process5(P that regulated_by(Role) and induces(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        induction(P),[by],biological_role(Role),[of],process(ModulatedProcess),role_prep,biological_role(ModulateeRole).

% (autophagy of host) during interaction with symbiont
process(P that affects(Role)) --> process5(P),role_prep,biological_role(Role).
process(P that affects(Role) and part_of(W)) --> process5(P),role_prep,biological_role(Role),[during],process(W).

% cytolysis by host of symbiont cells
process(P that affects(Affected) and regulated_by(By)) --> other_cytolysis(P),[by],biological_role(By),[of],biological_role(Affected),[cells].

% regulation by host of cytolysis of symbiont cells
process(P that regulates(RP) and affects(Affected) and regulated_by(By)) -->
        regulation(P),[by],biological_role(By),[of],other_cytolysis(RP),[of],biological_role(Affected),[cells].
process(P that negatively_regulates(RP) and affects(Affected) and regulated_by(By)) -->
        negative_regulation(P),[by],biological_role(By),[of],other_cytolysis(RP),[of],biological_role(Affected),[cells].
process(P that positively_regulates(RP) and affects(Affected) and regulated_by(By)) -->
        positive_regulation(P),[by],biological_role(By),[of],other_cytolysis(RP),[of],biological_role(Affected),[cells].
process(P that induces(RP) and affects(Affected) and regulated_by(By)) -->
        induction(P),[by],biological_role(By),[of],other_cytolysis(RP),[of],biological_role(Affected),[cells].

other_cytolysis(P) --> [cytolysis],{class_label_exact(P,'cytolysis of cells of another organism')}.
other_cytolysis(P) --> [hemoysis],{class_label_exact(P,'hemolysis by organism of erythrocytes in other organism during symbiotic interaction')}.



role_prep --> [of].
role_prep --> [in].

% TODO: environment
%        is_a GO:0052120 ! positive aerotaxis in host environment
%         is_a GO:0052132 ! positive aerotaxis on or near host
%         is_a GO:0052133 ! positive aerotaxis within host
environmentd( occurs_in_proximity_of(Role)) --> [in],biological_role(Role),[environment].
environmentd( occurs_in_proximity_of(Role)) --> [in],[environment],[of],biological_role(Role).
environmentd( unfolds_on_or_near(Role)) --> [on],[or],[near],biological_role(Role).
environmentd( unfolds_on_or_near(Role)) --> [on],[or],[near],biological_role(Role),[surface]. % ?? required
environmentd( unfolds_within(Role)) --> [within],biological_role(Role).

%environment_process(P that occurs_in_proximity_of(Role)) --> process5(P),[in],biological_role(Role),[environment].
%environment_process(P that occurs_in_proximity_of(Role)) --> process5(P),[in],[environment],[of],biological_role(Role).
%environment_process(P that unfolds_on_or_near(Role)) --> process5(P),[on],[or],[near],biological_role(Role).
%environment_process(P that unfolds_on_or_near(Role)) --> process5(P),[on],[or],[near],biological_role(Role),[surface]. % ?? required
%environment_process(P that unfolds_within(Role)) --> process5(P),[within],biological_role(Role).

environment_process(P that D) --> process5(P),environmentd(D).

process(P) --> environment_process(P).
% positive aerotaxis in environment of other organism during symbiotic interaction
process(P and part_of(W)) --> environment_process(P),[during],process(W).

% autophagy of symbiont on or near host
process(P that D and affects(Role)) --> process5(P),[of],biological_role(Role),environmentd(D).

/*

  Note: here the symbiotic nature is made explicit:
  
 GO:0052192 ! movement in environment of other organism during symbiotic interaction
 GO:0052215 ! energy taxis in environment of other organism during symbiotic interaction
 GO:0052216 ! chemotaxis in environment of other organism during symbiotic interaction
 GO:0052217 ! aerotaxis in environment of other organism during symbiotic interaction
 GO:0052218 ! positive energy taxis in environment of other organism during symbiotic interaction
 GO:0052219 ! negative energy taxis in environment of other organism during symbiotic interaction
 GO:0052220 ! positive aerotaxis in environment of other organism during symbiotic interaction
 GO:0052221 ! positive chemotaxis in environment of other organism during symbiotic interaction
 GO:0052222 ! negative aerotaxis in environment of other organism during symbiotic interaction
 GO:0052223 ! negative chemotaxis in environment of other organism during symbiotic interaction
! No parse for: GO:0075044 autophagy of host during interaction with symbiont

 Here it is implicit:
  
 GO:0052143 ! chemotaxis on or near host  
  
  */


% todo: check; this is redundant w above
% regulation by X of Y
%process5(P that regulates(RegulatedProcess) and has_enabler(Role)) --> regulation(P),[by],biological_role(Role),[of],process(RegulatedProcess).
%process5(P that negatively_regulates(RegulatedProcess) and has_enabler(Role)) --> negative_regulation(P),[by],biological_role(Role),[of],process(RegulatedProcess).
%process5(P that positively_regulates(RegulatedProcess) and has_enabler(Role)) --> positive_regulation(P),[by],biological_role(Role),[of],process(RegulatedProcess).


% cytokinesis after mitosis
process(P that preceded_by(PrecedingProcess)) --> process5(P),[after],process(PrecedingProcess).

% phagocytosis, recognition
% note the order: in GO this normally means is_a recognition and part_of phagocytosis
%% process(P that part_of(SuperProcess)) --> process5(SuperProcess),[','],process(P),{parentRT(P,_,SuperProcess)}. # too constraining..
process(P that part_of(SuperProcess)) --> process5(SuperProcess),[','],process(P).

% interaction with other organism
process(P that part_of(SuperProcess)) --> process5(P),[with],process(SuperProcess).

% respiratory burst at fertilization
process(P that part_of(P2)) --> process5(P),[at],process(P2).

% active evasion of immune response of other organism via regulation of complement system of other organism during symbiotic interaction
% negative regulation by symbiont of entry into host cell via phagocytosis; protein maturation via proteolysis
process(P that regulated_by(SuperProcess)) --> process5(P),[via],process(SuperProcess).
process(P that regulated_by(SuperProcess)) --> process5(P),[','],[via],process(SuperProcess).

% embryonic cranial skeleton morphogenesis
process(P that part_of(SuperProcess)) --> [embryonic],process(P),{class_label_exact(SuperProcess,'embryonic development')}.

% post-embryonic foregut morphogenesis
%  The process by which the anatomical structures of the foregut are generated and organized, during the post-embryonic phase. Morphogenesis pertains to the creation of form.
process(P that part_of(SuperProcess)) --> [post],['-'],[embryonic],process(P),{class_label_exact(SuperProcess,'post-embryonic development')}.

% meiosis I nuclear envelope disassembly
process(P that part_of(W)) --> process5(W),process5(P).
