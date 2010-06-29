:- [bio(obol_biological_process_xp_anatomy)].
:- [bio(obol_biological_process_xp_self)].

:- multifile process/3,process5/3,anatomical_continuant/3.
:- multifile def/3.

process5(P) --> symbiont_modification(P).

% modification by symbiont of host chloroplast
symbiont_modification(Mod that has_agent(AgentRole) and has_patient(PatientRole) and modifies(C)) --> [modification],[by],biological_role(AgentRole),[of],biological_role(PatientRole),anatomical_continuant(C),{class_label_exact(Mod,'modification of symbiont morphology or physiology')}.

% GO:0052417 ! metabolism by host of symbiont protein
%%% CIRCULAR symbiont_modification(Mod that has_agent(AgentRole) and has_patient(PatientRole) and modifies(C) and modification_process(P)) --> process(P),[by],biological_role(AgentRole),[of],biological_role(PatientRole),anatomical_continuant(C),{class_label_exact(Mod,'modification of symbiont morphology or physiology')}.

