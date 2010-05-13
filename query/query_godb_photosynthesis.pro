qprocess(P):- entity_label(P,'photosynthesis, light reaction').
qprocess(P):- entity_label(PP,'photosynthetic electron transport chain'),parentRT(P,PP).

