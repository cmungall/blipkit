
% TLX recruits histone deacetylases to repress transcription of p21(CIP1/WAF1) and pten and regulate neural stem cell proliferation
% (TLX recruits histone deacetylases) to ((repress transcription of (p21(CIP1/WAF1) and pten) and (regulate neural stem cell proliferation)))

% TLX recruits histone deacetylases
action(P and has_agent(Agent) and has_target(Target)) --> agent(Agent),verb_process(P),target(Target).

% (regulating VEGF expression)
action(P and has_target(Target)) --> verb_process(P),target(Target).

% (TLX recruits histone deacetylases) to ((repress transcription of (p21(CIP1/WAF1) and pten) and (regulate neural stem cell proliferation)))
rif(Pre and followed_by(Post)) --> action(Pre),[to],action(Post).

% (PTEN modulates (angiogenesis in prostate cancer)) by (regulating VEGF expression)
% PTEN is the agent in the post, so we can transfer this to the pre
rif(Pre and followed_by(Post)) --> action(Post),[by],action(Pre).

% PTEN is a [crucial] (mediator of (mitochondria-dependent apoptosis))





