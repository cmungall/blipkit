
:- multifile metadata_nlp:synset_hook/1.

metadata_nlp:synset_hook([disorder,syndrome,disease]).
metadata_nlp:synset_hook(['',s]).  % e.g. Asperger's
metadata_nlp:synset_hook(['','\'']).  % e.g. Asperger's



