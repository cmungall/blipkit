amigo_component(amigo_xp).
%amigo_component(amigo_feature).
bioresource(go).
bioresource(dl_go).
bioresource(test_llm).
bioresource(cell).
bioresource(fly_development).
bioresource(relationship).
bioresource(obol_relations).
%bioresource(tair_ga_xp).
%bioresource(fly_ga_xp).

% germ cell differentiation <= not in GO
% should fetch instances of oocyte differentiation
bench_call((class(GenusID,differentiation),
            class(CellID,'germ cell'),
            CellClass=intersection(CellID,[]),
            QueryClass=intersection(GenusID,[affects:CellClass]),            
            class(ClassID,'oocyte differentiation'),
            cdef(ClassID,Class),
            cdef(IDCheck,Class),
            writeln(IDCheck),
            %classdef_subsumes_id(QueryClass,ClassID),
	    subclassXT(QueryClass,ClassID),
            writeln(1)),
           []).
bench_call((class(GenusID,differentiation),
            class(CellID,'germ cell'),
            CellClass=intersection(CellID,[]),
            QueryClass=intersection(GenusID,[affects:CellClass]),            
            setof(ClassID,
                  (   class(ClassID,N),
                      writeln(N),
                      %writeln(Class),
                      %(N='zhigh affinity zinc uptake transporter activity',trace;true),
		      subclassXT(QueryClass,ClassID),
                      %classdef_subsumes_id(QueryClass,ClassID),
                      writeln(ClassID/N)),ClassIDs)),
           ClassIDs).
