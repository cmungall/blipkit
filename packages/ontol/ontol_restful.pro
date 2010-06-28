:- module(ontol_restful,
          [
           ont_idspace/2,
           id_url/2,
           id_url/3,
           id_imgurl/2,
           id_params_url/3,
           id_params_url/4,
           params_hidden/2,
           params_drels_crels/3,
	   knowsabout/1,
           id_wikipage/2,
           parse_id_idspace/2,
           parse_id_idspace/3,
           id_exturl/2,
           ontol_page/2
          ]).

:- use_module(bio(serval)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_lookup)).
:- use_module(bio(ontol_segmenter)).
:- use_module(bio(ontol_writer_dot)).
:- use_module(bio(metadata_db)).
:- use_module(bio(io)).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(blipkit_ontol)).
:- use_module(bio(safe_interpreter),[safe/1]).

idspace_confmod('PATO',ontol_config_pato).
idspace_confmod('FMA',ontol_config_fma).
idspace_confmod('MA',ontol_config_ma).
idspace_confmod('SO',ontol_config_so).
idspace_confmod('GAZ',ontol_config_gaz).
idspace_confmod('UBERON',ontol_config_uberon).
idspace_confmod('FBdv',ontol_config_fbdv).
idspace_confmod('ZFA',ontol_config_zfa).
idspace_confmod('XAO',ontol_config_xao).
idspace_confmod('NCBITaxon',ontol_config_ncbi_taxonomy).
idspace_confmod('NIF_GrossAnatomy',ontol_config_nif).


% maps an ontology (e.g. biological_process) onto an IDspace (e.g. GO) via
% settings in ontology metadata file
ont_idspace(Ont,IDSpace):- inst_sv(Ont,namespace,IDSpace,_).

% chebi images
% http://www.ebi.ac.uk/chebi/displayImage.do?defaultImage=true&imageIndex=0&chebiId=16977
id_imgurl(ID,URL) :-
        parse_id_idspace(ID,'CHEBI'),
        id_localid(ID,Num),
        atom_concat('http://www.ebi.ac.uk/chebi/displayImage.do?defaultImage=true&imageIndex=0&chebiId=',Num,URL).

ont_refs(Ont,Ont2):-
        inst_sv(Ont,xrefs_to,A),
        concat_atom(L,',',A),
        member(Ont2,L).
ont_refs(Ont,Ont2):-
        inst_sv(Ont,uses,Ont2,_).
ont_refs(Ont,Ont2):-
        inst_sv(Ont,extends,Ont2,_).

%% idspace_url_format(+IDSpace,?URL,?Fmt)
% E.g. CL --> http://purl.org/obo/obo/CL.obo, obo
% TODO: remove hardcoding
idspace_url_format('NIF_Subcellular',URL,Fmt) :- idspace_url_format('PKB',URL,Fmt).
idspace_url_format('NIF_Investigation',URL,Fmt) :- idspace_url_format('PKB',URL,Fmt).
idspace_url_format(snap,URL,Fmt) :- idspace_url_format(bfo,URL,Fmt).
idspace_url_format(span,URL,Fmt) :- idspace_url_format(bfo,URL,Fmt).
idspace_url_format(ncithesaurus,URL,Fmt) :- idspace_url_format('NCIt',URL,Fmt). % tmp
idspace_url_format(IDSpace,URL,obo) :-
        sformat(URL,'http://purl.org/obo/obo/~w.obo',[IDSpace]).

idspace_refs(S,S2):-
        ont_idspace(Ont,S),
        ont_refs(Ont,Ont2),
        ont_idspace(Ont2,S2).

listatom_ids(IDListAtom,IDs):-
        atom(IDListAtom),
	concat_atom(IDs,'+',IDListAtom),
	IDs = [_,_|_].

% lists
id_url(Xs,Fmt,URL):- is_list(Xs),!,concat_atom(Xs,'+',X),id_url(X,Fmt,URL).
id_url(Xs,URL):- is_list(Xs),!,concat_atom(Xs,'+',X),id_url(X,URL).
id_url(X/T,URL):- !,id_url(X,URL1),concat_atom([URL1,T],'/',URL).

id_url(X,X):- atom_concat('http:',_,X),!.
id_url(X,URL):- sformat(URL,'/obo/~w',[X]).
id_url(X,Fmt,URL):- sformat(URL,'/obo/~w.~w',[X,Fmt]).

% adds hidden params + additional import
id_params_url(X,Params,FullURL,Context) :-
        id_url(X,URL),
        params_hidden([import=Context|Params],Extra), 
        sformat(FullURL,'~w?~w',[URL,Extra]). % TODO -- what if we have params already?

% adds hidden params
id_params_url(X,Params,FullURL) :-
        id_url(X,URL),
        params_hidden(Params,Extra), 
        sformat(FullURL,'~w?~w',[URL,Extra]). % TODO -- what if we have params already?

params_hidden(Params,Extra) :-
        findall(PA,(member(import=Ont,Params),sformat(PA,'import=~w',[Ont])),PAs),
        concat_atom(PAs,'&',Extra).

knowsabout(ID) :-
	parse_id_idspace(ID,S,_),
	\+ \+ inst_sv(_,namespace,S,_).
	       


id_wikipage(ID,Page):- def_xref(ID,X),xref_wikipage(X,Page).
id_wikipage(ID,Page):- entity_xref(ID,X),xref_wikipage(X,Page).
xref_wikipage(X,Page):- parse_id_idspace(X,'Wikipedia',Page),\+ sub_atom(Page,0,_,_,'http:').
xref_wikipage(X,Page):- parse_id_idspace(X,'Wikipedia',URL),sub_atom(URL,0,_,_,'http:'),xref_wikipage(URL,Page).
xref_wikipage(X,Page):- parse_id_idspace(X,url,URL),sub_atom(URL,0,_,_,'http:'),xref_wikipage(URL,Page).
xref_wikipage(X,Page):- atom_concat('http://en.wikipedia.org/wiki/',Page,X).

parse_id_idspace(ID,S):-
        parse_id_idspace(ID,S,_).
parse_id_idspace(ID,S,Local):-
        !,
        concat_atom([S|LocalParts],':',ID),
        LocalParts\=[],
        concat_atom(LocalParts,':',Local).
parse_id_idspace(ID,obo,ID).

id_exturl(ID,URL):-
        parse_id_idspace(ID,S,Local),
        inst_sv(S,'GOMetaModel:url_syntax',Ex,_),
        atom_concat(Base,'[example_id]',Ex),
        atom_concat(Base,Local,URL).

% display relations and containment relations from parameters
params_drels_crels(Params,AllRels,CRels) :-
        params_drels_crels1(Params,Rels,CRels),
        (   Rels=[all]
        ->  AllRels=[]
        ;   AllRels=[subclass|Rels]).

params_drels_crels1(Params,AllRels,CRels) :-
        \+ member(rel=_,Params),
        \+ member(cr=_,Params),
        setof(R,user:graphviz_ontol_param(display_relation(Ont),R),AllRels),
        setof(R,user:graphviz_ontol_param(containment_relation(Ont),R),CRels),
        debug(ontol_rest,' Using display params for ~w : ~w & ~w',[Ont,AllRels,CRels]),
	% avoid combining multiple display relations from multiple configs
        \+ ((user:graphviz_ontol_param(display_relation(Ont2),_),Ont2\=Ont)),
        \+ ((user:graphviz_ontol_param(containment_relation(Ont2),_),Ont2\=Ont)),
        !.

params_drels_crels1(Params,AllRels,CRels) :-
        findall(Rel,member(rel=Rel,Params),AllRels),
        findall(Rel,member(cr=Rel,Params),CRels).


%% searchterm_entities(S,L) is semidet
searchterm_entities(S,L):-
        sformat(S1,'%~w%',[S]),
        setof(X,lookup_class(search(S1),X),L).

emit_content_type(CT):-
        format('Content-type: ~w~n~n', [CT]).

preload(ID,Params):-
        concat_atom([IDSpace,_LocalID],':',ID),
        preload_ont(IDSpace,Params).

preload_revlinks(ID,Params):-
        load_bioresource(obo_meta_xp),
        concat_atom([IDSpace,_LocalID],':',ID),
        debug(ontol_rest,'Fetching all onts that link to ~w',[IDSpace]),
        solutions(X,
                  idspace_refs(X,IDSpace),
                  Xs),
        debug(ontol_rest,'  All: ~w',[Xs]),
        forall(member(X,Xs),
               preload_ont(X,Params)).

%% preload_ont(+IDSpace,+Params)
% 
% IDSpace is an ontology IDSpace such as GO, CL, etc.
preload_ont(IDSpace,Params):-   % ont is NOT specified on param list
        \+ member(ont=_,Params),
        !,
        % some ontologies trigger the loading of species configurations
        % for graph drawing. we only do this in situations where a single
        % ontology is loaded. if there is an import, then we only go with the
        % default configuration
        (   \+ member(import=_,Params)
        ->  forall(idspace_confmod(IDSpace,Mod),
                   consult(bio(Mod)))
        ;   true),
        % TODO: check if this is registered
        debug(ontol_rest,'Loading IDSpace ~w',[IDSpace]),
        % E.g. CL --> http://purl.org/obo/obo/CL
        idspace_url_format(IDSpace,URL,Fmt),
        % make sure the import chain is followed (if any)
        catch((load_biofile(Fmt,url(URL)),
               ontol_db:import_all_ontologies),
              _,
              true).
preload_ont(_IDSpace,Params):-  % ont IS specified on param list; ignore IDSpace
        setof(Ont,member(ont=Ont,Params),Onts),
        debug(ontol_rest,'Loading ~w',[Onts]),
        maplist(load_bioresource,Onts),
        debug(ontol_rest,'Loaded ~w',[Onts]),
        ontol_db:import_all_ontologies.

%% preload_ont_plus_dependencies(+IDSpace,+Params)
% as preload_ont/2, but in addition will load other
% IDSpaces based on idspace_refs/2
preload_ont_plus_dependencies(IDSpace,Params):-
        %load_bioresource(obo_meta),
        load_bioresource(obo_meta_xp),
        preload_ont(IDSpace,Params),
        solutions(X,
                  idspace_refs(IDSpace,X),
                  Xs),
        debug(ontol_rest,'  All: ~w',[Xs]),
        forall(member(X,Xs),
               preload_ont(X,Params)).

% load all imported ontologies
ontol_page(Path,Params) :-
        forall(member(import=Ont,Params),
               preload_ont_plus_dependencies(Ont,Params)),
        ontol_page_actual(Path,Params).

        
ontol_page_actual(Path,Params):-
        \+ is_list(Path),
        !,
        concat_atom(PathElts,'/',Path),
        ontol_page_actual(PathElts,Params).

ontol_page_actual([],Params):-
        %load_bioresource(obo_meta),
        emit_content_type('text/html'),
        emit_page(entry_page,Params).

% remove trailing slash
ontol_page_actual(L,Params):-
        append(L1,[''],L),
        !,
        ontol_page_actual(L1,Params).

ontol_page_actual([mappings],Params):-
        load_bioresource(obo_meta_xp),
        emit_content_type('text/html'),
        emit_page(mappings_entry_page,Params).

ontol_page_actual([''],Params):-
        ontol_page_actual([],Params).

ontol_page_actual([help],Params):-
        emit_content_type('text/html'),
        emit_page(help_page,Params).

ontol_page_actual([Last],Params):-
        concat_atom([ID,Fmt],'.',Last),
        !,
        debug(ontol_rest,'id=~q fmt=~w',[ID,Fmt]),
        ontol_page_actual([Fmt,ID],Params).

ontol_page_actual([S],Params):-
        concat_atom([_],':',S),
        !,
        ontol_page_actual([ontology,S],Params).

ontol_page_actual([IDListAtom],Params):-
	listatom_ids(IDListAtom,IDs),
	!,
	forall(member(ID,IDs),
	       preload(ID,Params)),
	emit_content_type('text/html'),
	emit_page(multiple(IDs),Params).
        	
ontol_page_actual([ID],Params):-
        preload(ID,Params),
        (   soft_redirect(ID,ID2)
        ->  true
        ;   ID2=ID),
        (   redirect(ID2,Type,URL)
        ->  format('HTTP/1.1 ~w~nStatus: ~w~nLocation: ~w~n~n',[Type,Type,URL])
        ;   emit_content_type('text/html'),
            emit_page(basic(ID2),Params)).
        
ontol_page_actual([''|L],Params):-
        !,
        ontol_page_actual(L,Params).

ontol_page_actual([png,IDListAtom],Params):-
	listatom_ids(IDListAtom,IDs),
	!,
        ontol_page_actual([png,IDs],Params).

ontol_page_actual([png,IDs],Params):-
        is_list(IDs),
	!,
        debug(ontol_rest,' multiple IDs (will cluster)=~w',[IDs]),
        emit_content_type('image/png'),
        % treat xrefs like normal relations - useful for comparing two ontologies side-by-side
	ensure_loaded(bio(ontol_manifest_relation_from_xref)),
	forall(member(ID,IDs),
	       preload(ID,Params)),
        params_drels_crels(Params,AllRels,CRels),
        % TODO -- better way of handling xrefs
        (   AllRels=[]
        ->  AllRelsActual=[]
        ;   AllRelsActual=[xref|AllRels]),
        debug(ontol_rest,'   Display=~w Contain=~w DisplayActual=~w',[AllRels,CRels,AllRelsActual]),
        Opts=[cluster_pred(belongs(X,Ontol),X,Ontol), % we may have multiple ontologies
	      collapse_predicate(fail),
	      relations(AllRelsActual),containment_relations(CRels)],
        ontology_segment(IDs,Edges,_OutNodes,Opts),
        debug(ontol_rest,' Edges=~w',[Edges]),
        write_edges_to_image(Edges,Opts).

ontol_page_actual([png,ID],Params):-
        \+ is_list(ID),
        member(import=_,Params),
        !,
        ontol_page_actual([png,[ID]],Params).

ontol_page_actual([png,ID],Params):-
        emit_content_type('image/png'),
        preload(ID,Params),
        debug(ontol_rest,' Params=~w',[Params]),
        params_drels_crels(Params,AllRels,CRels),
        debug(ontol_rest,'   Display=~w Contain=~w',[AllRels,CRels]),
        Opts=[collapse_predicate(fail),relations(AllRels),containment_relations(CRels)],
        debug(ontol_rest,' Opts=~w',[Opts]),
        ontology_segment([ID],Edges,_OutNodes,Opts),
        debug(ontol_rest,' Edges=~w',[Edges]),
        write_edges_to_image(Edges,Opts).

ontol_page_actual([obo,ID],Params):-
        emit_content_type('text/plain'),
        write_obo(ID,Params).

ontol_page_actual([pro,ID],Params):-
        emit_content_type('text/plain'),
        write_fmt(ID,pro,Params).

ontol_page_actual([json,ID],Params):-
        emit_content_type('text/plain'),
        write_fmt(ID,jsontree,Params).

ontol_page_actual([Fmt,ID],Params):-
        fmt_ct_exec(Fmt,CT,Exec),
        emit_content_type(CT),
        tmp_file(owl,F),
        tell(F),
        write_obo(ID,Params),
        told,
        sformat(Cmd,'~w ~w',[Exec,F]),
        debug(ontol_rest,'executing: ~w',[Cmd]),
        shell(Cmd).

ontol_page_actual([revlinks,ID],Params):-
        debug(ontol_rest,'revlinks ~w',[ID]),
        emit_content_type('text/html'),
        preload(ID,Params),
        preload_revlinks(ID,Params),
        emit_page(what_links_here_table(ID),Params).

ontol_page_actual([ontology,ID],Params):-
        emit_content_type('text/html'),
        preload_ont(ID,Params),
        (   member(search_term=S,Params)
        ->  (   searchterm_entities(S,L)
            ->  emit_page(ontology_filtered(ID,S,L),Params)
            ;   emit_page(noresults(ID,S),Params))
        ;   emit_page(ontology(ID),Params)).

ontol_page_actual([ontology_table,ID],Params):-
        emit_content_type('text/html'),
        preload_ont(ID,Params),
        emit_page(ontology_table(ID),Params).

ontol_page_actual([query,Ont],Params):-
        debug(ontol_rest,' params=~w',[Params]),
        (   member(query=QA,Params)
        ->  true
        ;   QA=true),
        debug(ontol_rest,' QA=~w',[QA]),
        (   member(select=SA,Params),SA\=''
        ->  true
        ;   SA=QA),
        debug(ontol_rest,' SA=~w',[SA]),
        ontol_page_actual([query,Ont,QA,SA],Params).
ontol_page_actual([query,Ont,QueryAtom,SelectAtom],Params):-
        emit_content_type('text/html'),
        debug(ontol_rest,'QueryAtom=~w',[QueryAtom]),
        (   listatom_ids(Ont,Onts)
        ->  forall(member(O1,Onts),preload_ont(O1,Params))
        ;   preload_ont(Ont,Params)),
        sformat(QSA,'all((~w),(~w))',[QueryAtom,SelectAtom]),
        debug(ontol_rest,'QSA=~w',[QSA]),
        catch(atom_to_term(QSA,all(Query,Select),_Bindings),_,(Query=true,Select=true)),
        debug(ontol_rest,'QSA=~w',[Query,Select]),
        %catch(atom_to_term(QueryAtom,Query,_Bindings),_,Query=true),
        %catch(atom_to_term(SelectAtom,Select,_Bindings),_,Select=true),
        debug(ontol_rest,'Query=~w',[Query]),
        (   safe(Query)
        ->  (   catch(findall(Select,Query,Results),E,fail)
            ->  emit_page(ontology_query(Ont,QueryAtom,Results),Params)
            ;   emit_page(ontology_query(Ont,QueryAtom,[query_failed(E)]),Params))
        ;   emit_page(ontology_query(Ont,QueryAtom,[unsafe_query(Query)]),Params)).


ontol_page_actual([metadata,ID],Params):-
        emit_content_type('text/html'),
        preload_ont(ID,Params),
        %load_bioresource(obo_meta),
        load_bioresource(obo_meta_xp),
        emit_page(ontology_metadata(ID),Params).

ontol_page_actual([xps,S],Params):-
        emit_content_type('text/html'),
        preload_ont_plus_dependencies(S,Params),
        emit_page(xps(S),Params).

ontol_page_actual([statements,S],Params):-
        emit_content_type('text/html'),
        preload_ont(S,Params),
        %preload_ont_plus_dependencies(S,Params),
        emit_page(ontology_statements(S),Params).

ontol_page_actual([relationships,Ont,R],Params):-
        emit_content_type('text/html'),
        preload_ont(Ont,Params),
        emit_page(ontology_relationships(Ont,R),Params).


write_obo(ID,Params):-
        write_fmt(ID,obo,Params).

write_fmt(IDListAtom,Fmt,Params):-
	listatom_ids(IDListAtom,IDs),
	!,
	forall(member(ID,IDs),
	       preload(ID,Params)),
        (   member(rel=all,Params)
        ->  Opts=[relations([])]
        ;   findall(Rel,member(rel=Rel,Params),Rels),
            Opts=[relations([subclass|Rels])]),
        debug(ontol_rest,'Opts=',[Opts]),
        ontology_segment(IDs,Edges,_,Opts),
        blipkit_ontol:show_ontol_subset_by_edges(Fmt,Edges,Opts).

write_fmt(ID,Fmt,Params):-
        preload(ID,Params),
        (   member(rel=all,Params)
        ->  Opts=[relations([])]
        ;   findall(Rel,member(rel=Rel,Params),Rels),
            Opts=[relations([subclass|Rels])]),
        debug(ontol_rest,'Opts=',[Opts]),
        ontology_segment([ID],Edges,_,Opts),
        blipkit_ontol:show_ontol_subset_by_edges(Fmt,Edges,Opts).


emit_page(Page,Params):-
        user:consult(bio(ontol_webpages)),
        % X=xml([]),
        write_sterm(Params,Page).

load_mods:-
        serval:ensure_loaded(bio(metadata_db)),
        serval:ensure_loaded(bio(ontol_db)).

soft_redirect(From,To):-
        \+ class(From),
        parse_id_idspace(From,_,Name),
        format(user_error,'Q: ~w~n',[Name]),
        entity_label(To,Name).


redirect(ID,'301 Moved Permanently',URL):-
        entity_obsolete(ID,_),
        (   entity_replaced_by(ID,X)
        ;   entity_consider(ID,X)),
        id_url(X,URL).

fmt_ct_exec(owl,'text/xml',go2owl).
fmt_ct_exec(owl2,'text/xml','blip.local io-convert -to owl2 -f obo -u ontol_bridge_to_owl2_and_iao -i').
fmt_ct_exec(obox,'text/xml',go2xml).
fmt_ct_exec(chado,'text/xml',go2chadoxml).


/** <module> 

  ---+ Synopsis

==
:- use_module(bio(ontol_restful)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
