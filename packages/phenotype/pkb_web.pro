/* -*- Mode: Prolog -*- */

:- module(pkb_web,
          [
           start_server/0,
           start_server/1
          ]).

:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_reasoner)).
:- use_module(library(thea2/owl2_graph_reasoner)).
:- use_module(pkb_db).
:- use_module(phenoblast_writer_dot).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(tabling),[table_pred/1]).
:- use_module(bio(genome_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(index_util)).
:- use_module(bio(homol_db)).
:- use_module(bio(dotwriter)).

% pkb uses generic phenotype model
% TODO - retire
:- use_module(phenotype_db,
              [
               phenotype_subsumed_by/2, % required for queries
               atomic_subsumed_by_lca/3
               %phenotype_frequency/2
              ]).

% the following manifests subclass/2 and restriction/2 from Thea, required for phenotype subsumption tests.
% it is assumed you are using Thea/OWL for pkb_web and pkb_db
% DEPRECATED - todo - check entity_label
%:- use_module(pkb_to_phenotype).
metadata_db:entity_label(X,V) :- owl2_model:labelAnnotation_value(X,V).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

% register clio/thea handlers
:- multifile cliopatrial:entity_alternate_view/3.
entity_alternate_view(Obj,organism,URL) :-
        organism(Obj),
        http_link_to_id(organism, [organism=Obj],URL).

% ditched?
:- op(800, xfy, foreach).
foreach(Template, Goal, In, Rest) :-
        findall(Val,(Goal,phrase(Template,Val,[])),Vals),
        flatten(Vals,ValsF),
        append(ValsF,Rest,In).

% todo: test if safe..
%:- table_pred(owl2_basic_reasoner:entailed/1).
%:- table_pred(owl2_reasoner:reasoner_ask/2).
:- table_pred(owl2_graph_reasoner:class_descendant/2).
:- table_pred(owl2_graph_reasoner:class_ancestor/2).
:- initialization(initialize_reasoner(graph_reasoner,_,[])).

% ----------------------------------------
% LOCATIONS
% ----------------------------------------

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(www,         /,                  []).
http:location(pkb,         '/pkb',             []).
http:location(script,      www(script),        []).
http:location(css,         www(css),           []).
http:location(cliopatria,  pkb(clio),	       [priority(5)]).


% ----------------------------------------
% HANDLERS
% ----------------------------------------

:- http_handler(pkb(.), root, []).
:- http_handler(pkb(tree), organism_cluster_treeview, []).
:- http_handler(pkb(organisms), all_organisms, []).
:- http_handler(pkb(api/organisms), all_organisms_json, []).
:- http_handler(pkb(organisms_with_matches), all_organisms_with_matches, []).
:- http_handler(pkb(genes), all_genes, []).
:- http_handler(pkb('gene/'), view_gene, [prefix]).
:- http_handler(pkb(homologsets), all_homologsets, []).
:- http_handler(pkb(phenotypes), all_phenotypes, []).
:- http_handler(pkb(view), view_entity, []).
:- http_handler(pkb(hier), browse_hierarchy, []).
:- http_handler(pkb('organism/'), view_organism, [prefix]).
:- http_handler(pkb('orgtype/'), view_organism_type, [prefix]).
:- http_handler(pkb('compare/'), view_organism_pair, [prefix]).
:- http_handler(pkb('phenoblast/'), phenoblast, [prefix]).
:- http_handler(pkb(phenoquery), phenoquery, []).
:- http_handler(pkb(search_results_page), search_results_page, []).
:- http_handler(pkb('disease/'), view_disease, [prefix]).
:- http_handler(pkb('diseases/'), all_diseases, [prefix]).
:- http_handler(pkb('class/'), view_class, [prefix]).
:- http_handler(pkb(classes), used_classes, []).

:- http_handler(pkb(js), js_dir, [prefix]).
:- http_handler(pkb(images), images_dir, [prefix]).
:- http_handler('/images/', implicit_images_dir, [prefix]).
%:- http_handler('/bubble.gif', implicit_images_dir, [prefix]).


param(title,     [optional(true)]).
param(name,      [length >= 2 ]).
param(age,       [integer]).
param(url,       [optional(true),default('http://')]).
param(open,      [zero_or_more]).
param(letter,    [zero_or_more]).
param(query,     [optional(true)]).
param(organism,  [zero_or_more]).
param(entity,    []).
param(action,    [optional(true)]).


% TODO
:- html_resource(script('tabber.js'),
	[	requires([
		    css('tabbers.css')
                         ])
	]).	

% ----------------------------------------
% MAIN
% ----------------------------------------

background:-
        thread_get_message(_).

start_server :-
        start_server(9000).

start_server(Port) :-
        http_server(http_dispatch, [port(Port)]).

% ----------------------------------------
% GENERAL UTILS
% ----------------------------------------

% TODO - replace
js_dir(Request) :-
	http_location_by_id(js_dir, ResRoot),
	memberchk(path(Path), Request),
	atom_concat(ResRoot, File, Path),  % e.g. /img/, hi.jpg, /img/hi.jpg
        atom_concat('js',File,Local), % e.g. img/, hi.jpg, img/hi.jpg
	http_reply_file(Local, [], Request).

js(_URL) -->
        html_post(js,
                  script([type='text/javascript',src='/pkb/js/all.js'])).

images_dir(Request) :-
	http_location_by_id(images_dir, ResRoot),
	memberchk(path(Path), Request),
	atom_concat(ResRoot, File, Path),  % e.g. /img/, hi.jpg, /img/hi.jpg
        atom_concat('images',File,Local), % e.g. img/, hi.jpg, img/hi.jpg
	http_reply_file(Local, [], Request).

implicit_images_dir(Request) :-
	memberchk(path(Path), Request),
	atom_concat(/, File, Path),  % 
	http_reply_file(File, [], Request).


include_htmlfile(File) -->
	{read_file_to_codes(File,Codes,[]),
	 atom_codes(A,Codes)},
	[A].


tooltip(Info) -->
	html(a([href='#',class=tt],
	       ['[?]',
		span(class=tooltip,
		     [span(class=top,''),
		      span(class=middle,Info),
		      span(class=bottom,'')])])).



% ----------------------------------------
% DB SUMMARY, INTRO PAGE
% ----------------------------------------

% handle entry-level page requests
root(_Request) :-
        reply_html_page([ title('OBD-PKB'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          \db_summary,
			  \include_htmlfile('intro.html')
                        ]).

page_header(Title) -->
        html(div(class(new_page_header),
                 [
                  a([href(location_by_id(root)),class(new_page_logo)],'OBD'),
                  div(class(full_div_blue),''),
                  div(class(menu_bar),
                      [div(class(menu_current),
                           ['OBD Home',&(nbsp),&(raquo),&(nbsp),Title]),
                       div(class(menu_items),
                           ['< ',
                            a(href(location_by_id(root)),'Home'),
                            ' | ',
                            a(href(location_by_id(all_organisms)),'Organisms'),
                            ' | ',
                            a(href(location_by_id(browse_hierarchy)),'Class Browser'),
                            ' | ',
                            a(href(location_by_id(all_genes)),'Gene List'),
                            ' | ',
                            a(href(location_by_id(all_homologsets)),'Homologs List'),
                            ' | ',
                            a(href(location_by_id(organism_cluster_treeview)),'SimTree'),
                            ' | ',
                            a(href(location_by_id(used_classes)),'Class List'),
                            ' | ',
                            a(href(location_by_id(all_diseases)),'Diseases'),
                            ' | ',
                            a(href(location_by_id(all_phenotypes)),'Phenotypes'),
                            ' >'])
                      ]),
                  div(class(full_div_blue),''),
		  \query_box])).



db_summary -->
        {aggregate(count,O,organism(O),OC)},
        {aggregate(count,P,O^organism_phenotype(O,P),PC)},
        {aggregate(count,O-P,organism_phenotype(O,P),OPC)},
        html(table(class(foo),
                   [tr([td('Organisms'),td(OC)]),
                    tr([td('Phenotypes'),td(PC)]),
                    tr([td('Annotations'),td(OPC)])])).

% TODO - this is for viewing cluster analysis of organisms
organism_cluster_treeview(_Request) :-
        reply_html_page([ title('OBD-PKB: Organism Clustering'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',
				  src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',
				  src='/pkb/js/phylo_tree.js'],[]),
                          script([type='text/javascript',
				  src='/pkb/js/tree_data.js'],[]),
                          \html_receive(js)
                        ],
                        body(onload='drawTree()',
			     [% this confuses the display: \page_header('Organism Clustering'),
			      %p('Organisms clustered by phenotypic similarity'),
			      div(id=tree,[])
			     ])).

% ----------------------------------------
% SEARCH
% ----------------------------------------

query_box -->
	html(form([id(search_results_page),
                   action(location_by_id(search_results_page))],
		  ['Search:',
		   input([type(textfield),
                          size(25),
                          name(query)],
                         []),
		   input([type(submit),
			  name(search),
			  value(search)],
			 [])])).


% currently only searches organisms and classes
search_results_page(Request) :-
        http_parameters(Request,
                        [
                         query(S)
                        ],
                        [
                         attribute_declarations(param)
                        ]),
	nonvar(S),
	!,
        debug(phenotype,'  indexing if required...',[]),
        materialize_index(metadata_nlp:entity_label_token_stemmed(0,0,1,0)),
        debug(phenotype,'  query: ~w',[S]),
        (   label_query_results(S,true,ScoreEntityPairs)
        ->  true
        ;   ScoreEntityPairs=[]),
        findall(Org,(member(_-Org,ScoreEntityPairs),organism(Org)),Orgs),
        findall(Class,(member(_-Class,ScoreEntityPairs),class(Class)),Classes),
        reply_html_page([ title(['OBD-PKB: Search Results for ',S]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          h1(['Search: ',S]),
                          \organisms_summary(Orgs),
                          \used_class_info_table(Classes)
                        ]).


% ----------------------------------------
% ORGANISMS
% ----------------------------------------

% TOP-LEVEL: list all organisms
all_organisms(Request) :-
        http_parameters(Request,
                        [
                         query(S)
                        ],
                        [
                         attribute_declarations(param)
                        ]),
        % TODO - see above; redunant
	nonvar(S),
	!,
        debug(phenotype,'  query: ~w',[S]),
	solutions(Org,
		  (   organism_label(Org,N),
		      sub_atom(N,_,_,_,S)),
		  Orgs),
	all_organisms_page(Orgs).
% e.g. all orgs beginning 'A'
all_organisms(Request) :-
        http_parameters(Request,
                        [ letter(Letters)
                        ],
                        [ attribute_declarations(param)
                        ]),
        Letters=[_|_],
        solutions(Org,
                  (   member(Letter,Letters),
                      atom_codes(A,[Letter]),
                      downcase_atom(A,A2),
                      organism_label(Org,Label),
                      downcase_atom(Label,Label2),
                      sub_atom(Label2,0,1,_,A2),
                      debug(phenotype,'label=~w',[Label])),
                  Orgs),
        !,
        all_organisms_page(Orgs).
% action on selected orgs
all_organisms(Request) :-
        http_parameters(Request,
                        [ action(Action),
                          organism(Orgs)
                        ],
                        [ attribute_declarations(param)
                        ]),
        debug(phenotype,'act: ~w orgs: ~w',[Action,Orgs]),
        Orgs=[_|_],
        !,
        action_on_selected_organisms(Action,Orgs).
% show A-Z if too many
all_organisms(_Request) :-
        debug(phenotype,'  counting orgs',[]),
        aggregate(count,Org,organism(Org),N),
        debug(phenotype,'  num orgs: ~w',[N]),
        (   N>200
        ->  organisms_by_label_index_page
        ;   setof(Org,organism(Org),Orgs),
	    all_organisms_page(Orgs)).

all_organisms_with_matches(_Request) :-
        debug(phenotype,'  counting orgs',[]),
        findall(Org,
                (   organism(Org),
                    \+ \+ organism_role_disease(Org,canonical,_),
                    \+ \+ organism_pair_score_value(Org,_,_,_)),
                Orgs),
        all_organisms_page(Orgs).

all_organisms_page(Orgs) :-
        reply_html_page([ title('OBD-PKB'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          \organisms_summary(Orgs)
                        
                        ]).



% split list because too many orgs
organisms_by_label_index_page :-
        findall(Letter,
                 (   between(1,26,N),
                     Code is N+64,
                     atom_codes(Letter,[Code])),
                 Letters),
        findall(li(a(href(location_by_id(all_organisms)+'?'+'letter='+Letter),Letter)),
                 member(Letter,Letters),
                LetterList),
        reply_html_page([ title('OBD-PKB'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          \db_summary,
                          ul(LetterList),
                          a(href(location_by_id(all_organisms_with_matches)),'all with matches')]).

% split list because too many orgs
organisms_by_label_index-->
        {findall(Letter,
                 (   between(1,26,N),
                     Code is N+64,
                     atom_codes(Letter,[Code])),
                 Letters)},
        {findall(li(a(href(location_by_id(all_organisms)+'?'+'letter='+Letter),Letter)),
                 member(Letter,Letters),
                 LetterList)},
        html(div(class(organisms_summary),
                 ul(LetterList))).


action_on_selected_organisms(compare,Orgs) :-
        debug(phenotype,'comparing: ~w',[Orgs]),
        solutions(P,
                  (   (   member(O,Orgs)
                      ;   member(O,Orgs)),
                      organism_phenotype_quad(O,P)),
                  Ps),
        debug(phenotype,'Ps: ~w',[Ps]),
        solutions(C,
                  (   member(P,Ps),
                      class_quad_aspect(C,P,_)),
                  Cs),
        debug(phenotype,'Cs: ~w',[Cs]),
        subsumed_by_lca_set(Cs,CAs),
        % TODO: speed this up
        class_hrefs_indented(CAs,CMap),
        debug(phenotype,'CAs: ~w',[CAs]),
        findall(th(\organism_href(O)),
                member(O,Orgs),
                HdrCols),
        findall(tr([td(CInfo)|ColVals]),
                (   member(C-CInfo,CMap),
                    findall(td(\phenotype_infos(SubPs)),
                            (   member(O,Orgs),
                                solutions(P,
                                          (   organism_phenotype_quad(O,P),
                                              class_quad_aspect(C,P,_)),
                                          SubPs)),
                            ColVals)),
                Rows),
        reply_html_page([ title('Compare Multiple Organisms'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Compare Multiple Organisms'),
                          h2('Compare Multiple Organisms'),
                          table(class(std_table),
                                [tr([td('Class')|HdrCols])
                                |
                                Rows])]).

% defaults to view
action_on_selected_organisms(_,Orgs) :-
        setof(Org-P,(member(Org,Orgs),organism_phenotype(Org,P)),OrgPs),
        reply_html_page([ title('Selected Organism Phenotypes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Selected Organism Phenotypes'),
                          h2('Selected Organism Phenotypes'),
                          html(table(class('sortable std_table'),
                                     [\organism_phenotype_tblhdr,
                                      \organism_phenotype_rows(OrgPs,[])
                                     ]))
                        ]).

organism_hrefs(Orgs) -->
        multi(div(\organism_href(X)),X,Orgs).


%% JSON %%
all_organisms_json(_Request) :-
        %http_read_json(Request, JSONIn),
        %json_to_prolog(JSONIn, PrologIn),
        all_organisms_exhibit_json(JSONOut),
        reply_json(JSONOut).

all_organisms_exhibit_json(JSON) :-
        findall(Org,
                organism_label(Org,_),  % ignore duff data lacking labels
                Orgs),
        organisms_exhibit_json(Orgs,JSON).
:- initialization(table_pred(all_organisms_exhibit_json/1)).

organisms_exhibit_json(Orgs,json([items=Items])) :-
        findall(Item,(member(Org,Orgs),organism_exhibit_json(Org,Item)),Items).

organism_exhibit_json(Org,json(TVs)) :-
        debug(phenotype,'calculating json for ~w',[Org]),
        setof(T=V,organism_property_exhibit_json(Org,T,V),TVs).

organism_property_exhibit_json(Org,id,Org).
organism_property_exhibit_json(Org,url,URL) :-
        http_location_by_id(view_organism,Base),
        uri_encoded(path,Org,Org_2),
        atom_concat(Base,Org_2,URL).
organism_property_exhibit_json(Org,label,Label) :-
        organism_label(Org,Label).
organism_property_exhibit_json(Org,description,Label) :-
        organism_description(Org,Label).
organism_property_exhibit_json(Org,species,Label) :-
        organism_species(Org,Species),
        species_label(Species,Label).
organism_property_exhibit_json(Org,type,Label) :-
        organism_type(Org,Type),
        display_label(Type,Label).
organism_property_exhibit_json(Org,disease,Ds) :-
        setof(DN,D^(organism_disease(Org,D),display_label(D,DN)),Ds).
organism_property_exhibit_json(Org,gene,Gs) :-
        setof(G,organism_variant_gene(Org,G),Gs).
organism_property_exhibit_json(Org,Ont,Cs) :-
        setof(CN,C^(organism_to_ontology_class(Org,Ont,C),labelAnnotation_value(C,CN)),Cs).

organism_to_ontology_class(Org,phenotype,P) :-
        organism_phenotype(Org,P),
        atom(P).
organism_to_ontology_class(Org,Cat,Class) :-
        organism_phenotype(Org,P),
        phenotype_property_value(P,_,Class),
        atom(Class),
        class_phenocategory(Class,Cat).

class_phenocategory(Class,phenotype) :-      atom_concat('http://ccdb.ucsd.edu/NDPO/1.0/NDPO.owl#',_,Class),!.
class_phenocategory(Class,anatomy) :-      atom_concat('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#',_,Class),!.
class_phenocategory(Class,cell) :-      atom_concat('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#',_,Class),!.
class_phenocategory(Class,quality) :-      atom_concat('http://purl.org/obo/owl/PATO#',_,Class),!.
class_phenocategory(Class,quality) :-      atom_concat('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Quality.owl#',_,Class),!.
class_phenocategory(Class,subcellular) :-      atom_concat('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Subcellular.owl#',_,Class),!.
class_phenocategory(Class,molecule) :-      atom_concat('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Chemical.owl#',_,Class),!.
class_phenocategory(_,ontology).
                   


%% END OF JSON %%



        
        

% TOP-LEVEL: show detail on a specific organism
% /view_organism/Org
view_organism(Request) :-
        request_path_local(Request,view_organism,Org),
        (   organism_label(Org,Label)
        ->  true
        ;   Label=Org),
        organism_type(Org,Type),
        (   organism_description(Org,Desc) -> true ; Desc=''),
        solutions(P,organism_phenotype(Org,P),Phenotypes),
        solutions(G,organism_variant_gene(Org,G),Genes),
        solutions(D,organism_disease(Org,D),Diseases),
        length(Phenotypes,NumPhenotypes),
        reply_html_page([ title(['Organism ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Organism'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Org)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Description'),td(Desc)]),
                                tr([td('Type'),td(\organism_type_href(Type))]),
                                tr([td('Genes'),td(\gene_list(Genes))]),
                                tr([td('Diseases'),td(\disease_list(Diseases))])
                                ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2(['Phenotypes [',NumPhenotypes,']']),
                                   table([class('sortable std_table')],
                                         [\phenotype_tblhdr,
                                          \phenotype_rows(Phenotypes)])]),
                              div(class(tabbertab),
                                  [h2('Similar organisms'),
                                   \similar_organisms_table(Org)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   \axiom_infos(Org)])

                             ])
                        ]).

% TOP-LEVEL
% /view_organism_type/Org
% strain etc 
view_organism_type(Request) :-
        request_path_local(Request,view_organism_type,OrgType),
        view_organism_type(Request,OrgType).

view_organism_type(_Request,OrgType) :-
        display_label(OrgType,Label),
        solutions(Org-P,(organism_inferred_type(Org,OrgType),
                         organism_phenotype(Org,P)),OrgPhenotypePairs),
        reply_html_page([ title(['Organism Type ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Organism Type'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(OrgType)]),
                                tr([td('Label'),td(Label)])]),

                         h3('Parent taxa'),
                         \superclass_list(OrgType),
                         h3('Child taxa'),
                         \subclass_list(OrgType),
                         
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2('Phenotypes'),
                                   table([class('sortable std_table')],
                                         [\organism_phenotype_tblhdr,
                                          \organism_phenotype_rows(OrgPhenotypePairs,[])])]),
                              div(class(tabbertab),
                                  [h2('Similar organisms'),
                                   %p(soon)
                                   \similar_organisms_table(OrgType)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   \axiom_infos(OrgType)])
                             ])
                        ]).




organisms_summary(Orgs) -->
        html(div(class(organisms_summary),
		 [h2(['Organisms summary',
		      \tooltip('A list of all organisms represented in the database. Click on column headings to sort')]),
		  form([id(organisms_summary)],
		       [table(class('std_table sortable'),
			      [\organism_tblhdr,
			       \organism_rows(Orgs)]),
			select(name(action),
                               [option(value(view)),
                                option(value(compare))
			       ]),
			input([name(submit),type(submit),value(organism_query)])])
		 ])).






organism_type_href(OrgType) -->
        {display_label(OrgType,Label)},
        html(a(href(location_by_id(view_organism_type) + encode(OrgType)),Label)).

organism_pair_href(Org,Hit) -->
        html(a(href(location_by_id(view_organism_pair) + encode(Org) + ' ' + encode(Hit)),
	       b('[VIEW]'))).
%	       img([height='16px',src='/images/compare.gif'],''))).



organism_tblhdr --> html(tr([th(''),
                             th('Organism'),
                             th('Species'),
                             th('Type'),
                             th('Phenotypes'),
                             th('Description')])).

organism_rows(L) --> multi(organism_row,L).

organism_row(Org) -->
        {debug(phenotype,'org=~w',[Org])},
        {organism_species(Org,Species) -> true ; Species=''},
        {(   aggregate(count,P,organism_phenotype(Org,P),NumP)
         ->  true
         ;   NumP = 0)},
        {organism_type(Org,Type)
        ->  true
        ;   Type=''
        },
        {organism_description(Org,Desc) -> true ; Desc=''},
        html(tr([td(input([type=checkbox,name=organism,value=Org])),
                 td(\organism_href(Org)),
                 td(\organism_type_href(Species)),
                 td(\organism_type_href(Type)),
                 td(NumP),
                 td(Desc)])).

organism_href(Org) -->
        {organism(Org)},!,
        {organism_label(Org,Label) -> true ; Label=Org},
        html(a(href(location_by_id(view_organism) + encode(Org)),Label)).
organism_href(Org) -->
        {\+ \+ organism_type(_,Org)},!,
        html([\organism_type_href(Org),
              ' [multiple]']).

% TOP-LEVEL
% /view_organism_pair/Q+S
view_organism_pair(Request) :-
        request_path_local(Request,view_organism_pair,OrgPairAtom),
	http_parameters(Request,
			[ format(Fmt,
                              [optional(true),
                               default(html)])
                        ]),
        concat_atom([Q,S],' ',OrgPairAtom),
        debug(phenotype,'comparing ~w vs ~w~n',[Q,S]),
        view_organism_pair(Q,S,Fmt).

view_organism_pair(Q,S,html) :-
        reply_html_page([ title(['Organism Phenotype Comparison']),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [
                         \page_header('Comparison'),
                         \org_pairwise_comparison_table(Q,S)
                        ]).

view_organism_pair(Q,S,tsv) :-
        format('Content-type: text/plain;~n~n'),
        labelAnnotation_value(Q,QN),
        labelAnnotation_value(S,SN),
        phrase(org_pairwise_comparison_table_tsv(Q,S), Toks),
        maplist(write,['LCS\tIC\tSimJ\t',QN,'\t',SN,'\n'|Toks]).



% PAIRWISE comparisons
% DEPENDS: minimal_LCS_simJ-avg_simJ
org_pairwise_comparison_table(F1,F2) -->
	 !,
        {debug(phenotype,'compare: ~q, ~q',[F1,F2]),
	 organism_pair_score_value(F1,F2,minimal_LCS_simJ-avg_simJ,Pairs-AvgSim),
	 Pairs=[TopSim-lcs(TopLCS,TopX1,TopX2)|_],
	 (   TopLCS=[TopLCS_1|TopLCS_Rest] % old
         ->  true
         ;   TopLCS_1=TopLCS),
	 debug(phenotype,'avgSim: ~w',[AvgSim])},
        html([h2('Organism-Phenotype Pairwise Comparison Table'),
	      p(i(['Scroll to ',a(href='#info','bottom of table'),' for full description'])),
              p(a(href(location_by_id(view_organism_pair) + encode(F1) + ' ' + encode(F2) + '?format=tsv'),
                  [b('[Download this data as a tab-separated table]')])),
	      p(['Average Similarity: ',
		 b(AvgSim),
		 \avg_simj_tooltip]),
	      table(border(1),
		    [tr([th(width='40%',
			    \organism_href(F1)),
			th(' '),
			th(width='40%',
			   \organism_href(F2))]),
		    \org_pairwise_comparison_table_lcs_rows(Pairs,[])]),
	      div([id=info,class=infoBox],
		  [
		   h3('Documentation'),
		   p(['This table displays the phenotypic similarity between two organisms, ',
		      \organism_href(F1),' and ',\organism_href(F2),'. ']),
		   p(['Each phenotype is described combinatorially as a set of classes (this is a simplification of other underlying more complex phenotype representation). ',
		      'The classes are drawn from different ontologies, including the PATO ontology of phenotypic qualities. ',
		      'For example, the first phenotype listed for ',\organism_href(F1),
		      ' is {',\class_info(TopX1),'}. '
		      ]),
		   p(['We try and match up each phenotype in each of the two organisms with its best matching phenotype in the other organism. ',
		      'For each matching pair, we also show the least common subsumer - a description that unifies both phenotypes. ',
		      'The resulting display is a list of match-triples, between each pair and a common subsumer.']),
		   h3('Pairwise matches'),
		   p(['The phenotypes specific to each organism are listed on the left and right sides, such that each phenotype is aligned alongside ',
		      'the best-matching phenotype in the other organism, with the best match at the top. If the match is a reciprocal best hit, then both phenotypes will be shown in bold. ',
		      'Otherwise, one of the phenotypes is shown in italics (this phenotype will have a better match higher up in the table). ']),
		   p(['For example, the best match in the table above is ',
		      \example_sim(TopSim,TopX1,TopX2),
		      '. ']),
		   p(['Occasionally more than one phenotype is listed on each side of the pairwise match. This happens when there are ties between matches. ']),
		   h3('Pairwise Similarity Scores'),
		   p(['Each pairwise matching is assigned a similarity score. OBD is capable of calculating different similarity scores, but in this case the ',
		      a(href='http://en.wikipedia.org/wiki/Jaccard_index','Jacard Similarity'),' is used. ',
		      'This is the ratio of total set of all classes in common between the two phenotypes versus the the union of these classes. ',
		      'Note that the fully inferred subsumption hierarchy is used. ',
		      'For example, in this particular comparison, the best pairing is between ',
		      \class_info(TopX1),' and ',\class_info(TopX2),'. ',
		      'The classes in common include ',\class_info(TopLCS_1),' and all the subsumers of this class',
		      \example_subsumer(TopLCS_1),' as well as all the subsumers for ',\class_info(TopLCS_Rest)
		      ]),
		   h3('Least Common Subsumers'),
		   p(['Above each phenotype pairing is shown the Least Common Subsumer (LCS). This is the most specific description that could be found which is inclusive of both ',
		      'individual phenotypes. The description is also combinatorial, consisting of a conjunction of classes. ',
		      'Sometimes the individual elements of these descriptions may be class expressions, rather than named classes in the ontology']),
		   p(['We also calculate the information content (IC) of each of the LCSs. ',
		      'The IC is a measure of "surprise" at seeing a phenotype. It is calculated by taking the negative of the log of the probability of an organism ',
		      'having that phenotype. Note that the only way OBD has of calculating this probability is using the existing data in the database. ',
		      'This means that the IC is subject to literature bias, and is less useful when the corpus is small.']),
		   p(['For example, if the corpus is biased towards hippocampal phenotypes, then a hippocampal phenotype will have a lower IC, because it ',
		      'looks like a common phenotype. ',
		      'Conversely, if only two organisms in the corpus have a retinal phenotype, then this will score highly, because it looks like ',
		      'a rare phenotype.']),
		   p(['Note also that the IC of the LCS does not reflect differences between the phenotype pair, it only reflects what is shared between the two']),
		   h3('Unmatched phenotypes'),
		   p(['The bottom of the table shows the unmatched phenotypes (if any). ',
		      'No pairings could be made for these (using the existing ontologies)']),
		   ''
		  ])]).

org_pairwise_comparison_table_tsv(F1,F2) -->
	 !,
        {debug(phenotype,'compare: ~q, ~q',[F1,F2]),
	 organism_pair_score_value(F1,F2,minimal_LCS_simJ-avg_simJ,Pairs-AvgSim),
	 debug(phenotype,'avgSim: ~w',[AvgSim])},
        org_pairwise_comparison_table_lcs_rows_tsv(Pairs,[]).



example_sim(_,TopX,TopX) -->
	!,
	html(['an exact match -- both organisms have the ',\class_info(TopX),' phenotype (which has a perfect match score of 1)']).
example_sim(Sim,TopX1,TopX2) -->
	html([\class_info(TopX1),' and ',\class_info(TopX2),' (with a score of ',Sim,')']).

example_subsumer(X) -->
	{subClassOf(X,Y)},
	!,
	html([' (such as ',\class_info(Y),')']).
example_subsumer(_) -->
	html(' (umm, guess I chose a bad example here, I can\'t find any subsumers which is odd. Sorry about that, bear with me...)').



/*
org_pairwise_comparison_table(F1,F2) -->
	old_org_pairwise_comparison_table(F1,F2).
*/

org_pairwise_comparison_table_lcs_rows([],_) --> [].
org_pairwise_comparison_table_lcs_rows([Pair|Pairs],PairsDone) -->
	org_pairwise_comparison_table_lcs_row(Pair,PairsDone),
	org_pairwise_comparison_table_lcs_rows(Pairs,[Pair|PairsDone]).

org_pairwise_comparison_table_lcs_row(Pair,PairsDone) -->
	{Pair=Sim-lcs(LCS,S1s,S2s),
         % hack - old style used list here, new style singletons
	 (   member(S1,S1s),
	     member(S2,S2s),
	     phenotype_pair_score_value(S1,S2,lcs_IC,LCS_IC)
	 ->  true
	 ;   phenotype_pair_score_value(S1s,S2s,lcs_IC,LCS_IC)
         ->  true
         ;   LCS_IC='?'),
	 (   member(_-lcs(_,S1s,_),PairsDone)
	 ->  S1IsBest=false
	 ;   S1IsBest=true),
	 (   member(_-lcs(_,_,S2s),PairsDone)
	 ->  S2IsBest=false
	 ;   S2IsBest=true)},
        html([tr(td([colspan(3),align(center)],
                    [\phenotype_lcs_info(LCS),
		     br(''),
		     span(['Information Content of LCS: ',
			   b(LCS_IC),
			   \lcs_IC_tooltip(LCS_IC)
			   ])
		    ])),
              tr([td(\hi_phenotype_infos(S1IsBest,S1s)),
                  td(valign=top,
		     ['Pairwise Similarity: ',
		      b(Sim),
		      \pairwise_similarity_tooltip(Sim)
			   ]),
		  td(\hi_phenotype_infos(S2IsBest,S2s))]),
	      tr(td(colspan(3),p('')))]).

% TSV
org_pairwise_comparison_table_lcs_rows_tsv([],_) --> [].
org_pairwise_comparison_table_lcs_rows_tsv([Pair|Pairs],PairsDone) -->
	org_pairwise_comparison_table_lcs_row_tsv(Pair,PairsDone),
        ['\n'],
	org_pairwise_comparison_table_lcs_rows_tsv(Pairs,[Pair|PairsDone]).

% TODO: DRY
org_pairwise_comparison_table_lcs_row_tsv(Pair,_PairsDone) -->
	{Pair=Sim-lcs(LCS,S1s,S2s),
	 (   member(S1,S1s),
	     member(S2,S2s),
	     phenotype_pair_score_value(S1,S2,lcs_IC,LCS_IC)
	 ->  true
	 ;   LCS_IC='?'),
         /*
	 (   member(_-lcs(_,S1s,_),PairsDone)
	 ->  S1IsBest=false
	 ;   S1IsBest=true),
	 (   member(_-lcs(_,_,S2s),PairsDone)
	 ->  S2IsBest=false
	 ;   S2IsBest=true)
           */
         true},
        phenotype_info_txt(LCS),
        ['\t'],
        [LCS_IC],
        ['\t'],
        [Sim],
        ['\t'],
        phenotype_infos_txt(S1s),
        ['\t'],
        phenotype_infos_txt(S2s).
        

phenotype_lcs_info([]) --> !, html(i('No match')).
phenotype_lcs_info([X]) --> !,phenotype_info(X).
phenotype_lcs_info([X|L]) --> !,phenotype_info(X),phenotype_lcs_info(L).
phenotype_lcs_info(X) --> !,phenotype_info(X).

sim_expl(1,'A score of 1 indicates an exact match').
sim_expl(Sim,'This indicates a high degree of similarity') :- Sim>0.75.
sim_expl(Sim,'This indicates a moderate-to-high degree of similarity') :- Sim>0.5.
sim_expl(Sim,'This indicates a low-to-moderate degree of similarity') :- Sim>0.25.
sim_expl(0,'A score of 0 indicates that the class sets have nothing in common in the ontologies used').
sim_expl(_,'This indicates a very low level of similarity').

avg_simj_tooltip -->
	tooltip('The average pairwise similarity between all best-matching pairs. Note that at this time this is only calculated where matches can be found. This is not an ideal strategy, and in future this score will penalize unmatched phenotypes').


pairwise_similarity_tooltip(Sim) -->
	{sim_expl(Sim,Expl)},
	html(\tooltip(['This is the Jacard Similarity measure of the overlap between the pair of class sets in each of the organisms. ',
		       'The Jacard Similarity is the ratio between classes in common and classes in the union (the full inferred subsumption hierarchy is taken into account). ',
		       br(''),
		       'The similarity of these two class sets is ',Sim,'. ',
		       Expl,'. '])).

ic_expl(IC,'The IC was not calculated for this LCS') :- \+number(IC).
ic_expl(IC,'This is a high IC, representing p<0.01 (against the current database)') :- IC > 6.64.
ic_expl(IC,'This is a moderate IC, representing 0.01 < p < 0.05') :- IC > 4.32.
ic_expl(IC,'This is a low/moderate IC, representing 0.05 < p < 0.1') :- IC > 3.32.
ic_expl(IC,'This is a low IC, representing 0.1 < p < 0.25') :- IC > 2.
ic_expl(_,'This is an insignificant IC, representing 0 < p < 0.25').




lcs_IC_tooltip(IC) -->
	{ic_expl(IC,Expl)},
	html(\tooltip(['This is the information content (IC) of the most specific Least Common Subsuming description that could be inferred for the phenotype pair. ',
		       'The IC is the negative log of the probability of seeing this phenotype by chance in an organism, based on phenotypes in this database. ',
		       br(''),
		       'Note that without a large background set, this figure may be misleading. ',
		       'The IC for this LCS description is ',IC,'. ',
		       Expl,'. '])).


	

/*
old_org_pairwise_comparison_table(F1,F2) -->
	 !,
        {method_feature_pair_phenosim(postcomposed,F1,F2,Results), % TODO
	 member(bestmatches(P1Xs,P2Xs),Results),
         append(P1Xs,P2Xs,PXs),
	 length(PXs,NumPXs),
	 debug(phenotype,'number_of_pairs: ~w',[NumPXs]),
         setof(IC-P,P1^P2^member(P1-P2-P-IC,PXs),ICPsRev),
         reverse(ICPsRev,ICPs),
         findall(\phenoblast_summary(P,IC,P1Xs,P2Xs),
                 member(IC-P,ICPs),
                 Sections)
         },
        html(table(border(1),
                   [tr([th(\organism_href(F1)),
                        th(' '),
                        th(\organism_href(F2))])
                   |
                   Sections])).

% show pairwise matches in a graphviz display
old_org_pairwise_comparison_table(F1,F2) -->
        {method_feature_pair_phenosim(psimj,F1,F2,_), % TODO
	 !,
	 feature_pair_to_dotgraph(F1,F2,G),
	 debug(phenotype,'img=~w',[File]),
	 %concat_atom(['js/cache/',F1,'-',F2,'.png'],File),
	 File='js/cache/temp.png',
	 debug(phenotype,'g=~w',[G]),
	 graph_to_dot_file(G,png,File),
	 atom_concat('/pkb/',File,URL) % TODO
	},
        html(div([\feature_pair_subsumers(F1,F2),
		    div(img([src=URL]))
		 ])).

% symmetry - TODO
old_org_pairwise_comparison_table(F1,F2) -->
        {method_feature_pair_phenosim(psimj,F2,F1,_)}, % TODO - better symm
	!,
	org_pairwise_comparison_table(F2,F1).

feature_pair_subsumers(F1,F2) -->
	{method_feature_pair_phenosim(simj_all,F1,F2,Rs),
	 member(subsumers(L),Rs)},
	html(div(table(class('sortable std_table'),
		       [tr([td('IC'),td('Subsumer')]),
			\ic_match_pair_list(L)]))).

ic_match_pair_list(L)--> multi(ic_match_pair,L).
ic_match_pair(IC-M) -->
	html(tr([td(IC),td(\class_info(M))])).

*/  


% ----------------------------------------
% ORGANISM-PHENOTYPE
% ----------------------------------------

% detail
organism_phenotype_list -->
        {setof(Org-P,organism_phenotype(Org,P),OrgPs),
         length(OrgPs,NumOPs),
         debug(phenotype,'ops=~w',[NumOPs]),
         NumOPs < 2000},
        html(table(class('sortable std_table'),
                   [\organism_phenotype_tblhdr,
                    \organism_phenotype_rows(OrgPs,[])
                   ])
            ).
organism_phenotype_list -->
        html(h2('too many annotations')).

organism_phenotype_tblhdr --> html(tr([th('Organism'),
                                       th('Type'),
                                       th('Phenotype'),
                                       th('Anatomical Context'),
                                       th('Bearer'),
                                       th('Property'),
                                       th('Depends on')
                                      ])).
        
organism_phenotype_rows([],_) --> [].
organism_phenotype_rows([Org-P|T],Map) --> !,html([\organism_phenotype_row(Org,P,Map),\organism_phenotype_rows(T,Map)]).

organism_phenotype_row(Org,P,Map) -->
        {organism_label(Org,Label) -> true ; Label=Org},
        %{organism_role(Org,Role) -> true ; Role=''},
        {organism_type(Org,Type)
        ->  (   labelAnnotation_value(Type,TypeLabel)
            ->  true
            ;   TypeLabel=Type)
        ;   Type='',TypeLabel=''
        },
        {organism_description(Org,Desc)
        ->  Info= \tooltip(Desc)
        ;   Info=''},
        html(tr([ %td([a(href(location_by_id(view_organism) + encode(Org)),Label),' ',p([id(Org),class(def)],Desc)]),
                 td([\organism_href(Org),Info]),
                 td(\class_info(Type)),
                 \phenotype_cols(P,Map)
                 ])).

% triptychs for class-centric view
organism_phenotype_match_rows(L) --> multi(organism_phenotype_match_row,L).

organism_phenotype_match_header -->
        html(tr([th('Common Phenotype'),
                 th('Pairwise Similarity'),
                 th('Organism A'),
                 th('Phenotype A'),
                 th('Organism B'),
                 th('Phenotype B')
                 ])).

organism_phenotype_match_row(match(Org1,Org2,Match)) -->
        {debug(phenotype,'match=~w',[Match])},
        {Match= Score-lcs(LCS,S1s,S2s)},
        {sformat(ScoreAtom,'~3f',[Score])},
        !,
        html(tr([td(\phenotype_lcs_info(LCS)),
                 td(ScoreAtom),
                 td(\organism_href(Org1)),
                 td(\phenotype_infos(S1s)),
                 td(\organism_href(Org2)),
                 td(\phenotype_infos(S2s))
                 ])).


% ----------------------------------------
% PHENOTYPES
% ----------------------------------------

% TOP-LEVEL - list all phenotypes        
all_phenotypes(_Request) :-
        reply_html_page([ title('Phenotypes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Phenotypes'),
                          h2('All phenotypes'),
                          \organism_phenotype_list
                        ]).



% TOP-LEVEL - query by phenotype expression
% phenoquery?bearer=E&....
% TODO - use new model
phenoquery(Request) :-
        http_parameters(Request,
                        [
                         bearer(E, [optional(true),default(-)]),
                         quality(Q, [optional(true),default(-)]),
                         towards(D, [optional(true),default(-)]),
                         anatomical_context(W, [optional(true),default(-)])
                        ],
                        [
                         attribute_declarations(param)
                        ]),
        PQ=(E,Q,D,W),
        debug(phenotype,'Query=~w',[PQ]),
        solutions(Org-P,
                (   organism_phenotype_quad(Org,P,PQ1),
                    phenotype_subsumed_by(PQ1,PQ)),
                OrgPs),
        debug(phenotype,'Results=~w',[OrgPs]),
        reply_html_page([ title('Phenotype Query'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Phenotype Query'),
                          form(id(phenoquery),
                               [
                                \ontol_selectbox(anatomical_context,W),
                                \ontol_selectbox(bearer,E),
                                \ontol_selectbox(quality,Q),
                                \ontol_selectbox(towards,D),
                                input([name(submit),type(submit),value(query)])
                               ]),
                          h3('Current query:'),
                          table(\phenotype_rows([(E,Q,D,W)])),
                          table(class('sortable std_table'),
                                [\organism_phenotype_tblhdr,
                                 \organism_phenotype_rows(OrgPs,[])
                                ])
                        ]).

% TOP-LEVEL
% phenoblast/Org
phenoblast(Request) :-
        request_path_local(Request,phenoblast,Org),
        (   organism_label(Org,Label)
        ->  true
        ;   Label=Org),
        reply_html_page([ title(['Phenoblast ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])

                        ],
                        [
                         \page_header('Phenoblast'),
                         \similar_organisms_table(Org)
                        ]).



% 3 rows summarising a phenotype pair and their LCA
% DEP:
phenoblast_summary(P,IC,P1Xs,P2Xs) -->
        {solutions(P1,
                   (   member(P1-_-P-_,P1Xs)
                   ;   member(_-P1-P-_,P2Xs)),
                   P1s),
         solutions(P2,
                   (   member(P2-_-P-_,P2Xs)
                   ;   member(_-P2-P-_,P1Xs)),
                   P2s),
	 Freq=2		     % TODO - precompute? too slow
%         (   phenotype_frequency(P,Freq)
%	 ->  true
%	 ;   Freq=2) % TODO
        },
        html([tr(td([colspan(3),align(center)],
                    \phenotype_info(P))),
              tr(td([colspan(3),align(center)],
                    [IC,' freq:',Freq])),
              tr([td(ul(\phenotype_infos(P1s))),
                  td(''),
                  td(ul(\phenotype_infos(P2s)))])]).

hi_phenotype_infos(true,X) --> !,html(b(\phenotype_infos(X))).
hi_phenotype_infos(_,X) --> html(i(\phenotype_infos(X))).

phenotype_infos([]) --> !,[].
phenotype_infos([H]) --> !,phenotype_info(H).
phenotype_infos([H|L]) --> !,phenotype_info(H),html(hr('')),phenotype_infos(L).
phenotype_infos(H) --> !,phenotype_info(H).

phenotype_infos_txt([]) --> !,[].
phenotype_infos_txt([H]) --> !,phenotype_info_txt(H).
phenotype_infos_txt([H|L]) --> !,phenotype_info_txt(H),[', '],phenotype_infos_txt(L).

phenotype_info_txt( Conjs ) -->
        {is_list(Conjs)},
        !,
        ['<'],
        class_info_list_txt(Conjs),
        ['>'].
phenotype_info_txt( P ) --> class_info_txt( P ).




% todo - rename phenotype_quad_info?
phenotype_info(P) -->
        {P=(E,Q,D,W)},
	!,
        %{debug(phenotype,'p=~w/~w/~w/~w',[E,Q,D,W])},
        html(div([\class_info(W),
              '/',
              \class_info(E),
              '/',
              \class_info(Q),
              '/',
              \class_info(D),
              ' ( ',
              \phenoquery_href(P),
              ' ) '
             ])).
% allow also sets representing arbitrary conjunctions
phenotype_info(P) -->
	{P=[_|_]},
	!,
	class_info(P).
phenotype_info(P) -->
        html(div(\class_info(P))).


%% getscore(+Score:term, +ScoreVals:list, ?Val) is det
% defaults to zero
getscore(S,SVs,V) :- getscore(S,SVs,V,0).

getscore(S,SVs,V,_) :- member(S-V,SVs),!.
getscore(_,_,Def,Def) :- !.

% ad-hoc combo of maxIC and avg_IC and min_LCS_simJ
combine_scores(SVs,Score) :-
	getscore(max_IC,SVs,Score1),
	getscore(avg_simJ,SVs,Score2),
        (   number(Score1),
            number(Score2)
        ->  Score is Score1+Score2
        ;   Score=0).


similar_organisms_table(Org) -->
	{debug(phenotype,'getting hits for ~q',[Org]),
	 solutions(Score-hit(Org,Hit,SVs),
		   (   organism_match_all_score_values(Org,Hit,SVs,[avg_simJ,max_IC,max_IC_best]),
		       combine_scores(SVs,Score)), % todo
		   ScoreHitPairsR),
         debug(phenotype,'got hits for ~q',[Org]),
         reverse(ScoreHitPairsR,ScoreHitPairs)},
        html(table(class('sortable std_table'),
                   [tr([th('Organism/Type'),
                        th('Species'),
                        %th('Overlap'),
                        th(['MaxIC',
			   \tooltip('Average Information Content across minimal Least Common Subsumer set')]),
                        th(['AvgSimJ',
			    \avg_simj_tooltip]),
                        th(['Combined',
			    \tooltip('Ad-hoc combination of all scores')]),
                        th([colspan=3],
			   ['Best Match',
			    \tooltip('The most specific phenotype description that could be calculated to cover both source and target species.
				    The information content (IC) of this description is also shown. This is the inverse log of the probability of that
				    description being observed by chance')]),
			th('View')
			]),
		    \organism_similarity_matchrows(ScoreHitPairs)])).

organism_similarity_matchrows(L) --> multi(organism_similarity_matchrow,L).

% for upheno analysis, map to avg_IC and best_LCS
organism_similarity_matchrow(Combined-hit(Org,Hit,SVs)) -->
	{
	 organism_species(Hit,Sp),
	 getscore(max_IC,SVs,MaxIC),
	 getscore(max_IC_best,SVs,lcs(X,Y,A)),
	 %getscore(best_LCS,SVs,[BestLCS|_]),
	 getscore(avg_simJ,SVs,AvgSimJ)
	},
        html(tr([td(\organism_href(Hit)),
                 td(\organism_type_href(Sp)),
                 td(MaxIC),
                 td(AvgSimJ),
                 td(Combined),
                 td(\entity_info(X)),
                 td(\entity_info(Y)),
                 td(A),
                 %td(\multi(composite_entity_info,AvgSimJ)),
                 td(\organism_pair_href(Org,Hit))])
            ).

% --

        
phenotype_tblhdr --> html(tr([th('Phenotype'),
                              th('Anatomical Context'),
                              th('Bearer'),
                              th('Property'),
                              th('Depends on'),
                              th('Description')
                              ])).

phenotype_rows(L) --> multi(phenotype_row,L).

phenotype_row(P) -->
        {debug(phenotype,'phenotype=~w',[P])},
        html(tr(\phenotype_cols(P,[]))).

phenotype_cols(P,Map) -->
        {(phenotype_quad(P,PQ),
          PQ=(E,Q,D,W)
         ->  true
         ;   PQ=(-,-,-,-)),
	 (   atom(P)
	 ->  P1=P
	 ;   P1=''),
         (   member(P-e,Map) -> HE=true ; HE=false),
         (   member(P-q,Map) -> HQ=true ; HQ=false),
         (   member(P-d,Map) -> HD=true ; HD=false),
         (   member(P-w,Map) -> HW=true ; HW=false)
        },
        html(
             [td(\class_info(P1,false)),
              td(\class_info(W,HW)),
              td(\class_info(E,HE)),
              td(\class_info(Q,HQ)),
              td(\class_info(D,HD)),
              td(\phenoquery_href(P)),
              td('')
             ]
            ).


phenoblast_href(Org) -->
        html(a(href(location_by_id(phenoblast) + encode(Org)),'Phenoblast')).

phenoquery_href((E,Q,D,W)) -->
        !,
        {sformat(E2,'~w',[E]),
         sformat(Q2,'~w',[Q]),
         sformat(D2,'~w',[D]),
         sformat(W2,'~w',[W])},
        html(a(href(location_by_id(phenoquery)+'?bearer='+encode(E2)+'&quality='+encode(Q2)+'&towards='+encode(D2)+'&anatomical_context='+encode(W2)),query)).
phenoquery_href(P) -->
        {phenotype_quad(P,PQ)},
        !,
        phenoquery_href(PQ).

% ----------------------------------------
% GENES
% ----------------------------------------

% TOP-LEVEL: list all genes
all_genes(_Request) :-
        setof(G,gene(G),Genes),
        reply_html_page([ title('Genes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Genes'),
                          h2('Genes'),
                          table(\gene_rows(Genes))
                        ]).

gene_rows(L) --> multi(gene_row,L).
gene_row(G) -->
        {(   aggregate(count,D,V^disease_gene_variant(D,G,V),NumD)
         ->  true
         ;   NumD = 0)},
	{
	 solutions(G2,homologous_to(G,G2),Homologs)
	},
        html(tr([td(input([type=checkbox,name=gene,value=G])),
                 td(\gene_href(G)),
                 td(\gene_list(Homologs)),
                 td(NumD)])).

        

gene_list([]) --> [].
gene_list([G|GL]) --> entity_info(G),[' '],gene_list(GL).

gene_href(X) -->
        {(   entity_label(X,Label)
	 ->  true
	 ;   gene_symbol(X,Label)
	 ->  true
	 ;   Label=X)},
        html(a(href(location_by_id(view_gene) + encode(X)),Label)).

view_gene(Request) :-
        request_path_local(Request,view_gene,Gene),
        view_gene(Request,Gene).

view_gene(_Request,Gene) :-
        (   gene_symbol(Gene,Label)
        ->  true
        ;   Label=Gene),
        solutions(D,disease_gene_variant(D,Gene,_),Diseases),
        solutions(O,organism_variant_gene(O,Gene),Organisms),
        solutions(G2,homologous_to(Gene,G2),Homologs),
        reply_html_page([ title(['Gene ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Gene'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Gene)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Organisms'),td(\organism_hrefs(Organisms))]),
                                tr([td('Diseases'),td(\disease_list(Diseases))])
                                ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2(['Phenotypes']),
                                   table([class('sortable std_table')],
                                         [\phenotype_tblhdr,
                                          \phenotype_rows([])])]),
                              div(class(tabbertab),
                                  [h2('Homologs'),
                                   table(\gene_rows(Homologs))
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
				   []])
                             ])
                        ]).


% ----------------------------------------
% HOMOLOGSETS
% ----------------------------------------

% TOP-LEVEL: list all homologsets
all_homologsets(_Request) :-
        solutions(H,homologset_member(H,_),Homologsets), % TODO
	solutions(Sp,(homologset_member(H,G),
		      organism_variant_gene(Org,G),
		      organism_species(Org,Sp)
		      ),
		  Sps),
	findall(th(\entity_info(Sp)),member(Sp,Sps),SpHdrCols),
        reply_html_page([ title('Homologsets'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Homologsets'),
                          h2('Homologsets'),
                          table(class('sortable std_table'),
				[tr([th('Diseases')|SpHdrCols]),
				 \homologset_rows(Homologsets,Sps)])
                        ]).

homologset_rows([],_) --> [].
homologset_rows([H|Hs],Sps) --> homologset_row(H,Sps),homologset_rows(Hs,Sps).

homologset_row(H,Sps) -->
        {solutions(O,
		   (   homologset_member(H,G),
		       disease_gene_variant(D,G,_),
		       organism_disease(O,D)),
		   Os)},
        html([tr([td(\organism_hrefs(Os)),
		  \homologset_species_cols(H,Sps),
		  td(\homologset_sim(H))])]).


homologset_species_cols(_,[]) --> [].
homologset_species_cols(H,[Sp|Sps]) -->
	{solutions(G,(
		      homologset_member(H,G),
		      organism_variant_gene(Org,G),
		      organism_species(Org,Sp)),
		   Gs)
	},
	html([td(\gene_list(Gs)),
	      \homologset_species_cols(H,Sps)]).

homologset_sim(H) -->
	{solutions(CL,(
		       homologset_member(H,G1),
		       homologset_member(H,G2),
		       G1\=G2,
		       organism_variant_gene(O1,G1),
		       organism_variant_gene(O2,G2),
		       debug(phenotype,'testing ~w vs ~w',[O1,O2]),
		       feature_pair_phenosim_value(O1,O2,subsumers,CL)),
		   CLs),
	 flatten(CLs,Atts)
	},
	html(\multi(entity_info,Atts)).



% ----------------------------------------
% DISEASES
% ----------------------------------------

% TOP-LEVEL: list all diseases
all_diseases(_Request) :-
        solutions(S,(species(S),
                     \+ \+ inferred_organism_role_disease_species(_,model,_,S,_)),
                  SL),
	debug(phenotype,'species with data: ~w',[SL]),
        findall(th(\organism_type_href(S)),
                member(S,SL),
                SpHdrs),
	setof(D,disease(D),DL),
	debug(phenotype,'Diseases: ~w',[DL]),
        reply_html_page([ title('Diseases'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Diseases'),
                          h2('All Diseases'),
                          p(class(info),
                            'this table shows all diseases with organismal phenotype data associated. Click on column headings to sort.'),
                          table(class('sortable std_table'),
				[
				 tr([th('Name'),
				     th(['Cases',
					 \tooltip('Total distinct human records in database associated with this disease')]),
				     th(['Phenotypes',
					 \tooltip('Total distinct phenotypes in database associated with this disease')]),
				    th(['All Potential',
					\tooltip('Best algorithmically predicted model for the disease, across all species')])
				    |SpHdrs]),
				% tr([th(colspan=4,'Disease'),
				%    th(colspan=10,'Predicted Best Model')]),
				 \disease_rows(DL,SL)]),
			  div([id=info,class=infoBox],
			      [
			       h3('Documentation'),
			       p('This pages summarises the main diseases in the database, and shows algorothmically predicted best models for the disease.'),
			       h3('Predictions'),
			       p('For each disease, the single best model is predicted. If it is a tie, multiple models are shown.'),
			       p(['For each disease-species pair, the best predicated model of that disease in that species is shown. ',
				  'If this is a reciprocal best hit (i.e. that model does not better match any other disease), then this is indicated.']),
			       p(['Predicted matches are also shown between the "canonical" disease and individual humans. ',
				  'We would expect that individual humans diagnosed with the disease are always the best match for the canonical disease; ',
				  'in some cases this is not the case, but this may be because there are no individual humans diagnosed with that disease in the database.']),
			       h3('Algorithm'),
			       p('Multiple metrics are combined. Soon you will be able to select different metrics')
			      ])
		   
                        ]).

disease_rows([],_) --> [].
disease_rows([X|XL],SL) --> disease_row(X,SL),disease_rows(XL,SL).

disease_row(Disease,SL) -->
	{
	 debug(phenotype,'disease: ~w',[Disease]),
	 disease_canonical_organism(Disease,Canonical),
	 (   disease_description(Disease,Desc)
	 ->  true
	 ;   Desc=''),
	 aggregate(count,Org,Role^organism_role_disease(Org,Role,Disease),NumOrgs),
	 aggregate(count,P,disease_phenotype(Disease,P),NumPhenotypes),
	 debug(phenotype,'NumP: ~w',[NumPhenotypes]),
	 Metric=avg_IC+maxIC,
	 % best overall model for Disease
	 solutions(Model,
		   inferred_organism_role_disease(Model,model,Disease,Metric),
		   Models),
	 % best model per-species for Disease
	 findall(S-ModelsBySpecies,
		 (   member(S,SL),
		     solutions(Model-IsReciprocal,
			       inferred_organism_role_disease_species(Model,model,Disease,S,IsReciprocal,Metric),
			       ModelsBySpecies)
		 ),
		 SpeciesMatchesList)},
	html(tr([td([\disease_href(Disease),
		     \tooltip(Desc)]),
		 td(NumOrgs),
		 td(NumPhenotypes),
		 td(\organism_hrefs(Models)),
		 \model_matches_columns(Canonical,SpeciesMatchesList)
		 ])).

model_matches_columns(_,[]) --> [].
model_matches_columns(Canonical,[_-ML|MLs]) -->
	html(td(\model_matches(Canonical,ML))),
	model_matches_columns(Canonical,MLs).


model_matches(_,[]) --> [].
model_matches(Canonical,[M|ML]) --> model_match(Canonical,M),model_matches(Canonical,ML).
	
model_match(Canonical,M-IsReciprocal) -->
	{(   IsReciprocal
	 ->  Notes=' [Reciprocal]'
	 ;   Notes='')},
	html(div([\organism_href(M),
		  b(Notes),
		  \organism_pair_href(Canonical,M)])).

% TOP-LEVEL: view a specific disease
% /disease/Disease
view_disease(Request) :-
        request_path_local(Request,view_disease,Disease),
        disease_label(Disease,Label),
        (   disease_description(Disease,Desc)
	->  true
	;   Desc=''),
        findall(tr([td(\organism_href(Org)),
                    td(Role)]),
                organism_role_disease(Org,Role,Disease),
                OrgRows),
        solutions(P,disease_phenotype(Disease,P),Phenotypes),
        reply_html_page([ title(['Disease ',Label]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Disease'),
                         h1(['Disease: ',Label]),
                         table(disease('sortable std_table'),
                               [tr([td('URI'),td(Disease)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Description'),td(Desc)]),
                                te([td('Neurolex'),
                                    td(\neurolex_href(Disease))])
                               ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                [h2('Organisms'),
                                 p(class(info),'the following organisms are associated with this disease. All column headings are sortable'),
                                 table(class('sortable std_table'),
                                       [tr([th('Organism'),th('Role')])
                                       |
                                       OrgRows
                                        ])
                                ]),
                              div(class(tabbertab),
                                  [h2('Phenotypes'),
                                   p(class(info),'the following phenotypes are associated with this disease (via an organism). All column headings are sortable'),
                                   table(class('sortable std_table'),
                                         [\phenotype_tblhdr,
                                          \phenotype_rows(Phenotypes)])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Info'),
                                   table([
                                         \class_annotation_assertions(Disease),
                                         \class_property_assertions(Disease)])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Similar'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   p('This tab allows you to view the underlying OWL (expert use only)'),
                                   \axiom_infos(Disease),
                                   \axiom_infos_references(Disease)
                                   ])
                             ])
                        ]).

disease_list([]) --> [].
disease_list([G|GL]) --> disease_href(G),[' '],disease_list(GL).

disease_href(X) -->
        {disease_label(X,Label) -> true ; Label=X},
        html(a(href(location_by_id(view_disease) + encode(X)),Label)).

% ----------------------------------------
% CLASSES
% ----------------------------------------

% this could form part of a generic ontology browser

class_hrefs_indented(Cs,CMap) :-
        solutions(C,
                  (   member(C,Cs),
                      \+ ((member(C2,Cs),
                           C2\=C,
                           atomic_subsumed_by(C,C2)))),
                  RootCs),
        class_hrefs_indented(RootCs,Cs,'',CMap).

class_hrefs_indented([],_,_,[]).
class_hrefs_indented([C|Roots],Cs,I,[C-span([I,\class_info(C)]) | CMap]) :-
        !,
        solutions(SubC,
                  (   member(SubC,Cs),
                      proper_subsumed_by(SubC,C),
                      \+ ((member(SubC2,Cs),
                           proper_subsumed_by(SubC2,C),
                           proper_subsumed_by(SubC,SubC2)))),
                  SubCs),
        debug(phenotype,'C: ~w subCs: ~w',[C,SubCs]),
        atom_concat('>',I,I2),
        class_hrefs_indented(SubCs,Cs,I2,CMap1),
        solutions(DC,member(DC-_,CMap1),DCs),
        subtract(Cs,DCs,NextCs),
        class_hrefs_indented(Roots,NextCs,I,CMap2),
        append(CMap1,CMap2,CMap).

% TOP-LEVEL: list all classes used in annotation
used_classes(_Request) :-
        solutions(C,(organism_phenotype_quad(_,P),class_quad_aspect(C,P,_),class(C)),Cs),
        length(Cs,NumCs),
        debug(phenotype,'Classes=~w',[NumCs]),
        findall(tr([td(Ont),
                    td(SuperClassCol),
                    td(\class_info(C)),
                    td(NumPhenotypes)]),
                (   member(C,Cs),
                    class_ontology(C,Ont),
                    % TODO: speed this up?
                    %aggregate(count,
                    %         P,
                    %         Org^Aspect^lookup_organism_by_inferred_class(C,Org,P,Aspect),
                    %         NumPhenotypes),
                    NumPhenotypes=0,
                    solutions(\class_info(Super),
                              (   subClassOf(C,Super),
                                  class(Super)),
                              SuperClassCol)),
                Rows),
        reply_html_page([ title('Ontology Classes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Ontology Classes'),
                          h2('Classes used in phenotype descriptions'),
                          table(class('sortable std_table'),
                                [tr([th('Ontology'),th('Superclass'),th('Class'),th('Phenotypes')])
                                |
                                 Rows])
                        ]).

% PHENO-SPECIFIC - hook?
used_class_info_table(Classes) -->
  html([h2('Classes used in phenotype descriptions'),
        table(class('sortable std_table'),
              [tr([th('Ontology'),th('Superclass'),th('Class'),th('Phenotypes')]),
               \used_class_info_rows(Classes)])]).

used_class_info_rows([]) --> [].
used_class_info_rows([C|Cs]) -->
        used_class_info_row(C),used_class_info_rows(Cs).
                          
% this should replace the above...
used_class_info_row(C) -->
        {class_ontology(C,Ont),
         NumPhenotypes='',
         solutions(\class_info(Super),
                   (   subClassOf(C,Super),
                       class(Super)),
                   SuperClassCol)},
        html(tr([td(Ont),
                 td(SuperClassCol),
                 td(\class_info(C)),
                 td(NumPhenotypes)])).

% ----------------------------------------
% TREE BROWSING
% ----------------------------------------

% TOP-LEVEL
browse_hierarchy(Request) :-
        http_parameters(Request,
                        [ open(OpenClasses)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title('Browser'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])

                        ],
                        [ \page_header('Ontology browser'),
                          h2('Browser'),
                          p(class(info),'NOT READY YET!!'),
                          \subclass_tree(OpenClasses)
                        ]).


ontol_selectbox(Param,Class) -->
        {debug(phenotype,' param(~w)=~w',[Param,Class]),
         %solutions(SuperClass,atomic_subsumed_by(Class,SuperClass),SuperClasses),
         solutions(SuperClass,(subClassOf(Class,SuperClass),class(SuperClass)),SuperClasses),
         solutions(SubClass,(subClassOf(SubClass,Class),class(SubClass)),SubClasses),
         append(SuperClasses,[Class|SubClasses],AllClasses),
         findall(option([value(X)|Selected],[XN]),
                 (   member(X,AllClasses),
                     (   X=Class
                     ->  Selected=[selected(yes)]
                     ;   Selected=[]),
                     display_label(X,XN)),
                 Opts)},
        html(select([name(Param)],
                    Opts)).

% ----------------------------------------
% GENERIC OWL
% ----------------------------------------
% move to owl/thea/clio...?

superclass_list(Class) -->
        {solutions(li(\class_info(Super)),subClassOf(Class,Super),List)},
        html(ul(List)).

equivalentclass_list(Class) -->
        {solutions(li(\class_info(Super)),equivalent_to(Class,Super),List)},
        html(ul(List)).

subclass_list(Class) -->
        {solutions(li(\class_info(Sub)),subClassOf(Sub,Class),List),
         debug(phenotype,'sc ~w => ~w',[Class,List])},
        html(ul(List)).

% TOP-LEVEL 
view_class(Request) :-
        request_path_local(Request,view_class,Class),
        view_class(Request,Class).

% redirect
view_class(Request,Class) :-
        reasoner_ask(subClassOf(Class,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_2')),
        \+ organism(Class),
        !,
        debug(phenotype,'rerouting to ~w',[Class]),
        view_organism_type(Request,Class).

        

view_class(_Request,Class) :-
        (   labelAnnotation_value(Class,Label)
        ->  true
        ;   Label=Class),
        debug(phenotype,'fetching relevant organisms for ~q',[Class]),
        solutions(Org-P-Aspect,lookup_organism_by_inferred_class(Class,Org,P,Aspect),OrgPsA),
        solutions(P-Aspect,member(_-P-Aspect,OrgPsA),Map),
        solutions(Org-P,member(Org-P-_,OrgPsA),OrgPs),
        solutions(Org,member(Org-P,OrgPs),Orgs),
        length(OrgPs,NumOrgPs),
        debug(phenotype,'num org-pheno pairs ~w',[NumOrgPs]),
        (   NumOrgPs>10         % set LOW for now
        ->  Matches=[]
        ;   debug(phenotype,'Finding matches (NumOrgPs=~w)',[NumOrgPs]),
            % TODO: cache this
            solutions(match(Org1,Org2,Match),
                      (   member(Org1,Orgs),
                          member(Org2,Orgs),
                          Org1@<Org2,
                          phenotype_lcs_organism_pair(Class,Org1,Org2,Match)),
                      Matches)),
        debug(phenotype,'fetching class assertions for ~q',[Class]),
        solutions(tr([td(\entity_info(I)),
		      td(\class_info(AssertedClass))]),
                  (   reasoner_ask(classAssertion(Class,I)),
                      classAssertion(AssertedClass,I)),
                  InstRows),
	length(InstRows,NumRows),
        debug(phenotype,'rendering ~q (~w rows)',[Class,NumRows]),
        reply_html_page([ title(['Class ',Label]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Ontology Class'),
                         h1(['Ontology Class: ',Label]),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Class)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Neurolex'),
                                    td(\neurolex_href(Class))])
                               ]),
                         h3('Parent classes'),
                         \superclass_list(Class),
                         h3('Equivalent classes'),
                         \equivalentclass_list(Class),
                         div(class(tabber),
                             [div(class(tabbertab),
                                [h2('Phenotypes'),
                                 table(class('sortable std_table'),
                                       [\organism_phenotype_tblhdr,
                                        \organism_phenotype_rows(OrgPs,Map)
                                       ])
                                ]),
                              div(class(tabbertab),
                                [h2('Matches'),
                                 p('Scroll down for documentation. Click column headings to sort.'),
                                 table(class('sortable std_table'),
                                       [\organism_phenotype_match_header,
                                        \organism_phenotype_match_rows(Matches)
                                       ]),
                                 div([id=info,class=infoBox],
                                     [
                                      h3('Documentation: Phenotype Pairs for a focus class'),
                                      p(['This view shows pairwise phenotypic similarity for the ontology class ',
                                         \class_info(Class),'. Each row shows a pair of phenotypes found in a pair of ',
                                         'organisms. These two phenotypes will be united by their relationship to ',\class_info(Class),'. ',
                                         'The degree to which the two phenotypes are related is given by the pairwise similarity score. ',
                                         'The ',a(href='http://en.wikipedia.org/wiki/Jaccard_index','Jacard Similarity'),' metric is used. ',
                                         'Note that whilst two individual phenotypes might be closely related, it does not mean the entire phenotypic profile pair ',
                                         'for the organism pair is closely related. The purpose of this view is to allow you to focus on particular aspects of ',
                                         'phenotypic similarity.']),
                                      p(['Note that this page will show no results if (a) there is only a single organism with a phenotype related to ',\class_info(Class),
                                         ' or (b) the class selected is so high level that there too high a number of combinatorial pairings to show'])
                                     ])
                                ]),
                              div(class(tabbertab),
                                  [h2('Info'),
                                   table(class('sortable std_table'),
                                         [
                                         \class_annotation_assertions(Class),
                                         \class_property_assertions(Class)])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Child classes'),
                                   \subclass_list(Class)]),
                              div(class(tabbertab),
                                  [h2('Instances'),
                                   p(class(info),'Inferred instances'),
                                   table(class('sortable std_table'),
                                         [tr([th('Instance'),
                                              th('Type (asserted)')])
                                         |
                                         InstRows])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Similar'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Diseases'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   \axiom_infos(Class),
                                   \axiom_infos_references(Class)
                                   ])
                             ])
                        ]).
        /*
        solutions(Aspect,member(_-_-Aspect,OrgPsA),Aspects),
        solutions(div(class(x),
                      [h3(Aspect),
                       table(class(sortable),
                             \organism_phenotype_rows(OrgPs))]),
                  (   member(Aspect,Aspects),
                      solutions(Org-P,
                                member(Org-P-Aspect,OrgPsA),
                                OrgPs)),
                  Divs),
        reply_html_page([ title(['Class ',Label])
                        ],
                        [ \class_detail(Class),
                          h2('phenotypes of relevance')|Divs]).
          */

view_instance(_Request,Instance) :-
        (   labelAnnotation_value(Instance,Label)
        ->  true
        ;   Label=Instance),
        findall(tr([td('Type'),td(\class_info(C))]),
                classAssertion(C,Instance),
                TypeRows),
        reply_html_page([ title(['Instance ',Label]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Instance'),
                         h1(['Ontology Instance: ',Label]),
                         p('You have burrowed down to the instance level. This is intended for debugging only.'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Instance)]),
                                tr([td('Label'),td(Label)])
                               |
                               TypeRows
                               ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2('Info'),
                                   table( \class_annotation_assertions(Instance)),
                                   table( \class_property_assertions(Instance))
                                  ]),
                              div(class(tabbertab),
                                  [h2('Similar'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Diseases'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   h3('About'),
                                   \axiom_infos(Instance),
                                   h3('References'),
                                   \axiom_infos_references(Instance)
                                   ])
                             ])
                        ]).

class_annotation_assertions(Class) -->
        {solutions(tr([td(\property_href(P)),td(V)]),
                   annotationAssertion(P,Class,literal(type(_,V))),
                   Rows)},
        html(Rows).

class_property_assertions(Class) -->
        {solutions(tr([td(\property_href(P)),td(V)]),
                   propertyAssertion(P,Class,literal(type(_,V))),
                   Rows)},
        html(Rows).


class_detail(Class) -->
        {labelAnnotation_value(Class,Label) -> true ; Label=Class},
        html(div(class(detail),
            [table(class(sortable),
                   [tr(td(id),td(Class)),
                    tr(td(label),td(Label))]),
             h2('axioms'),
             \axiom_infos(Class)]
           )).

inst_class_info(X) -->
        {classAssertion(Class,X)},
        !,
        html(\class_info(Class)).
inst_class_info(_X) -->
        html('?').

property_href(X) -->
        {display_label(X,Label)},
        !,
        html(a(href(location_by_id(view_entity) + encode(X)),Label)).

class_info_list_txt([]) --> [].
class_info_list_txt([C|L]) --> class_info_txt(C),[' '],class_info_list_txt(L).

class_info_txt(Class) -->
        {atom(Class)},
        !,
        {labelAnnotation_value(Class,Label) -> true ; Label=Class},        
        ['"',Label,'"'].
class_info_txt(intersectionOf(L)) -->
        ['IntersectionOf('],
        class_info_list_txt(L),
        [' )'].
class_info_txt(someValuesFrom(R,C)) -->
        class_info_txt(R),
        [' some '],
        class_info_txt(C).

class_info(Class,true) --> html(b(\class_info(Class))).
class_info(Class,false) --> html(\class_info(Class)).
        
class_info((-)) --> !,html('-').

class_info([]) --> !.
class_info([H]) --> !,class_info(H).
class_info([H|T]) --> !,class_info(H),[' * '],class_info(T).

class_info(Class) -->
        {atom(Class)},
	!,
	class_info_r(Class).
class_info(Class) -->
	class_info_r(Class),
	class_expression_tooltip.

class_info_r(Class) -->
        {atom(Class)},
	!,
        {labelAnnotation_value(Class,Label) -> true ; Label=Class},
        html(a(href(location_by_id(view_class) + encode(Class)),Label)).

class_info_r(intersectionOf(L)) -->
        {select(someValuesFrom(P,Y),L,[X])},
	!,
        class_info_simple_gd(X,P,Y).
class_info_r(intersectionOf([X])) -->
	!,
        class_info_r(X).
class_info_r(intersectionOf([X|L])) -->
	!,
        html([\class_info_r(X),
	      ' and ',
	      \class_info_r(intersectionOf(L))]).
class_info_r(someValuesFrom('http://purl.org/obo/owl/obo#towards',X)) -->
	!,
	class_info_r(X).
class_info_r(someValuesFrom(P,X)) -->
	!,
	html([\entity_info(P),' some ',\class_info_r(X)]).
class_info_r(Class) -->
        % generic class expression
        entity_info(Class).

% PKB SPECIFIC - obol hook?
% e.g. nitrated protein
class_info_simple_gd(X,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',Y) -->
	!,
        html([\class_info(Y),' ',\class_info(X)]).

class_info_simple_gd(X,R,Y) -->
	{rel_uri(part_of,R)},
	!,
        html([\class_info(X),' of ',\class_info(Y)]).

class_info_simple_gd(X,R,Y) -->
	{rel_uri(has_part,R)},
	!,
        html([\class_info(Y),' ',\class_info(X)]).
class_info_simple_gd(X,R,Y) -->
	html([\class_info(X),' that ',\entity_info(R),' some ',\class_info(Y)]).

class_expression_tooltip -->
	tooltip(['This is a ',i('class expression'),', ',
		 'which is a way of describing a complex entity by combining existing named classes from the ontology. ',
		 'In some cases, the least common subsumer of two existing classes may be a class expression. ',
		 'The class expression here is written in OWL Manchester Syntax. Future versions of OBD may display this in a more intuitive way.']).


rel_uri(part_of,'http://purl.org/obo/owl/OBO_REL#part_of').
rel_uri(part_of,'http://www.obofoundry.org/ro/ro.owl#part_of').
rel_uri(has_part,'http://purl.org/obo/owl/obo#has_part').
rel_uri(has_part,'http://www.obofoundry.org/ro/ro.owl#has_part').


axiom_infos(E) -->
        {solutions(li(\axiom_info(A)),axiom_directly_about(A,E),L)},
        html([p(['Axioms directly about ',\class_info(E)]),
              \tooltip(['All OWL axioms that describe the entity with URI ',E,'.',
                       '(Note that this view is mainly for debugging purposes)']),
              ul(L)]).
axiom_infos_references(E) -->
        {solutions(li(\axiom_info(A)),axiom_directly_references(A,E),L)},
        html([p(['Axioms referencing ',\class_info(E)]),
              \tooltip(['All OWL axioms that use the entity with URI ',E,'.',
                       '(Note that this view is mainly for debugging purposes)']),
             ul(L)]).


subclass_tree(OpenClasses) -->
        {findall(\subclass_tree(C,OpenClasses),(class(C),\+subClassOf(C,_)),Elts)},
        html([p(open),
              p(OpenClasses)|
              Elts]
            ).

subclass_tree(Class,OpenClasses) -->
        {
         (   member(Class,OpenClasses)
         ->  findall(\subclass_tree(SubClass,OpenClasses),
                     subClassOf(SubClass,Class),
                     ChildElts)
         ;   (   \+ \+ subClassOf(_,Class)
             ->  findall(open(C),member(C,[Class|OpenClasses]),Params),
                 ChildElts=[' ',a(href(location_by_id(browse_hierarchy)+Params),'+')]
             ;   ChildElts=[]))
         },
        html([ul(li([\class_info(Class)|
                     ChildElts]))]).

axiom_info(A) -->
        {A=..[P|L]},
        html([P,'( ',\axiom_args(L),' ) ',
              \axiom_ontology_info(A)]).

axiom_args([A]) --> html(\entity_info(A)).
axiom_args([A|L]) --> html([\entity_info(A), ', ', \axiom_args(L)]).

axiom_ontology_info(A) -->
        {setof(O,ontologyAxiom(O,A),Os)},
        !,
        html(span([class=axiom_ontology_list],
                  [ontology_list(Os)])).
axiom_ontology_info(_) --> [].

ontology_list(Onts) -->
        multi(div(\ontology_href(X)),X,Onts).

ontology_href(X) --> [X].


% ----------------------------------------
% REDIRECTION
% ----------------------------------------

% redirect
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        class(E),
        !,
        view_class(Request,E).
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        gene(E),
        !,
        view_gene(Request,E).
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        classAssertion(_,E),
        !,
        view_instance(Request,E).
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title(E)],
                        [ h2('Axioms'),
                          \axiom_infos(E),
                          h2('Referenced in'),
                          \axiom_infos_references(E)
                        ]).

% TODO - make this generic
neurolex_href(Class) -->
        {(   labelAnnotation_value(Class,Label)
         ->  true
         ;   Label=Class),
         sformat(URL,'http://www.neurolex.org/wiki/Category:~w',[Label])},
        html(a(href(URL),URL)).




%class_info(C) --> entity_info(C).

list_of_entities(L) --> html(ul(\entity_infos(L))).
entity_infos(L) --> multi(entity_info,L).

entity_info(literal(type(_,X))) --> !,html(X).
entity_info(literal(X)) --> !,html(X).


entity_info(C) -->
        {atom(C),
         display_label(C,Label)},
        !,
        html([' ',a(href(location_by_id(view_entity)+'?entity='+encode(C)),Label),' ']).
%        html(a(href(location_by_id(view_entity)+'entity='+encode(C)),C)).

entity_info(L) -->
        {is_list(L),
         findall(\entity_info(E),member(E,L),EL)},
        html(EL).

entity_info(A) -->
        {A=..[P|L]},
        html([P,'( ',\axiom_args(L),' )']).

composite_entity_info(E) --> html([' < ',\entity_info(E),'> ']).


% ----------------------------------------
% OTHER
% ----------------------------------------

% mixture of pkb-specific and owl-specific

display_label(C,Label) :- species_label(C,Label),!.
display_label(C,Label) :- labelAnnotation_value(C,Label),!. % TODO - speed this up
display_label(C,Label) :- entity_label(C,Label),!. % TODO - unify
display_label(C,Label) :- concat_atom([_,Label],'#',C),!.
display_label(C,C).


                  
 % HTTP UTIL

% TODO - use path_info instead?
request_path_local(Request,Loc,X) :-
        memberchk(path(ReqPath), Request),
	http_location_by_id(Loc, Me),
	atom_concat(Me, X, ReqPath).

% ----------------------------------------
% AUTOCOMPLETE
% ----------------------------------------
% taken from ClioPatria
% todo: refactor into general lib


% ----------------------------------------
% GENERIC UTIL
% ----------------------------------------

%% DCG predicate for generating sequences from lists
multi(Goal,Var,L) -->
        {findall(Goal,member(Var,L),Goals)},
        multi(Goals).

multi(P,L) -->
        {findall(\Goal,
                 (   member(X,L),
                     Goal=..[P,X]),
                 Goals)},
        multi(Goals).

multi(Goals) -->
        html(Goals).

% ----------------------------------------
% PHENOTYPE LOGIC
% ----------------------------------------

class_ontology(C,O) :- concat_atom([Base,_],'#',C),concat_atom(Parts,'/',Base),reverse(Parts,[O|_]),!.
class_ontology(_,unknown).

% TODO: this recapitulates some phenoblast logic, and does not take parthood into account

lookup_organism_by_inferred_class(Class,Org,P,Aspect) :-
	debug(temp,'  lookup: ~w',[Class]),
	atomic_subsumed_by(SubClass,Class),
	debug(temp,'    subclass: ~w',[SubClass]),
	lookup_organism_by_asserted_class(SubClass,Org,P,Aspect).
lookup_organism_by_inferred_class(Class,Org,P,Aspect) :-
	debug(temp,'  lookup: ~w',[Class]),
	reasoner_ask(subClassOf(SubClass,someValuesFrom(_,Class))),
	debug(temp,'    [r]subclass: ~w',[SubClass]),
	lookup_organism_by_asserted_class(SubClass,Org,P,Aspect).

% post
lookup_organism_by_asserted_class(Class,Org,P,Aspect) :-
	class_quad_aspect(Class,PQ,Aspect),
	phenotype_quad(P,PQ),
	organism_phenotype(Org,P).
% pre
lookup_organism_by_asserted_class(P,Org,P,phenotype) :-
	organism_phenotype(Org,P).

:- table_pred(lookup_organism_by_asserted_class/4).

% ----------------------------------------
% OWL/PHENO
% ----------------------------------------

% TODO: move to phenotype_db? (but this uses ontol_db model..)
% note: uses same pred name as simmatrix_multi
atomic_subsumed_by(X,Y) :- reasoner_ask(subClassOf(X,Y)).
atomic_subsumed_by(X,X).

% this is currently too slow...
%atomic_subsumed_by(X,Y) :- subClassOfRT(X,Y).

:- table_pred(atomic_subsumed_by/2).

proper_subsumed_by(X,Y) :- atomic_subsumed_by(X,Y),X\=Y.

%% subsumed_by_lca_set(+Classes:list,?AncClasses:list)
% nr set of atomic ancestors
% TODO - use new model
subsumed_by_lca_set(Cs,CAs) :-
        solutions(A,
                  (   member(C,Cs),
                      subsumed_by(C,A),
                      A\='-'),
                  As),
        findall(A,
                (   member(A,As),
                    \+ \+ ((member(C,Cs),
                            subsumed_by(C,A),
                           member(C2,Cs),
                            atomic_subsumed_by_lca(C,C2,A)))), % TODO: deprecate this
                CAs).


