:- module(quickterm,
          [
           valid_qtt/2,
           qtt_arg_type/3,
           qtt_description/2,
           qtt_wraps/2,
           qtt_external/2,
           template_request/3,
           template_resolve_args/4
          ]).

:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_obo)).
:- use_module(bio(metadata_db)).

% TODO
regrel(regulates).
regrel(negatively_regulates).
regrel(positively_regulates).

% ----------------------------------------
% TEMPLATES
% ----------------------------------------
%
% Tags:
%  * description
%  * ontology
%  * arguments - name=domain constraint list
%  * ontology - ID space
%  * wraps - optional list of sub-templates
%  * private - true if this is only accessible via a super-template
%  * cdef - template for logical definition, of form cdef(Genus,[R1=X1, ..., ])
%  * name - template for name
%  * synonyms - list of templates for synonyms
%  * def - template for textual definition
%  * requires - list of URLs of ontologies that must be imported. E.g. xp bridge files.

template(all_regulation(X),
         [
          description= 'Select all three subtemplates to generate
           terms for regulation, negative regulations and positive
           regulation. Names, synonyms and definitions are all
           generated automatically',
          ontology= 'GO',
          arguments= [target=biological_process],
          wraps= [regulation(X),
                  negative_regulation(X),
                  positive_regulation(X)]
         ]).
template(regulation(X),
         [
          ontology= 'GO',
          private= true,
          arguments= [target=biological_process],
          cdef= cdef('GO:0065007',[regulates=X]),
          name= ['regulation of ',name(X)],
          synonyms= ['regulation of ',synonym(X)],
          def= ['Any process that modulates the frequency, rate or extent of ',name(X),'.']
         ]).
template(negative_regulation(X),
         [
          ontology= 'GO',
          private= true,
          arguments= [target=biological_process],
          cdef= cdef('GO:0065007',[negatively_regulates=X]),
          name= ['negative regulation of ',name(X)],
          synonyms= ['negative regulation of ',synonym(X)],
          def= ['Any process that stops, prevents or reduces the frequency, rate or extent of ',name(X),'.']
         ]).
template(positive_regulation(X),
         [
          ontology= 'GO',
          private= true,
          arguments= [target=biological_process],
          cdef= cdef('GO:0065007',[positively_regulates=X]),
          name= ['positive regulation of ',name(X)],
          synonyms= ['positive regulation of ',synonym(X)],
          def= ['Any process that activates or increases the frequency, rate or extent of ',name(X),'.']
         ]).

template(involved_in(P,W),
         [
          description= 'processes involved in other processes',
          ontology= 'GO',
          requires= ['http://www.geneontology.org/scratch/xps/biological_process_xp_self.obo'],
          arguments= [part=biological_process,whole=biological_process],
          cdef= cdef(P,[part_of=W]),
          name= [name(P),' involved in ',name(W)],
          synonyms= [[synonym(P),' of ',synonym(W)]],
          def= ['Any ',name(P),' that is involved in ',name(W),'.']
         ]).

template(takes_place_in(P,C),
         [
          description= 'processes occurring in parts of the cell',
          ontology= 'GO',
          requires= ['http://www.geneontology.org/scratch/xps/biological_process_xp_cellular_component.obo'],
          arguments= [process=biological_process,location=cellular_component],
          cdef= cdef(P,['OBO_REL:occurs_in'=C]),
          name= [name(P),' in ',name(C)],
          synonyms= [[synonym(P),' in ',synonym(C)]],
          def= ['Any ',name(P),' that takes place in ',name(C),'.']
         ]).

template(part_of_cell_component(P,W),
         [
          description= 'cell components part of other cell components',
          ontology= 'GO',
          arguments= [part=cellular_component,whole=cellular_component],
          cdef= cdef(P,[part_of=W]),
          name= [name(W),' ',name(P)],
          synonyms= [[synonym(P),' of ',synonym(W)]],
          def= ['Any ',name(P),' that is part of a ',name(W),'.']
         ]).

template(protein_binding(X),
         [
          description= 'binding to a protein',
          ontology= 'GO',
          externals= ['PRO'],
          requires= ['http://www.geneontology.org/scratch/xps/molecular_function_xp_protein.obo'],
          arguments= [target='PRO'],
          cdef= cdef('GO:0005488',['OBO_REL:results_in_binding_of'=X]),
          name= [name(X),' binding'],
          synonyms= [[synonym(X),' binding']],
          def= ['Interacting selectively and non-covalently with ',name(X),'.']
         ]).

% TODO: merge plant/metazoan
template(metazoan_development(X),
         [
          description= 'development of an animal anatomical structure',
          ontology= 'GO',
          externals= ['UBERON'],
          requires= ['http://www.geneontology.org/scratch/xps/biological_process_xp_uber_anatomy.obo'],
          arguments= [target='UBERON'],
          cdef= cdef('GO:0032502',['OBO_REL:results_in_complete_development_of'=X]),
          name= [name(X),' development'],
          synonyms= [[synonym(X),' development']],
          def= ['The process whose specific outcome is the progression of ',refname(X),' over time, from its formation to the mature structure.']
                %def([' A ',name(X),' is '],X,'.')
         ]).

template(metazoan_morphologenesis(X),
         [
          description= 'morphogenesis of an animal anatomical structure',
          ontology= 'GO',
          externals= ['UBERON'],
          requires= ['http://www.geneontology.org/scratch/xps/biological_process_xp_uber_anatomy.obo'],
          arguments= [target='UBERON'],
          cdef= cdef('GO:0009653',['OBO_REL:results_in_morphogenesis_of'=X]),
          name= [name(X),' morphogenesis'],
          synonyms= [[synonym(X),' morphogenesis']],
          def= ['The developmental process by which ',refname(X),' is generated and organized.']
         ]).

template(plant_development(X),
         [
          description= 'development of a plant anatomical structure',
          ontology= 'GO',
          externals= ['PO'],
          requires= ['http://www.geneontology.org/scratch/xps/biological_process_xp_plant_anatomy.obo'],
          arguments= [target='UBERON'],
          cdef= cdef('GO:0032502',['OBO_REL:results_in_complete_development_of'=X]),
          name= [name(X),' development'],
          synonyms= [[synonym(X),' development']],
          def= ['The process whose specific outcome is the progression of ',refname(X),' over time, from its formation to the mature structure.']
                %def([' A ',name(X),' is '],X,'.')
         ]).

template(plant_morphologenesis(X),
         [
          description= 'morphogenesis of a plant animal anatomical structure',
          ontology= 'GO',
          externals= ['PO'],
          requires= ['http://www.geneontology.org/scratch/xps/biological_process_xp_plant_anatomy.obo'],
          arguments= [target='UBERON'],
          cdef= cdef('GO:0009653',['OBO_REL:results_in_morphogenesis_of'=X]),
          name= [name(X),' morphogenesis'],
          synonyms= [[synonym(X),' morphogenesis']],
          def= ['The developmental process by which ',refname(X),' is generated and organized.']
         ]).


template(abnormal_morphology(A),
         [
          ontology= 'HP',
          description= 'Abnormal X morphology',
          externals= ['FMA','PATO'],
          requires= ['http://compbio.charite.de/svn/hpo/trunk/human-phenotype-ontology_xp.obo'],
          arguments= [target='FMA'],
          cdef= cdef('PATO:0000051',['OBO_REL:inheres_in'=A]),
          name= ['Abnormal ',name(A),' morphology'],
          def= ['Any morphological abnormality of a ',name(A),'.']
         ]).

template(entity_quality(E,Q),
         [
          ontology= 'HP',
          description= 'basic EQ template',
          externals= ['FMA','PATO'],
          requires= ['http://compbio.charite.de/svn/hpo/trunk/human-phenotype-ontology_xp.obo'],
          arguments= [entity='FMA', quality='PATO'],
          cdef= cdef(Q,['OBO_REL:inheres_in'=E]),
          name= [name(Q),' ',name(E)],
          def= ['Any ',name(E),' that is ',name(Q)]
         ]).

template(metazoan_location_specific_cell(C,A),
         [
          ontology= 'CL',
          description= 'A cell type differentiated by its anatomical location (animals)',
          externals= ['UBERON'],
          arguments= [cell=C,location='UBERON'],
          cdef= cdef(C,[part_of=A]),
          name= [name(A),' ',name(C)],
          def= ['Any ',name(C),' that is part of a ',name(A),'.']
         ]).

template(cell_by_surface_marker(C,P),
         [
          ontology= 'CL',
          description= 'A cell type differentiated by proteins or complexes on the plasma membrane',
          externals= ['PRO','GO'],
          arguments= [cell=C,membrane_part=['PRO','GO:0032991']],
          cdef= cdef(C,[has_plasma_membrane_part=P]),
          properties = [multivalued(has_plasma_membrane_part),
                        any_cardinality(has_plasma_membrane_part)],
          name= [names(P),' ',name(C)],
          def= ['Any ',name(C),' that has ',names(P),' on the plasma membrane']
         ]).

template(structural_protein_complex(X,Y),
         [
          description= 'protein complex defined structurally',
          ontology= 'GO',
          externals= ['PRO'],
          %requires= ['http://www.geneontology.org/scratch/xps/cellular_component_xp_protein.obo'],
          arguments= [unit1='PRO',unit2='PRO'],
          cdef= cdef('GO:0043234',[has_part=X,has_part=Y]),
          name= [name(X),'-',name(Y),' complex'],
          synonyms= [[synonym(X),'-',synonym(Y),' complex']],
          def= ['Any protein complex consisting of a',name(X),' and a ',name(Y),'.']
         ]).

template(metazoan_location_specific_anatomical_structure(X,Y),
         [
          description= 'location-specific anatomical structure',
          ontology= 'UBERON',
          arguments= [part='UBERON',whole='UBERON'],
          cdef= cdef(P,[part_of=W]),
          name= [name(W),' ',name(P)],
          synonyms= [[synonym(P),' of ',synonym(W)]],
          def= ['Any ',name(P),' that is part of a ',name(W),'.']
         ]).

% ----------------------------------------
% CONFIG
% ----------------------------------------

ontology_repository('GO',cvs,'ext.geneontology.org:/share/go/cvs').
ontology_repository('CL',cvs,'obo.cvs.sourceforge.net:/cvsroot/obo').
ontology_repository('UBERON',git,'github.com:cmungall/uberon.git').
ontology_repository('HP',svn,'https://compbio.charite.de/svn/hpo/trunk').

ontology_xp_submit_path('GO','go/ontology/editors/xp_submit','go_xp').
ontology_xp_submit_path('CL','obo/ontology/anatomy/cell/xp_submit','CL_xp').
ontology_xp_submit_path('UBERON','uberon/xp_submit','UBERON_xp').
ontology_xp_submit_path('HP','hpo/xp_submit','HP_xp').

% ----------------------------------------
% extracting directory path
% ----------------------------------------

% TODO - use this
template_xp_submit_file(Template,Type,File,Opts) :-
        template_lookup(Template,ontology,Ont),
        ontology_xp_submit_path(Ont,Dir,Name),
        (   member(ontology_dir(Prefix),Opts)
        ->  true
        ;   Prefix='/users/cjm/cvs'),
        concat_atom([Prefix,'/',Dir,'/',Name,'_',Type,'.obo'],File).


% ----------------------------------------
% TEMPLATE LOOKUP
% ----------------------------------------

valid_qtt(T,S) :-
        template(TT,_),
        functor(TT,T,_),
        \+ template_lookup(T,private,true),
        template_lookup(T,ontology,S).

qtt_arg_type(T,A,Dom) :-
        template_lookup(T,arguments,AL),
        member(A=Dom,AL).

qtt_description(T,Desc) :-
        template_lookup(T,description,Desc).

qtt_ontology(T,Ont) :-
        template_lookup(T,ontology,Ont).

qtt_wraps(T,X) :-
        template_lookup(T,wraps,L),
        member(XT,L),
        functor(XT,X,_).

qtt_external(T,O) :-
        template_lookup(T,externals,L),
        member(O,L).

template_lookup(T,Key,Val) :-
        atom(T), % allow either template name or template term
        !,
        template(Template,KeyVals),
        functor(Template,T,_),
        member(Key=Val,KeyVals).
template_lookup(Template,Key,Val) :-
        template(Template,KeyVals),
        member(Key=Val,KeyVals).

% ----------------------------------------
% TEMPLATE -> PROLOG FACTS
% ----------------------------------------

%% generate_facts(+Template,+NewClass,?Facts,+Opts)
% given a template and a class ID, what are the ontol_db and metadata_db facts that
% can be generated from this?

generate_facts(_,New,[ontol_db:class(New)],_).

generate_facts(_,New,[metadata_db:entity_creation_date(New,D)],_) :-
        current_time_iso_full(D).

generate_facts(_,New,[metadata_db:entity_created_by(New,obol)],_).

generate_facts(Template,New,[metadata_db:entity_label(New,Name)],Opts) :-
        (   member(name(Name),Opts)
        ->  true
        ;   template_lookup(Template,name,NameT),
            generate_text(NameT,Name,Opts)).

generate_facts(Template,New,[metadata_db:entity_synonym(New,Syn),
                             metadata_db:entity_synonym_scope(New,Syn,Scope),
                             metadata_db:entity_synonym_xref(New,Syn,'OBOL:automatic')
                             ],Opts) :-
        template_lookup(Template,synonyms,SynsT),
        generate_text(SynsT,Syn,Scope,Opts),
        \+ ((template_lookup(Template,name,NameT),
             generate_text(NameT,Syn,Opts))).

generate_facts(Template,New,[ontol_db:def(New,Def),
                             ontol_db:def_xref(New,DX)
                            ],Opts) :-
        (   member(def(Def),Opts)
        ->  true
        ;   template_lookup(Template,def,DefT),
            generate_text(DefT,Def,Opts)),
        (   member(def_xref(DX),Opts)
        ->  true
        ;   DX='OBOL:automatic').

generate_facts(_,New,[metadata_db:entity_comment(New,X)],Opts) :-
        member(comment(X),Opts).

generate_facts(Template,New,[ontol_db:genus(New,Genus)],_) :-
        template_lookup(Template,cdef,cdef(Genus,_)).
generate_facts(Template,New,[ontol_db:differentium(New,R,X)],_) :-
        template_lookup(Template,cdef,cdef(_,DL)),
        member(R=X,DL).
generate_facts(Template,New,[ontol_db:restriction(New,R,X)],Opts) :-
        \+ member(suppress_relationship(true),Opts),
        template_lookup(Template,cdef,cdef(_,DL)),
        member(R=X,DL).

% single-valued
generate_fact(Template,New,Fact,Opts) :-
        generate_facts(Template,New,Facts,Opts),
        debug(quickterm,'  facts: ~w',[Facts]),
        member(Fact,Facts).

% ----------------------------------------
% REQUEST
% ----------------------------------------

%% template_request(+Template,?Msgs,+Opts)
%
% Opts:
%  * commit(Bool) - if true then sub add and del files are written
%  * subtemplate(ST) - zero or more.
%     if Template is a wrapper then subtemplates must be explicitly selected

% First check to make sure Mutex not owned
template_request(Template,error('Lock owned by someone else - try in 5 mins'),Opts) :-
        template_xp_submit_file(Template,submit,NF,Opts),
        atom_concat(NF,'-mutex',Mutex),
        exists_file(Mutex),
        time_file(Mutex,MT),
        get_time(T),
        DT is T-MT,
        DT =< 299, % assume stale after 5 mins
        !.

% lock Mutex and proceed to next step
template_request(MultiTemplate,Msgs,Opts) :-
        template_xp_submit_file(MultiTemplate,submit,NF,Opts),
        atom_concat(NF,'-mutex',Mutex),
        tell(Mutex),
        format('~w.~n',[MultiTemplate]),
        told,
        sformat(Cmd,'chmod 777 ~w',[Mutex]),
        shell(Cmd),

        % make sure previous submissions have been loaded,
        % so as to avoid duplicate requests
        (   exists_file(NF)
        ->  load_biofile(NF)
        ;   true),
        template_xp_submit_file(MultiTemplate,add,AF,Opts),
        (   exists_file(AF)
        ->  load_biofile(AF)
        ;   true),

        % loading everything in requires
        % (currently leaves externals up to caller.... TODO)
        template_lookup(MultiTemplate,requires,ReqURLs),
        maplist(load_biofile,ReqURLs),

        % now do the real deal...
        template_request_2(MultiTemplate,Msgs,Opts),

        % unlock system
        (   nonvar(Mutex)
        ->  catch(delete_file(Mutex),_,true)
        ;   true).

template_request_2(MultiTemplate,Msgs,Opts) :-
        % wrapper templates
        template_lookup(MultiTemplate,wraps,Templates),
        !,
        findall(Msg,(member(Template,Templates),
                     functor(Template,W,_),
                     member(subtemplate(W),Opts),
                     template_request_2(Template,Msg,Opts)),
                Msgs).
template_request_2(Template,error(term_with_same_logical_definition_exists(X)),_) :-
        template_lookup(Template,cdef,CDef),
        class_cdef(X,CDef),
        !.
template_request_2(Template,error(genus_may_not_match_differentia_term(G)),_) :-
        template_lookup(Template,cdef,cdef(G,DL)),
        member(_=G,DL),
        !.
template_request_2(Template,Msg,Opts) :-
        generate_id(Template,New,Opts),
        findall(Fact,generate_fact(Template,New,Fact,Opts),Facts),
        request_term_from_facts(Template,New,Facts,Msg,Opts).

request_term_from_facts(_Template,New,Facts,error(Err),_Opts) :-
        new_facts_error(New,Facts,Err),
        !.
request_term_from_facts(Template,New,Facts,Msg,Opts) :-
        template_lookup(Template,cdef,CDef),
        debug(quickterm,'placement: ~w',[CDef]),
        cdef_placement(CDef,Equivs,NRParents,NRChildren,RedundantSubclassPairs),
        debug(quickterm,'  placement: P:~w C:~w R:~w',[NRParents,NRChildren,RedundantSubclassPairs]),
        findall(ontol_db:subclass(New,Parent),member(Parent,NRParents),PFacts),
        findall(ontol_db:subclass(Child,New),member(Child,NRChildren),CFacts),
        flatten([Facts,PFacts,CFacts],NewFacts),
        findall(ontol_db:subclass(X,Y),member(X-Y,RedundantSubclassPairs),DeleteFacts),
        write_obo(New,NewFacts,DeleteFacts,Files,Opts),
        collect_files(Files,OboAtom),
        (   Equivs=[]
        ->  (   member(commit(true),Opts)
            ->  commit_files(Template,Files,Opts),
                Msg=ok(New,committed,OboAtom)
            ;   Msg=ok(New,uncommitted,OboAtom))
        ;   Equivs=[Equiv|_], % take the first one arbitrarily
            Msg=error(reasoner_inferred_equivalence_to_existing_class(Equiv))).


commit_files(Template,files(N,A,D),Opts) :-
        template_xp_submit_file(Template,submit,NF,Opts),
        template_xp_submit_file(Template,add,AF,Opts),
        template_xp_submit_file(Template,del,DF,Opts),
        !,
        append_to(N,NF),
        append_to(A,AF),
        append_to(D,DF).
commit_files(_,_,Opts) :-
        throw(error(must_specify_files_in_opts(Opts))).


append_to(A,F) :-
        append(F),
        nl,
        read_file_to_codes(A,Cs,[]),
        atom_codes(Atom,Cs),
        writeln(Atom),
        told.


collect_files(files(N,A,D),Atom) :-
        collect_file(N,NA),
        collect_file(A,AA),
        collect_file(D,DA),
        sformat(Atom,'!! SUBMISSION:~n~w~n~n!! AXIOMS TO ADD:~n~w~n~n!! AXIOM TO DELETED:~n~w~n',
                [NA,AA,DA]).


collect_file(F,A) :-
        read_file_to_codes(F,Cs,[]),
        atom_codes(A,Cs).

write_obo(New,NewFacts,DeleteFacts,files(NFile,AFile,DFile),_Opts) :-
        tmp_file(new_obo,NFile),
        tmp_file(add_obo,AFile),
        tmp_file(del_obo,DFile),
        forall(member(Fact,NewFacts),
               assert(Fact)),
        tell(NFile),
        write_class(obo,New),
        told,
        tell(AFile),
        forall((member(_:subclass(A,B),NewFacts),
                A\=New),
               write_axiom(obo,subclass(A,B))),
        told,
        tell(DFile),
        forall(member(_:subclass(A,B),DeleteFacts),
               write_retract_axiom(obo,subclass(A,B))),
        told.

write_date_as_comment :-
        current_time_iso_full(D),
        format('! Submission-date: ~w~n~n',[D]).


write_facts(File,Facts) :-
        open(File,write,IO,[]),
        forall(member(Fact,Facts),
               format(IO,'~q.~n',[Fact])),
        close(IO).


% check
new_facts_error(New,Facts,term_with_same_name_exists(X)) :- 
        member(_:entity_label(New,Name),Facts),
        entity_label(X,Name).
new_facts_error(New,Facts,cannot_generate_name) :- 
        \+ member(_:entity_label(New,_),Facts).
new_facts_error(New,Facts,cannot_generate_def) :- 
        \+ member(_:def(New,_),Facts).
new_facts_error(New,Facts,cannot_generate_logical_def) :- 
        \+ member(_:genus(New,_),Facts).
new_facts_error(New,Facts,cannot_generate_logical_def) :- 
        \+ member(_:differentium(New,_,_),Facts).

% ----------------------------------------
% IDs
% ----------------------------------------
generate_id(T,ID,Opts) :-
        (   member(idspace(S),Opts),
            nonvar(S)
        ->  true
        ;   qtt_ontology(T,S)
        ->  true
        ;   S='GO'),
        (   member(idnum_min(Num),Opts)
        ->  true
        ;   Num=2000001),
        get_next_id(S,Num,ID).

get_next_id(S,Num,ID) :-
        make_id(S,Num,GenID),
        class(GenID),
        !,
        Num2 is Num+1,
        get_next_id(S,Num2,ID).
get_next_id(S,Num,ID) :-
        make_id(S,Num,ID).

make_id(S,Num,ID) :-
        % assumes Num has most significant digit above zero, no padding required
        concat_atom([S,':',Num],ID).

% ----------------------------------------
% JSON MESSAGES
% ----------------------------------------

facts_json(New,NewFacts,DelFacts,[new=NewTVs,delete=DelTVs],Opts) :-
        findall(TV,(member(Fact,NewFacts),
                    fact_json(New,Fact,TV,Opts)),
                NewTVs),
        findall(TV,(member(Fact,DelFacts),
                    fact_json(New,Fact,TV,Opts)),
                DelTVs).

fact_json(_,_:Ax,T=V,_) :- Ax=..[T|V].

% ----------------------------------------
% TEXT GENERATION
% ----------------------------------------

generate_text(Toks,Atom,Opts) :-
        generate_text(Toks,Atom,_,Opts).

%% generate_text(+Toks,?Atom,?ScopeCombined,+Opts) is nondet
generate_text(Toks,Atom,ScopeCombined,Opts) :-
        debug(quickterm,'generating text: ~q',[Toks]),
        tokens_translate(Toks,ToksX,Scopes,Opts),
        combine_scopes(Scopes,ScopeCombined),
        debug(quickterm,'  ==> ~q',[ToksX]),
        flatten(ToksX,ToksY),
        concat_atom(ToksY,Atom).

%% tokens_translate(+Tin,?Tout,?Scopes,+Opts) is nondet
tokens_translate([],[],[],_) :- !.
tokens_translate([Tok|Toks],[TokX|ToksX],[S1|S2],Opts) :-
        !,
        tokens_translate(Tok,TokX,S1,Opts),
        tokens_translate(Toks,ToksX,S2,Opts).
tokens_translate(name(X),Name,name,_) :-
        entity_label(X,Name),
        !.
tokens_translate(refname(X),Phrase,name,_) :-
        entity_label(X,Name),
        (   starts_with_vowel(Name)
        ->  atom_concat('an ',Name,Phrase)
        ;   atom_concat('a ',Name,Phrase)),
        !.
tokens_translate(synonym(X),Name,Scope,_) :-
        !,
        (   entity_label(X,Name),
            Scope=name
        ;   entity_synonym_scope(X,Name,Scope)).
tokens_translate(def(Prefix,X,Suffix),DefSentence,def,Opts) :-
        !,
        (   def(X,Def)
        ->  tokens_translate([Prefix,Def,Suffix],DefSentence,_,Opts)
        ;   DefSentence='').
tokens_translate(X,X,exact,_) :-
        atom(X),
        !.
tokens_translate(X,_,_,_) :-
        print_message(error,quickterm(no_translation(X))),
        fail.

combine_scopes(Scopes,ScopeCombined) :-
        flatten(Scopes,L),
        (   forall(member(S,L),
                   (   S=name
                   ;   S=exact))
        ->  ScopeCombined=exact
        ;   member(ScopeCombined,L),
            forall(member(S,L),S=ScopeCombined)
        ->  true
        ;   ScopeCombined=related).

starts_with_vowel(A) :-
        sub_atom(A,0,1,_,X),
        member(X,[a,e,i,o,u]). % h?



% ----------------------------------------
% RESOLVING PARAMETERS
% ----------------------------------------
template_resolve_args(T,Params,Template,UnresolvedList) :-
        template_lookup(T,arguments,ArgDomains),
        params_args(ArgDomains,Params,Args,UnresolvedList),
        Template=..[T|Args].

params_args([],_,[],[]).
params_args([P=_|Doms],Params,[A|Args],UL) :-
        member(P=AN,Params),
        entity_label(A,AN),
        !,
        params_args(Doms,Params,Args,UL).
params_args([P=_|Doms],Params,[A|Args],UL) :-
        member(P=AN,Params),
        entity_synonym_scope(A,AN,exact),
        !,
        params_args(Doms,Params,Args,UL).
params_args([P=_|Doms],Params,Args,[AN|UL]) :-
        member(P=AN,Params),
        !,
        params_args(Doms,Params,Args,UL).
params_args([P=_|Doms],Params,Args,[P|UL]) :-
        !,
        params_args(Doms,Params,Args,UL).

/** <module> compositional class generation

---+ Synopsis

  See:
  http://www.berkeleybop.org/obo/quickterm/GO

---+ Details

 See also

  * ontol_restful.pro
  * ontol_webpages.pro

*/


            
        

        




