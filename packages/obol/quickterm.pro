:- module(quickterm,
          [
           quickterm_template/1,
           qtt_arg_type/3,
           qtt_description/2,
           qtt_wraps/2,
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
          description= 'generates 3 regulation terms',
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

% ----------------------------------------
% TEMPLATE LOOKUP
% ----------------------------------------


quickterm_template(T) :-
        template(TT,_),
        functor(TT,T,_),
        \+ template_lookup(T,private,true).


qtt_arg_type(T,A,Dom) :-
        template_lookup(T,arguments,AL),
        member(A=Dom,AL).

qtt_description(T,Desc) :-
        template_lookup(T,description,Desc).

qtt_wraps(T,X) :-
        template_lookup(T,wraps,L),
        member(XT,L),
        functor(XT,X,_).
        

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

generate_facts(Template,New,[metadata_db:entity_comment(New,X)],Opts) :-
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
template_request(_,error('Lock owned by someone else - try in 5 mins'),Opts) :-
        member(subfile(NF),Opts),
        nonvar(NF),
        atom_concat(NF,'-mutex',Mutex),
        exists_file(Mutex),
        time_file(Mutex,MT),
        get_time(T),
        DT is T-MT,
        DT =< 299, % assume stale after 5 mins
        !.

% lock Mutex and proceed to next step
template_request(MultiTemplate,Msgs,Opts) :-
        (   member(subfile(NF),Opts),
            nonvar(NF)
        ->  atom_concat(NF,'-mutex',Mutex),
            tell(Mutex),
            format('~w.~n',[MultiTemplate]),
            told,
            sformat(Cmd,'chmod 777 ~w',[Mutex]),
            shell(Cmd)
        ;   true),
        
        (   member(subfile(NF),Opts),
            nonvar(NF),
            exists_file(NF)
        ->  load_biofile(NF)
        ;   true),
        (   member(addfile(AF),Opts),
            nonvar(AF),
            exists_file(AF)
        ->  load_biofile(AF)
        ;   true),
        template_request_2(MultiTemplate,Msgs,Opts),
        (   nonvar(Mutex)
        ->  catch(delete_file(Mutex),_,true)
        ;   true).


template_request_2(Template,_Msgs,_Opts) :-
        template_lookup(Template,requires,URLs),
        maplist(load_biofile,URLs),
        fail.
template_request_2(MultiTemplate,Msgs,Opts) :-
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
template_request_2(Template,Msg,Opts) :-
        generate_id(Template,New,Opts),
        findall(Fact,generate_fact(Template,New,Fact,Opts),Facts),
        request_term_from_facts(Template,New,Facts,Msg,Opts).

request_term_from_facts(_Template,New,Facts,error(Errs),_Opts) :-
        setof(Err,new_facts_error(New,Facts,Err),Errs),
        !.
request_term_from_facts(Template,New,Facts,Msg,Opts) :-
        template_lookup(Template,cdef,CDef),
        debug(quickterm,'placement: ~w',[CDef]),
        cdef_placement(CDef,_Equivs,NRParents,NRChildren,RedundantSubclassPairs),
        debug(quickterm,'  placement: P:~w C:~w R:~w',[NRParents,NRChildren,RedundantSubclassPairs]),
        findall(ontol_db:subclass(New,Parent),member(Parent,NRParents),PFacts),
        findall(ontol_db:subclass(Child,New),member(Child,NRChildren),CFacts),
        flatten([Facts,PFacts,CFacts],NewFacts),
        findall(ontol_db:subclass(X,Y),member(X-Y,RedundantSubclassPairs),DeleteFacts),
        %facts_json(New,NewFacts,DeleteFacts,JSON,Opts),
        write_obo(New,NewFacts,DeleteFacts,Files,Opts),
        collect_files(Files,OboAtom),
        (   member(commit(true),Opts)
        ->  commit_files(Files,Opts),
            Msg=ok(New,committed,OboAtom)
        ;   Msg=ok(New,uncommitted,OboAtom)).

commit_files(files(N,A,D),Opts) :-
        member(subfile(NF),Opts),
        append_to(N,NF),
        member(addfile(AF),Opts),
        append_to(A,AF),
        member(delfile(DF),Opts),
        append_to(D,DF),
        !.
commit_files(_,Opts) :-
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

        

write_facts(File,Facts) :-
        open(File,write,IO,[]),
        forall(member(Fact,Facts),
               format(IO,'~q.~n',[Fact])),
        close(IO).


% check
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
generate_id(_,ID,Opts) :-
        (   member(idspace(S),Opts)
        ->  true
        ;   true),
        (   var(S)
        ->  S='GO'
        ;   true),
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
tokens_translate(synonym(X),Name,Scope,_) :-
        !,
        (   entity_label(X,Name),
            Scope=name
        ;   entity_synonym_scope(X,Name,Scope)).
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


            
        

        




