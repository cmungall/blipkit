
%:- use_module(bio(classbuilder)).  % store_cdef..
:- use_module(bio(classdef_parser)).
:- use_module(bio(tokenizer)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_obo)).
:- use_module(bio(mode)).
:- use_module(bio(serval)).
:- [amigo_src(amigo_term)].

http_param_label(parse_terms,'Parse Terms').
http_param_label(parse_term,parse_term).
http_param_label(parse_names,'Parse Names').
http_param_label(feeling_lucky,'I\'m feeling lucky').
http_param_label(select_parses,'Select these parses').

config_global_default(logo,'obol_logo.png').

send_option(term,parse_terms).

init_hook:-
        preprocess_tokens(pre).

:- multifile entry_page/2.      % TODO - registry
entry_page(obol_main,'Obol').
obol_main =>
 doc:'ask for names',
 outer('Obol',
       sform(obol,[],
             html:h4('WebObols WObol but they don\'t fall down'),
             html:p('List of term names'),
             html:textarea(rows=10,cols=40,name=names),
             %span(checkbox(stemming_on,1),'Use stemming?'),
             submit(parse_names),
             submit(feeling_lucky),
             select_ontol_checkboxes)).

show_parses( Ps ) =>
 doc:'shows all parses of sentences; asks for correct ones',
 outer('Parse results',
       sform(obol,[],
             %getparam(pstructs,Ps),
             debug(obol,'pstructs=~w',[Ps]),
             log(ps=Ps),
             if(Ps=[],
                then: html:h4('No names entered'),
                else: html:div(class=results,
                               show_parses1(P) forall_unique member(P,Ps),
                               log(abc),
                               submit(select_parses)))))
 :: pre(S,
        (   submit_param(S,parse_names),
            lgetparam(S,ontology_id,Onts),
            ngetparam(S,stemming_on,Stem),
            getparam(S,names,Names),
            parse_input(Names,Ps,Onts,[stem(Stem)]))).
     

store_selected_parses =>
 doc:'filtered cdef set, stored in local db',
 outer('Selected parses',
       sform(obol,[],
             getparam(selected_parses,NCs),
             log(cdefs=NCs),
             if(NCs=[],
                then: html:h4('None selected'),
                else: html:div(class=results,
                               ufindall(if(class(ID,N),
                                           then: href_data_item(term,ID,N),
                                           else: html:b(N)),
                                        member(ID-_-_,NCs)),
                               ufindall(div(class=obo_format,
                                            pre(class=obo,
                                                call(write_class(obo,ID)))),
                                        member(ID-_N-_C,NCs)),
                               submit(select_parses))))).

select_ontol_checkboxes =>
 doc:'ontology subset to use for making cdefs',
 if(setof(On,ID^belongs(ID,On),Ons),
    then: html:ul(findall(li(checkbox(ontology_id,On,true),On),
                          member(On,Ons)))).

% parse(Name,Cs) Cs=[Num-score(Score,Cdef),...]
show_parses1(pstruct(N,Parses)) =>
 doc:'shows all parses of one sentences',
 html:div(class=info,
          html:p('Parses for: ',
                 if(class(ID,N),
                    then: href_data_item(term,ID,N),
                    else: html:b(N))),
          log(xyz),
          map(parse(Score,Num,C),
              show_cdef_opt(C,Num,Score), % todo: more RESTful
              Parses)).

show_cdef_opt(C,N1-N2,Score) =>
 doc:'shows cdef plus option',
 log(foo),
 debug(obol,'showing ~w ~w ~w',[C,N1-N2,Score]),
 html:div(class=cdef,
          log(c(C,N1-N2)),
          call(concat_atom([N1,N2],'-',Num)),
          call(atom_concat(parse_,N1,RadioButtonName)),
%          call(atom_to_term(C,CDefAtom,_)),
%          input(type=textfield,name=p,value=CDefAtom),
          %input(type=radio,name=RadioButtonName,value=CDefAtom),
          input(type=radio,name=RadioButtonName,value=test),
%          checkbox(cdef_opt_id,
%                   Num,
%                   (N2=1)),
          %%Num,':',
          show_cdef(C),
          html:span(class=score,'score: ',Score)).

show_cdef(C) =>
 doc:'pretty print class def; recursive',
 if(C=cdef(G,DL),
    then:
   if(DL=[],
      then: show_genus(G),
      else: html:span(show_genus(G),
                      show_differentia(DL))),
    else: % new form: cdef can be primitive
   if(atom(C),
      then: show_genus(C))).


show_genus(anon(N,On)) =>
 doc:'shows genus term in classdef',
 N,
 in_ontology(On).
show_genus(cdef(GID,DL)) =>
 doc:'shows composite genus term in classdef',
 show_cdef(cdef(GID,DL)).
show_genus(GID) =>
 doc:'shows primitive genus term in classdef',
 if((belongs(GID,On),
     class(GID,GN)),
    then: [href_data_item(term,GID,GN),in_ontology(On)],
    else: '??').
show_differentia(DL) =>
 doc:'shows list of differentiating characteristics',
 ul(html:i(' *which* '),
    findall(li(data(T),':',
               if(C2=cdef(_,[]),
                  then:
                 show_cdef(C2),
                  else:
                 ul(li(show_cdef(C2))))),
            member(T=C2,DL))).

in_ontology(On) =>
 html:span(class=smallnote,
           '  (in ',html:i(On),')').

% REGISTERING HOOKS

amigo_hook(term_extra_info(ID),term_obol_parse_link(ID)).

term_obol_parse_link(ID) =>
 doc:'creates a link to parse a term',
 call(obolify_button_img_url(Src)),
 log(Src),
 ilink(img(class=button,src=Src),
       submit=parse_term,id=ID).

obolify_button_img_url(URL):- make_url('amigo2/images/obolify.gif',URL).

% CONTROLLER 
 
% PARSE IDs
xxstrans(show_parses,S,
       % pre: multiple IDs selected
       (   getparam(S,action,parse_terms),
           lgetparam(S,ontology_id,Onts),
           ngetparam(S,stemming_on,Stem),
           lgetparam(S,id,IDs)),
       % post
       (   userlog(IDs),
           parse_ids(IDs,Ps,Onts,[stem(Stem)])),

       % change
       add([[pstructs,Ps]])).

% PARSE IDs
strans(show_parses,S,
       % pre: an ID is selected
       (   submit_param(S,parse_term),
           lgetparam(S,ontology_id,Onts),
           ngetparam(S,stemming_on,Stem),
           getparam(S,id,ID),
           (class(ID,N);synonym(ID,exact,N))),
       % post
       (   userlog(N),
           parse_input(N,Ps,Onts,[stem(Stem)])),

       % change
       add([[pstructs,Ps]])).

% PARSE NAMES
strans(show_parses,S,
       % pre: a list of terms typed in to box
       (   submit_param(S,parse_names),
           lgetparam(S,ontology_id,Onts),
           ngetparam(S,stemming_on,Stem),
           getparam(S,names,Names)),
       % post
       (   parse_input(Names,Ps,Onts,[stem(Stem)])),

       % change
       add([[pstructs,Ps]])).

strans(store_selected_parses,S,
       % pre:
       (   submit_param(S,feeling_lucky), % choose the first one at random
           lgetparam(S,ontology_id,Onts),
           ngetparam(S,stemming_on,Stem),
           getparam(S,names,Names)),
       % post
       (   parse_input(Names,Ps,Onts,[stem(Stem)]),
           findall(N-C,
                   member(pstruct(N,[parse(_,_,C)|_]),Ps), % first
                   NCs),
           true),

       % change
       add([[selected_parses,NCs]])).

strans(store_selected_parses,S,
       % pre:
       (   submit_param(S,select_parses),
           getparam(S,pstructs,Ps), % previously parsed
           lgetparam(S,cdef_opt_id,Nums)), % selected 
       % post
       (   userlog(nums=Nums),
           findall(ID-N-C,(member(pstruct(N,Parses),Ps),
                           member(parse(_,N1-N2,C1),Parses),
                           concat_atom([N1,N2],'-',Num),
                           member(Num,Nums),
                           userlog(storing(C1,ID,C)),
                           store_cdef(C1,ID,C)),
                   NCs),
           userlog(ncs=NCs),
           true),

       % change
       add([[selected_ids,Nums],[selected_parses,NCs]])).

% (+,?,+)
%  Parses=[parses(Name,[Score-CDef,...]),...]
parse_input(NamesAtom,Ps,Onts,Opts):-
        tok_atom(NamesAtom,Names,[ws([10,13])]),  % split on newline
        parse_names(Names,Ps,Onts,Opts).

% parse_names(+Names,?PStructs,+Onts,+Opts)
parse_names(Names,Ps,Onts,Opts):-
        number_list_items(Names,NumberedNames),
        findall(P,
                (   member(Num-N,NumberedNames),
                    userlog(parsing(Num-N)),
                    name_to_pstruct(N,Num,P,Onts,Opts)),
                Ps).

parse_ids(IDs,Ps,Onts,Opts):-
        number_list_items(IDs,NumberedIDs),
        findall(P,
                (   member(Num-ID,NumberedIDs),
                    id_to_pstruct(ID,Num,P,Onts,Opts)),
                Ps).

% name_to_pstruct(+Name,+Num,?PStruct,+Onts,+Opts)
% PStruct = pstruct(Name,[parse(Score,Num-InnerNum,ClassDef),...])
% 
name_to_pstruct(N,Num,pstruct(N,Parses),Onts,_Opts):-
        solutions(score(Score,C),
                  (   name_to_cdef_via_cfg(N,C),
                      \+ atom(C),
                      userlog(classdef=C),
                      %is_cdef_allowed(C,Onts),
                      %cdef_score(C,Score)),
                      cdef_fitness(C,Score)),
                  Pairs),
        userlog(parse_pairs=Pairs),
        sort(Pairs,SPairs),
        reverse(SPairs,RSPairs),
        number_list_items(RSPairs,NRSPairs),
        findall(parse(Score,Num-InnerNum,C),
                member(InnerNum-score(Score,C),NRSPairs),
                Parses).

id_to_pstruct(ID,Num,pstruct(PrimaryName,Parses),Onts,_Opts):-
        class(ID,PrimaryName),
        solutions(score(Score,C),
                  (   class_label(ID,N,exact),
                      name_to_cdef(N,C),
                      userlog(classdef=C),
                      is_cdef_allowed(C,Onts),
                      cdef_score(C,Score)),
                  Pairs),
        userlog(parse_pairs=Pairs),
        sort(Pairs,SPairs),
        reverse(SPairs,RSPairs),
        number_list_items(RSPairs,NRSPairs),
        findall(parse(Score,Num-InnerNum,C),
                member(InnerNum-score(Score,C),NRSPairs),
                Parses).

%TODO - denormalize (otherwise can't IDify)
% true when cdef is in list of selected ontologies
:- mode is_cdef_allowed(+,+) is semidet.
is_cdef_allowed(_,[all]):- !.
is_cdef_allowed(_,[]):- !.
is_cdef_allowed(C,Onts):-
        userlog(checking_allowed(C,Onts)),
        C=cdef(_G,DL),
%        cdef_ont(C,Ont),
        member(Ont,Onts),
        forall(member(_=C2,DL),
               is_cdef_allowed(C2,Onts)),
        userlog(allowed(C)).
