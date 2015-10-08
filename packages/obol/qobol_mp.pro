:- module(qobol_mp,
          [
           parse_entity/5,
           show_class_parse_mismatch/2,
           suggest_term/5,
           qobol_process_all/1,
           write_new_parsed_expressions/0,
           write_new_parsed_expressions/1,
           qobol_prep/0,
           qobol_prep/1,
           qobol_index/1
           ]).

:- use_module(library(thea2/owl2_model),[]).
:- use_module(library(thea2/owl2_plsyn)).
:- use_module(bio(io)).
:- use_module(bio(bioprolog_util),[call_unique/1]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_writer_obo)).

qobol_index(Opts) :-
        \+ member(noindex(true),Opts),
        !,
        debug(qobol,'Indexing...',[]),
        materialize_index(metadata_db:entity_label_scope(1,1,-)),
        materialize_index(metadata_db:entity_label_scope_dn(1,1,-)).

qobol_index(_).

qobol_prep :-
        qobol_prep([]).
qobol_prep(Opts) :-
        \+ nb_current(qobol_opts,_),
        nb_setval(qobol_opts,Opts),
        debug(qobol,'Setting opts: ~w',[Opts]),
        (   member(lexical_variant(IsLV),Opts)
        ->  nb_setval(use_lexical_variants,IsLV)
        ;   nb_setval(use_lexical_variants,false)),
        fail.
qobol_prep(Opts) :-
        member(taxon(_),Opts),
        load_bioresource(gotax),
        load_bioresource(taxslim),
        load_bioresource(taxunion),
        fail.
qobol_prep(Opts) :-
        member(ontology(Ontology),Opts),
        member(xont(XOntology),Opts),
        !,
        qobol_prep_ont(Ontology,XOntology).
qobol_prep(Opts) :-
        member(ontology(Ontology),Opts),
        !,
        qobol_prep_ont(Ontology).
qobol_prep(_Opts) :- true.


qobol_prep_ont('TO') :- prep_plant_trait,!.
qobol_prep_ont(mp) :- prep_mp_all,!.
qobol_prep_ont('MP') :- prep_mp_all,!.
qobol_prep_ont(hp) :- prep_hp_all,!.
qobol_prep_ont('HP') :- prep_hp_all,!.
qobol_prep_ont('ZFA') :- load_bioresource('ZFA'), !.
qobol_prep_ont('BBQ') :- load_bioresource(bbq),load_bioresource(uberonp),load_bioresource(go),load_bioresource(pato),load_bioresource('CL'), !.
qobol_prep_ont('MA') :- load_bioresource('MA'),load_bioresource(uberonp), !.
qobol_prep_ont('UBERON') :- load_bioresource(uberonp), !.
qobol_prep_ont('GO') :- load_bioresource(go),load_bioresource(goxp(relations_process_xp)),!.
qobol_prep_ont('CL') :- load_bioresource(cell),!.
%qobol_prep_ont('DOID') :- load_bioresource(disease_xp),load_bioresource(fma),load_bioresource(cell).
qobol_prep_ont('VT') :- load_bioresource(vt),load_bioresource(vt_xp),load_bioresource(go),load_bioresource(protein),load_bioresource(chebi),load_bioresource(pato),load_bioresource('CL'),load_bioresource(uberonp), !.
qobol_prep_ont(Ont) :- atom(Ont),load_bioresource(Ont),!.
qobol_prep_ont(_).
%qobol_prep_ont(_) :- prep_mp_all,!.

qobol_prep_ont('GO','UBERON') :-
        load_bioresource(go),
        load_bioresource(uberonp),
        !.
qobol_prep_ont('GO','CL') :-
        load_bioresource(go),
        load_bioresource('CL'),
        !.
qobol_prep_ont('GO','PO') :-
        load_bioresource(go),
        load_bioresource(plant_anatomy),
        %load_bioresource(goxp(biological_process_xp_plant_anatomy)),
        load_bioresource(goxp(relations_process_xp)),
        !.
qobol_prep_ont('GO','PR') :-
        load_bioresource(go),
        load_bioresource(protein),
        load_bioresource(goxp(relations_process_xp)),
        !.

qobol_prep_ont('GO','CHEBI') :-
        load_bioresource(go),
        load_bioresource(chebi),
        load_bioresource(ions),
        load_bioresource(goxp(relations_process_xp)),
        !.


prep :-
        load_bioresource(obol_av).

prep_plant_trait :-
        prep,
        load_bioresource(pato),
        load_bioresource(chebi),
        load_bioresource(go),
        load_bioresource('PO'),
        load_bioresource(plant_trait),
        load_bioresource(plant_trait_xp).

prep_mp :-
        prep,
        load_bioresource(pato),
        load_bioresource(mammalian_phenotype),
        load_bioresource(mammalian_phenotype_xp).

prep_mp_all :-
        prep_mp,
        load_bioresource(go),
        load_bioresource(protein),
        load_bioresource(chebi),
        load_bioresource(cell),
        load_bioresource(mpath),
        load_bioresource(disease),
        load_bioresource(uberonp),
        load_bioresource(mouse_anatomy).

prep_hp :-
        prep,
        load_bioresource(pato),
        load_bioresource(human_phenotype),
        load_bioresource(human_phenotype_xp).

prep_hp_all :-
        prep_hp,
        load_bioresource(go),
        load_bioresource(protein),
        load_bioresource(chebi),
        load_bioresource(cell),
        load_bioresource(uberonp),
        load_bioresource(fma_downcase).


molecular(C) :- in(C,'CHEBI',E),class(Atom,atom),debug(qobol,'?is_molecular: ~w // ~w',[C,E]),\+subclassT(E,Atom).
molecular(C) :- in(C,'PR').
molecular(C) :- in(C,'GO',E),subclassT(E,'GO:0032991'). % MM complex



%% qobol(?Categories:list, ?MatchTemplate:list, ?OWLExpression, ?MatchGoal, ?ValidGoal)
:- discontiguous qobol/5.

% ----------------------------------------
% TRAIT
% ----------------------------------------

% e.g. X sensitivivity
qobol([plant,trait,chemical],
      [C,Q],
      Q and (inheres_in some 'whole plant') and (towards some C),
      in(Q,'PATO'),
      in(C,'CHEBI')).
qobol([plant,trait,anatomy],
      [E,Q],
      Q and (inheres_in some E),
      in(Q,'PATO'),
      in(E,'PO')).
qobol([bbq,trait,anatomy],
      [E,Q],
      Q and (inheres_in some E),
      true,
      %in(Q,'PATO'),
      true).

% ----------------------------------------
% SUPER-GENERIC
% ----------------------------------------

qobol([gemet,generic,no_constraint],
      [A,B],
      A and related_to some B,
      true,
      true).
qobol([gemet,generic,no_constraint],
      [A,to,B],
      A and related_to some B,
      true,
      true).



% ----------------------------------------
% TRAITS
% ----------------------------------------

qobol([vt,generic,anatomy],
      [A,trait],
      'biological attribute' and occurs_in some A,
      true,
      in(A,'UBERON')).

qobol([vt,generic,process],
      [A,trait],
      'biological attribute' and attribute_of some A,
      true,
      in(A,'GO')).

qobol([vt,eq],
      [E,Q],
      'biological attribute' and affects_quality some Q and attribute_of some E,
      in(Q,'PATO'),
      in(E,['CHEBI','PR','UBERON','CL','GO'])).

qobol([vt,eq,form2],
      [E,Q,trait],
      'biological attribute' and affects_quality some Q and attribute_of some E,
      in(Q,'PATO'),
      in(E,['CHEBI','PR','UBERON','CL','GO'])).

% blood aldosterone amount
qobol([vt,amount],
      [Tissue,Substance,amount],
      'biological attribute' and affects_quality some amount and attribute_of some Substance and occurs_in some Tissue,
      true,
      in(Tissue,['UBERON'])).

qobol([vt,quantity],
      [A,quantity],
      'biological attribute' and affects_quality some amount and attribute_of some A,
      true,
      true).


% 
qobol([vt,physiology],
      [A,physiology,trait],
      'biological attribute' and affects_quality some 'phsyiological state' and attribute_of some A,
      true,
      true).






% ----------------------------------------
% METABOLISM
% ----------------------------------------

qobol([mp,metabolic,level,abnormal],
      [abnormal,circulating,C,level/levels],
      'concentration of' and (qualifier some abnormal) and ( inheres_in some blood) and (towards some C),
      true,
      in(C,_)).

qobol([mp,metabolic,level,abnormal],
      [abnormal,E,C,level/levels],
      'concentration of' and (qualifier some abnormal) and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)).

qobol([mp,metabolic,level,abnormal],
      [abnormal,C,level/levels],
      'concentration of' and (qualifier some abnormal) and (towards some C),
      true,
      in(C,_)).

qobol([mp,metabolic,level,changed],
      [IncOrDec,circulating,C,level/levels],
      Q and ( inheres_in some blood) and (towards some C),
      true,
      in(C,_)) :-
        pato(concentration,IncOrDec,Q).


qobol([mp,metabolic,level,changed],
      [IncOrDec,E,C,level/levels],
      Q and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)) :-
        pato(concentration,IncOrDec,Q).


qobol([mp,metabolic,level,changed],
      [IncOrDec,C,level/levels],
      Q and (towards some C),
      true,
      in(C,_)) :-
        pato(concentration,IncOrDec,Q).

% number
qobol([mp,metabolic,anatomy,number,changed],
      [IncOrDec,E,C,number],
      Q and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)) :-
        pato(number,IncOrDec,Q).
qobol([mp,metabolic,basic,number,changed],
      [IncOrDec,C,number],
      Q and (towards some C),
      true,
      in(C,_)) :-
        pato(number,IncOrDec,Q).

% abnormal processes and functions
qobol([mp,process,abnormal],
      [abnormal,P],
      quality and (qualifier some abnormal) and (Rel some P),
      (   in(P,'GO',PID),
          (   belongs(PID,biological_process),
              Rel=inheres_in_part_of
          ;   belongs(PID,molecular_function),
              Rel=inheres_in)),
      true).
/*
qobol([mp,process,abnormal],
      [abnormal,P],
      quality and (qualifier some abnormal) and (inheres_in some P),
      (   in(P,'GO',PID),
          (   belongs(PID,biological_process)
          ;   belongs(PID,molecular_function))),
      true).
*/


% ----------------------------------------
% MORPHOLOGY
% ----------------------------------------


qobol([mp,morphology,abnormal],
      [abnormal,C,morphology],
      morphology and (qualifier some abnormal) and (inheres_in some C),
      true,
      true).

qobol([mp,absent,singular],
      [absent,C],
      'lacks all parts of type' and (towards some C),
      true,
      true).
qobol([mp,absent,plural],
      [absent,C+s],
      'lacks all parts of type' and (towards some C),
      true,
      true).

% note: will not catch: abnormal X function
qobol([mp,eav,abnormal],
      [abnormal,C,A],
      A and (qualifier some abnormal) and (inheres_in some C),
      entity_partition(A,attribute_slim),
      in(A,'PATO')).


qobol([mp,magnitude,changed],
      [Mag,C,A],
      Q and (inheres_in some C),
      (   in(A,'PATO',AX),entity_partition(AX,attribute_slim)),
      atomic_list_concat([Mag,A],' ',Q)
     ) :- magdiff(Mag).

qobol([mp,rate,processual],
      [increased,E],
      'increased rate' and (inheres_in some E),
      true,
      in(E,'GO')).
qobol([mp,rate,processual],
      [decreased,E],
      'decreased rate' and (inheres_in some E),
      true,
      in(E,'GO')).

qobol([mp,impaired,processual],
      [impaired,E],
      'disrupted' and (inheres_in some E),
      true,
      in(E,'GO')).

qobol([mp,branching,involved,processual],
      [Q,branching,involved,in,E],
      Q and (inheres_in some 'morphogenesis of a branching structure') and (inheres_in_part_of some E),
      true,
      in(E,'GO')).

qobol([mp,physiology],
      [abnormal,E,physiology],
      functionality and (inheres_in some E) and (qualifier some abnormal),                         
      true,
      true).

/*
qobol([mp,development,processual],
      [Q,E,development],
      Q and (inheres_in some 'morphogenesis of a branching structure') and (inheres_in_part_of some E),
      true,
      in(E,'GO')).
*/

qobol([mp,eq,process,quality],
      [E,P],
      Q and (inheres_in some E),
      true,
      true) :-
        process_quality(P,Q).

% increased DO incidence
qobol([mp,incidence,inc,disease,quality],
      [increased,D,incidence],
      'increased rate' and (towards some D),
      true,
      true).
qobol([mp,incidence,dec,disease,quality],
      [decreased,D,incidence],
      'decreased rate' and (towards some D),
      true,
      true).

% e.g. X atrophied
qobol([mp,eq,processual],
      [E,P],
      Q and (inheres_in some E),
      true,
      true) :-
        process_quality(P,Q).

% e.g. Q E
qobol([mp,eq,generic],
      [Q,E],
      Q and inheres_in some E,
      in(Q,'PATO'),
      true).

% eg coloboma of the iris
qobol([mp,coloboma],
      [coloboma,of,the,A],
      coloboma and  (inheres_in some A),
      true,
      true).


/*
qobol([mp,persistent,embryonic,plural],
      [persistent,C],
      'persistent TODO' and (inheres_in some C),
      true,
      true).
*/

% ----------------------------------------
% HP
% ----------------------------------------

qobol([hp,quality,simple,plural,abnormal],
      [abnormality,of/involving,opt(the),plural(E)],
      quality and (qualifier some abnormal) and (inheres_in some E),
      true,
      true).
qobol([hp,quality,simple,singular,abnormal],
      [abnormality,of/involving,opt(the),E],
      quality and (qualifier some abnormal) and (inheres_in some E),
      true,
      true).
qobol([hp,quality,combined,plural,abnormal],
      [abnormality,of/involving,opt(the),opt_plural(E1),and,opt_plural(E2)],
      quality and (qualifier some abnormal) and (inheres_in some E1) and (inheres_in some E2),
      true,
      true).

% e.g. abnormal size of the Xs
qobol([hp,eav,simple,plural,abnormal],
      [abnormal,A,of,opt(the),plural(E)],
      A and (qualifier some abnormal) and (inheres_in some E),
      label_partition(A,attribute_slim),
      true).
qobol([hp,eav,simple,singular,abnormal],
      [abnormal,A,of,opt(the),E],
      A and (qualifier some abnormal) and (inheres_in some E),
      label_partition(A,attribute_slim),
      true).

qobol([hp,sclerosis,abnormal],
      ['Sclerosis',of,the,E],
      'increased mass density' and (qualifier some pathological) and (inheres_in some E),
      true,
      true).
qobol([hp,symphalangism,abnormal],
      [symphalangism,affecting,the,E],
      'fused with' and (towards some phalanx) and (inheres_in some E),
      true,
      true).
qobol([hp,symphalangism,abnormal],
      [symphalangism,of,the,E],
      'fused with' and (towards some phalanx) and (inheres_in some E),
      true,
      true).

/*
qobol([hp,generic,abnormal],
      [P,of,the,E],
      PClass and (inheres_in some E),
      true,
      subclassT(E,Parent)) :- hp_generic(P,PClass,Parent).
*/

qobol([hp,quality,part,plural,abnormal],
      [abnormality,involving/of,opt(the),opt_plural(P),of,the,opt_plural(W)],
      quality and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      true,
      true).
qobol([hp,quality,part,plural,abnormal],
      [abnormality,involving/of,opt(the),W,opt_plural(P)],
      quality and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      true,
      true).

qobol([hp,eav,part,plural,abnormal],
      [abnormal,A,of,the,opt_plural(P),of,the,opt_plural(W)],
      A and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      label_partition(A,attribute_slim),
      true).
qobol([hp,eav,part,plural,abnormal],
      [abnormal,A,of,the,W,opt_plural(P)],
      A and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      label_partition(A,attribute_slim),
      true).

% ----------------------------------------
% DISEASE
% ----------------------------------------

qobol([disease,anatomical,basic],
      [disease,of,E],
      disease and located_in some EX,
      in(E,['FMA','UBERON'],EX),
      true).
qobol([disease,anatomical,basic],
      [E,disease],
      disease and located_in some EX,
      in(E,['FMA','UBERON'],EX),
      true).

qobol([disease,cell,basic],
      [disease,of,E],
      disease and located_in some EX,
      in(E,'CL',EX),
      true).
qobol([disease,cell,basic],
      [E,disease],
      disease and located_in some EX,
      in(E,'CL',EX),
      true).

qobol([disease,go,basic],
      [disease,of,E],
      disease and located_in some EX,
      in(E,'GO',EX),
      true).
qobol([disease,go,basic],
      [E,disease],
      disease and located_in some EX,
      in(E,'GO',EX),
      true).


qobol([disease,anatomical,generic],
      [E,D],
      DX and located_in some EX,
      (   in(E,['FMA','UBERON','CL'],EX),
          dmap(D,DX),
          in(DX,['DOID'])
      ),
      true).
qobol([disease,anatomical,generic],
      [D,of,E],
      DX and located_in some EX,
      (   in(E,['FMA','UBERON'],EX),
          dmap(D,DX),
          in(DX,['DOID'])
      ),
      true).

dmap('carcinoma in situ','in situ carcinoma').
dmap(D,D).



% ----------------------------------------
% GO
% ----------------------------------------

% RoBQ
qobol([go,bp,regulation,quality],
      [regulation,of,Q],
      'biological regulation' and regulates some Q,
      true,
      true). % GO:0065008 ! regulation of biological quality

qobol([go,bp,regulation,by],
      [Reg1,by,Reg2],
      Reg2 and (results_in some Reg1),
      true,
      subclassT(Reg1,'GO:0065007')). % biological regulation

qobol([mf,binding,domain],
      [P,binding],
      binding and (has_input some P),
      true,
      true).

qobol([mf,binding,protein],
      [P,binding],
      binding and (has_input some P),
      true,
      in(P,'PR')).
qobol([mf,receptor_activity,protein],
      [P,'receptor activity'],
      'receptor activity' and (has_active_participant some P),
      true,
      in(P,'PR')).
qobol([mf,receptor_activity,chemical],
      [P,'receptor activity'],
      'receptor activity' and (has_input some P),
      true,
      in(P,'CHEBI')).

qobol([go,bp,transport,from,to],
      [S,to,E,transport],
      'transport' and has_target_start_location some S and has_target_end_location some E,
      true,
      true).

qobol([go,bp,transport,import_export,mol],
      [C,export],
      'transport' and exports some C,
      true,
      molecular(C)).
qobol([go,bp,transport,import_export,mol],
      [C,import],
      'transport' and imports some C,
      true,
      molecular(C)).

% check - e.g. dopamine uptake
qobol([go,bp,transport,uptake,mol],
      [C,uptake],
      'transport' and imports some C,
      true,
      molecular(C)).

/*
qobol([go,bp,transport,import_export,chemical,transmembrane],
      [C,transmembrane, transport],
      'transmembrane transport' and exports some C,
      true,
      in(C,'CHEBI')).
qobol([go,bp,transport,import_export,chemical,transmembrane],
      [C,import],
      'transmembrane transport' and imports some C,
      true,
      in(C,'CHEBI')).
*/

qobol([go,bp,transport,import_export,mol,transmembrane],
      [C,transmembrane,import,into,E],
      'transmembrane transport' and imports some C and has_target_end_location some E,
      true,
      molecular(C)).
qobol([go,bp,transport,import_export,mol,transmembrane],
      [C,transmembrane,export,from,E],
      'transmembrane transport' and exports some C and has_target_start_location some E,
      true,
      molecular(C)).



% foo('0').
%snRNA import into Cajal body
%qobol([go,bp,import],
%      [M,import,into,C],
%      transport and (transports_or_maintains_localization_of some M) and (results_in_transport_to some C),
%      true,
%      true).
qobol([go,bp,transport,import,into],
      [M,import,into,C],
      transport and (imports some M) and (has_target_end_location some C),
      true,
      molecular(M)).
qobol([go,bp,transport,export,from],
      [M,export,from,C],
      transport and (exports some M) and (has_target_start_location some C),
      true,
      molecular(M)).
% e.g. Golgi calcium ion export
qobol([go,bp,transport,export,implicit],
      [C,M,export],
      transport and (exports some M) and (has_target_start_location some C),
      true,
      molecular(M)).

% e.g. plasma membrane acetate transport - TODO - check it's a membrane
%qobol([go,bp,transport,implicit],
%      [C,M,transport],
%      transport and (transports_or_maintains_localization_of some M) and (results_in_transport_across some C),
%      true,
%      molecular(M)).

% e.g. ER to Golgi ceramide transport
qobol([go,bp,transport,path,mol],
      [S,to,E,M,transport],
      transport and (transports_or_maintains_localization_of some M) and (has_target_start_location some S) and (has_target_end_location some E),
      true,
      (molecular(M), in(S,'GO'), in(E,'GO'))).


qobol([go,bp,transport,transepithelial],
      [transepithelial,M,transport],
      'transepithelial transport' and (transports_or_maintains_localization_of some M),
      true,
      molecular(M)).

% TODO: patterns for portein targeting etc
qobol([go,bp,generic_transport,generic],
      [G,to,C],
      G and (has_target_end_location some C),
      true,
      true).

qobol([go,bp,generic_transport,import_export,into],
      [P,into,C],
      P and has_target_end_location some C,
      true,
      subclassT(P,'GO:0006810')).
% foo('0').


qobol([go,mf,transport,antiporter],
      [M1,':',M2,antiporter,activity],
      'antiporter activity' and (imports some M1) and (exports some M2),
      true,
      (   molecular(M1),molecular(M2))
      ).
qobol([go,mf,transport,symporter],
      [M1,':',M2,symporter,activity],
      'symporter activity' and (exports some M1) and (imports some M2),
      true,
      (   molecular(M1),molecular(M2))
      ).


qobol([go,mf,transport,channel],
      [M,channel,activity],
      'channel activity' and transports_or_maintains_localization_of some M,
      true,
      molecular(M)).

qobol([go,mf,transport],
      [M,transporter,activity],
      'transporter activity' and transports_or_maintains_localization_of some M,
      true,
      molecular(M)).
qobol([go,mf,transport],
      [M,transmembrane,transporter,activity],
      'transmembrane transporter activity' and transports_or_maintains_localization_of some M,
      true,
      molecular(M)).



% TODO
%qobol([go,transport,localization,establishment],
%      [protein,localization,to,C],
%      'protein localization' and has_target_end_location some C,
%      true,
%      true).

qobol([go,bp,involved],
      [P,involved,in,W],
      P and part_of some W,
      true,
      true).

qobol([go,bp,apoptosis],
      [C,P],
      P and occurs_in some C,
      true,
      in(C,'CL')) :-
        member(P,['cell death','programmed cell death','apoptotic process']).



qobol([go,bp,generic,P,anatomy],
      [embryomic,C,P],
      PClass and (R some C) and (part_of some 'embryo development'),
      true,
      in(C,Ont)) :- bp_generic(P,PClass,R,Ont,anatomy).

qobol([go,bp,generic,P,Type],
      [C,P],
      PClass and R some C,
      true,
      in(C,Ont)) :- bp_generic(P,PClass,R,Ont,Type).

qobol([go,bp,generic,P,Type],
      [P,of,C],
      PClass and R some C,
      true,
      in(C,Ont)) :- bp_generic(P,PClass,R,Ont,Type).

qobol([go,bp,generic,from,P,Type],
      [C,P,from,C2],
      PClass and (R some C) and (has_input some C2),
      true,
      in(C,Ont)) :- bp_generic(P,PClass,R,Ont,Type).

qobol([go,bp,cc,reladj,occurs],
      [Adj,P],
      P and occurs_in some C,
      true,
      (   in(P,'GO',PID),
          in(C,'GO',CID),
          belongs(PID,biological_process),
          belongs(CID,cellular_component))) :- reladj(Adj,C).

reladj(mitochondrial,mitochondrion).
reladj(cytoplasmic,cytoplasm).
reladj(cytosolic,cytosol).
reladj(intracellular,intracellular).
reladj(vacuolar,vacuole).
reladj(pancreatic,pancreas).

qobol([go,cc,lumen],
      [W,lumen],
      'membrane-enclosed lumen' and (part_of some W),
      true,
      (   in(W,'GO',WID),
          belongs(WID,cellular_component))).

qobol([go,cc,membrane,p1],
      [W,P],
      P and (bounding_layer_of some W),
      true,
      (   in(P,'GO',PID),
          in(W,'CL'),
          belongs(PID,cellular_component))).


qobol([go,cc,part_of,cell],
      [W,P],
      P and (part_of some W),
      true,
      (   in(P,'GO',PID),
          in(W,'CL'),
          belongs(PID,cellular_component))).


qobol([go,bp,cc,in,occurs],
      [C,P],
      P and occurs_in some C,
      true,
      (   in(P,'GO',PID),
          in(C,'GO',CID),
          belongs(PID,biological_process),
          belongs(CID,cellular_component))).


% with generic tag, avoid ambiguous parses
qobol([go,bp,anat,generic,occurs],
      [C,P],
      P and occurs_in some C,
      true,
      in(C,['UBERON','CL'])) :- \+ bp_generic(P,_,_,_,_).

% no generic tag, force occurs. E.g. paraxial mesoderm cell differentiation
%  NOTE: this fails because 'mesoderm cell differentiation' is found by greedy match
%qobol([go,bp,occurs,weak],
%      [C,P],
%      P and occurs_in some C,
%      true,
%      in(C,['UBERON'])).


% e.g. regulation of transcription by galactose
qobol([go,bp,by,has_input,chemical],
      [P,by,C],
      P and has_input some C,   % TODO - check
      true,
      in(C,['CHEBI','PR'])).

% methionine catabolic process to 3-methylthiopropanoate
qobol([go,bp,to,has_output,has_input,chemical],
      [In,P,to,Out],
      P and has_input some In and has_output some Out,
      true,
      in(In,['CHEBI','PR'])).

% glutamate catabolic process via 2-oxoglutarate
qobol([go,bp,via,has_output,has_input,chemical],
      [In,P,via,Via],
      P and has_input some In and has_intermediate some Via, % TODO
      true,
      in(In,['CHEBI','PR'])).

% phosphatidylcholine biosynthesis from choline
qobol([go,bp,from,has_output,has_input,chemical],
      [Out,biosynthesis,from,In],
      'biosynthetic process' and has_input some In and has_output some Out,
      true,
      in(In,['CHEBI','PR'])).

qobol([go,bp,stimulus,response,chemical],
      [response,to,C,stimulus],
      'response to stimulus' and has_input some C,
      true,
      in(C,['CHEBI','PR'])).

qobol([go,bp,stimulus,response,chemical],
      [response,to,C],
      'response to stimulus' and has_input some C,
      true,
      in(C,['CHEBI','PR'])).

qobol([cellular,go,bp,stimulus,response,chemical],
      [cellular,response,to,C,stimulus],
      'cellular response to chemical stimulus' and has_input some C,
      true,
      in(C,'CHEBI')).
qobol([cellular,go,bp,stimulus,response,chemical],
      [cellular,response,to,C],
      'cellular response to chemical stimulus' and has_input some C,
      true,
      in(C,'CHEBI')).


% WARNING: use this only with -subclass GO:0019725  ! cellular homeostasis 
qobol([go,bp,homeostasis,occurs,cell],
      [C,homeostasis],
      'cellular homeostasis' and occurs_in some C,
      true,
      in(C,'CL')).

% WARNING: use this only with -subclass GO:0048872 ! homeostasis of number of cells 
qobol([go,bp,homeostasis,levels,cell],
      [C,homeostasis],
      'homeostasis of number of cells' and regulates_level_of some C,
      true,
      in(C,'CL')).

% WARNING: use this only with -subclass GO:0048872 ! homeostasis of number of cells 
%qobol([go,bp,homeostasis,occurs,levels,cell],
%      [C,homeostasis],
%      %'homeostasis of number of cells' and regulates_level_of some C,
%      'homeostasic process' and regulates_level_of some C,
%      true,
%      true).



qobol([go,bp,homeostasis,chemical],
      [C,homeostasis],
      'chemical homeostasis' and regulates_level_of some C,
      true,
      in(C,'CHEBI')).

qobol([go,bp,cellular,homeostasis,chemical],
      [cellular,C,homeostasis],
      'cellular chemical homeostasis' and regulates_level_of some C,
      true,
      in(C,'CHEBI')).

% FF

qobol([ff],
      [cellular,C,homeostasis],
      'cellular chemical homeostasis' and regulates_level_of some C,
      true,
      in(C,'CHEBI')).


% HP generic
% Symphalangism affecting the phalanges of the hallux
%hp_generic('Symphalangism affecting',PClass,Parent).

% notes: activation has overlap with differentiation and proliferation
% notes: called bp_generic but also used for mf
bp_generic(differentiation,'cell differentiation',results_in_acquisition_of_features_of,['CL','PO'],cell).
bp_generic('fate specification','cell fate specification',results_in_specification_of,['CL','PO'],cell).
bp_generic('fate determination','cell fate determination',results_in_determination_of,['CL','PO'],cell).
bp_generic(proliferation,'cell proliferation',acts_on_population_of,['CL','PO'],cell).
bp_generic(activation,'cell activation',has_input,['CL','PO'],cell).
bp_generic(migration,'cell migration',has_input,['CL','PO'],cell).
bp_generic(development,'cell development',results_in_development_of,['CL','PO'],cell).
bp_generic(growth,'growth',results_in_growth_of,['CL','PO'],cell).
bp_generic(maturation,'developmental maturation',results_in_maturation_of,['CL','PO'],cell).
bp_generic('cell death','developmental maturation',results_in_ending_of,['CL','PO','UBERON'],cell).

bp_generic(morphogenesis,'anatomical structure morphogenesis',results_in_morphogenesis_of,['CL','UBERON','PO'],anatomy).
bp_generic('structural arrangement','anatomical structure arrangement',results_in_arrangement_of,['CL','UBERON','PO'],anatomy).
bp_generic(formation,'anatomical structure formation involved in morphogenesis',results_in_formation_of,['CL','UBERON','PO'],anatomy).
bp_generic(growth,'developmental growth',results_in_growth_of,['CL','UBERON','PO'],anatomy).
bp_generic(maturation,'anatomical structure maturation',results_in_maturation_of,['UBERON','PO'],anatomy).
bp_generic(induction,'developmental induction',induces,['CL','UBERON','PO'],anatomy).
bp_generic(migration,'tissue migration',has_input,['UBERON','PO'],anatomy).
bp_generic(regression,'anatomical structure regression',directly_involves,['UBERON','PO'],anatomy).
bp_generic(senescence,'organ senescence',results_in_ending_of,['PO','UBERON'],cell).
bp_generic(development,'anatomical structure development',results_in_development_of,['CL','UBERON','PO'],anatomy).

bp_generic(fusion,'organelle fusion',results_in_fusion_of,['GO'],cell_component).
bp_generic(assembly,'cellular component assembly',results_in_assembly_of,['GO'],cell_component).
bp_generic(disassembly,'cellular component disassembly',results_in_disassembly_of,['GO'],cell_component).

bp_generic(organization,'cellular component organization',results_in_organization_of,['GO'],cell_component).

bp_generic(metabolism,'metabolic process',has_participant,['CHEBI','PR'],chemical).
bp_generic('metabolic process','metabolic process',has_participant,['CHEBI','PR'],chemical).
bp_generic(catabolism,'catabolic process',has_input,['CHEBI','PR'],chemical).
bp_generic('catabolic process','catabolic process',has_input,['CHEBI','PR'],chemical).
bp_generic(biosynthesis,'biosynthetic process',has_output,['CHEBI','PR'],chemical).
bp_generic('biosynthetic process','biosynthetic process',has_output,['CHEBI','PR'],chemical).

bp_generic('transmembrane transport','transmembrane transport',transports_or_maintains_localization_of,['CHEBI','PR','GO'],chemical).
bp_generic(transport,transport,transports_or_maintains_localization_of,['CHEBI','PR','GO'],chemical).
bp_generic(transporter,transporter,transports_or_maintains_localization_of,['CHEBI','PR','GO'],chemical).
bp_generic(binding,binding,has_input,['CHEBI','PR','GO'],chemical).
bp_generic(export,transport,exports,['CHEBI','PR','GO'],chemical).
bp_generic(import,transport,imports,['CHEBI','PR','GO'],chemical).
bp_generic(secretion,secretion,transports_or_maintains_localization_of,['CHEBI','PR','GO'],chemical).

test_bp_generic(AX,BX,X) :-
        bp_generic(_,A,_,_,_),
        bp_generic(_,B,_,_,_),
        A@<B,
        class(AX,A),
        class(BX,B),
        subclassRT(X,AX),
        subclassRT(X,BX),
        \+ ((subclassT(X,Y),
             subclassRT(Y,AX),
             subclassRT(Y,BX))).


% ----------------------------------------
% ANATOMY
% ----------------------------------------

qobol([anatomy,future],
      [future,X],
      Parent and (develops_into some X),
      true,
      (   in(X,_Ont,XC),
          subclass(XC,Parent))).

qobol([anatomy,generic,part],
      [W,P],
      P and (part_of some W),
      true,
      true).


qobol([cell,generic,part],
      [W,P],
      P and (part_of some W),
      true,
      (   in(P,'CL'),
          in(W,'UBERON'))
      ).

qobol([cell,generic,part],
      [P,of,opt(the),W],
      P and (part_of some W),
      true,
      (   in(P,'CL'),
          in(W,'UBERON'))
      ).

qobol([anatomy,nif,regional,part],
      [regional,part,of,X],
      'anatomical structure' and (part_of some X),
      true,
      in(X,'UBERON')).


% requires secreted_by - can get from uberon. e.g.
% obol -r uberonp -ontology MA -subclass MA:0002450 -tag secretion -undefined_only true -export obo
qobol([anatomy,ma,fluid,secretion],
      [X,'fluid/secretion'],
      G and (secreted_by some X),
      true,
      true) :- class(G,'body fluid or substance').

qobol([anatomy,uberon,interdigital],
      [interdigital,region,between,Digit,X,and,Y],
      'interdigital region' and adjacent_to some DX and adjacent_to some DY,
      digit_number(Y),
      (   atomic_list_concat([Name,X],' ',DX),
          atomic_list_concat([Name,Y],' ',DY)
      )
     ) :-
        digit_name(Digit,Name).

digit_name(digits,digit).
digit_name(fingers,'hand digit').
digit_name(toes,'foot digit').
digit_number('1').
digit_number('2').
digit_number('3').
digit_number('4').
digit_number('5').

qobol([anatomy,uberon,subdivision,spine],
      [Adj,X],
      X and part_of some Region, % not quite right...
      true,
      true
     ) :-
        spinal_region(Adj,Region).

spinal_region(cervical,'cervical region').
spinal_region(thoracic,'thoracic region').
spinal_region(lumbar,'lumbar region').
spinal_region(sacral,'sacral region').
spinal_region(caudal,'caudal region').
spinal_region(coccygeal,'caudal region').



% ----------------------------------------
% TEMPLATE LOGIC
% ----------------------------------------

magdiff(increased).
magdiff(decreased).

pato(concentration,increased,'increased concentration').
pato(concentration,decreased,'decreased concentration').

pato(number,increased,'has extra parts of type').
pato(number,decreased,'has fewer parts of type').
pato(number,abnormal,'altered number of').

process_quality(degeneration,degenerate).
process_quality(atrophy,atrophied).
process_quality(fusion,fused).
process_quality(persistence,persistent).
process_quality(hyperplasia,hyperplastic).
process_quality(hypoplasia,hypoplastic).


class_category(C,mp) :- id_idspace(C,'MP').
class_category(C,metabolic) :- class(P,'homeostasis/metabolism phenotype'),subclassRT(C,P).

% ----------------------------------------
% OPTION PROCESSING
% ----------------------------------------

opts_excluded_class(E,Opts) :-
        member(id(X),Opts),
        X\=E.
opts_excluded_class(E,Opts) :-
        member(satisfies(A),Opts),
        atom_to_term(A,E-Goal),
        \+ Goal.
opts_excluded_class(E,Opts) :-
        member(ontology(Ont),Opts),
        \+ id_idspace(E,Ont).
opts_excluded_class(E,Opts) :-
        member(idspace(Ont),Opts),
        \+ id_idspace(E,Ont).
opts_excluded_class(E,Opts) :-
        member(undefined_only(true),Opts),
        genus(E,_),
        print_message(informational,already_defined(E)).
opts_excluded_class(E,Opts) :-
        member(subclass(X),Opts),
        \+ subclassRT(E,X).
opts_excluded_class(E,Opts) :-
        setof(C,member(union(C),Opts),Cs),
        \+ ((member(C,Cs),
             subclassRT(E,C))).
opts_excluded_class(E,Opts) :-
        member(part_of(X),Opts),
        \+ parentRT(E,part_of,X).
opts_excluded_class(E,Opts) :-
        member(not_subclass(X),Opts),
        \+ \+ subclassRT(E,X).
opts_excluded_class(E,Opts) :-
        member(taxon(Tax),Opts),
        entity_not_in_taxon(E,Tax).

opts_included_class(E,Opts) :-
        \+ opts_excluded_class(E,Opts).

opts_allowed_scope(label,_) :- !.
opts_allowed_scope(_,Opts) :-
        member(scope(all),Opts),
        !.
opts_allowed_scope(exact,Opts) :-
        \+ member(scope(no_exact),Opts),
        !.
opts_allowed_scope(Scope,Opts) :-
        member(scope(Scope),Opts).

entity_not_in_taxon(E,Tax) :-
        parentRT(E,X),
        restriction(X,only_in_taxon,TaxR),
        \+ subclassRT(Tax,TaxR).
entity_not_in_taxon(E,Tax) :-
        parentT(E,X),
        restriction(X,never_in_taxon,TaxR),
        subclassRT(Tax,TaxR).

        
category_match(CatTags,Opts) :-
        member(tags(InTags),Opts),
        !,
        forall(member(Tag,InTags),member(Tag,CatTags)),
        \+ category_exclude(CatTags,Opts).
category_match(CatTags,Opts) :-
        % ALL tags must match, if tags are specified
        setof(Tag,member(tag(Tag),Opts),InTags),
        !,
        forall(member(Tag,InTags),member(Tag,CatTags)),
        \+ category_exclude(CatTags,Opts).
category_match(CatTags,Opts) :-
        setof(Tag,member(oneof(Tag),Opts),InTags),
        !,        
        member(Tag,InTags),
        member(Tag,CatTags),
        \+ category_exclude(CatTags,Opts).
category_match(CatTags,Opts) :-
        \+ category_exclude(CatTags,Opts).


category_exclude(CatTags,Opts) :-
        member(xtag(Tag),Opts),
        member(Tag,CatTags).


% ----------------------------------------
% USER GOALS
% ----------------------------------------

qobol_process_all(Opts) :-
        call_unique(ontol_db:class(E)),
        qobol_process_class(E,Opts),
        fail.
qobol_process_all(_).

qobol_process_class(E,Opts) :-
        \+ opts_included_class(E,Opts),
        !.
        %print_message(informational,excluded(E)).
qobol_process_class(E,Opts) :-
        parse_entity(E,X,Msg,Opts),
        !,
        print_message(informational,Msg),
        qobol_process_parse(E,X,Msg,Opts).
qobol_process_class(E,_Opts) :-
        print_message(informational,no_parse(E)).

qobol_process_parse(_,_,fail(_),_) :- !.
qobol_process_parse(E,X,pass(_),Opts) :-
        member(export(obo),Opts),
        !,
        qobol_write_obo(E,X,Opts).
qobol_process_parse(E,X,pass(Msg),Opts) :-
        !,
        print_message(informational,ok(E,Msg,X,Opts)).


qobol_write_obo(E,X,_Opts) :-
        owl2cdef(X,CDef),
        !,
        write_cdef(obo,E,CDef).
qobol_write_obo(E,X,_Opts) :-
        print_message(informational,no_owl(E,X)).

write_new_parsed_expressions :-
        write_new_parsed_expressions([]).
write_new_parsed_expressions(Opts) :-
        entity_parsed_expression(E,_Label,X,Msg,[undefined_only(true)|Opts]),
        print_message(informational,Msg),
        (   Msg=fail(_)
        ->  true
        ;   owl2cdef(X,CDef),   % fails if ?(_)s cannot be resolved
            writeln(cdef=CDef),
            write_cdef(obo,E,CDef)),
        fail.
write_new_parsed_expressions(_).

entity_new_parsed_expression(E,Label,X,Msg) :-
        entity_parsed_expression(E,Label,X,Msg,[undefined_only(true)]).

entity_parsed_expression(E,Label,X,Msg,Opts) :-
        ontol_db:class(E),
        opts_included_class(E,Opts),
        parse_entity(E,Label,X,Msg,Opts).

show_class_parse_mismatch(E,Opts) :-
        uniq_class(E),
        opts_included_class(E,Opts),
        compare_parse(E,_,_,fail(mismatch(_,Def1\=Def2)),Opts),
        writeln('<<<<<<<<'),
        write_cdef(obo,E,Def1),
        writeln('--------'),
        write_cdef(obo,E,Def2),
        writeln('>>>>>>>>'),
        nl.

suggest_term(E,Label,X_Repl,NewTerm,Opts) :-
        uniq_class(E),
        \+ genus(E,_),
        opts_included_class(E,Opts),
        suggest_term_d(E,Label,X_Repl,NewTerm,Opts),
        \+ entity_label_scope(_,NewTerm,_).


% ----------------------------------------
% MAIN LOGIC
% ----------------------------------------

uniq_class(C) :- setof(C,class(C),Cs),member(C,Cs).

%% parse_entity(+E,?Label,?X,?Msg,+Opts:list)
% Label is unified with whichever label was used to gain the expression
parse_entity(E,X,Msg,Opts) :-
        parse_entity(E,_,X,Msg,Opts).
parse_entity(E,Label,X_Repl,Msg,Opts) :-
        select(compare(true),Opts,Opts2),
        !,
        debug(qobol,'Comparing: ~w',[E]),
        compare_parse(E,Label,X_Repl,Msg,Opts2).
parse_entity(E,_,_,_,Opts) :-
        member(undefined_only(true),Opts),
        genus(E,_),
        debug(qobol,'has def: ~w',[E]),
        !,
        print_message(informational,xp_exists_for(E)),
        fail.
parse_entity(E,Label,X_Repl,Msg,Opts) :-
        debug(qobol,'Parsing: ~w',[E]),
        qobol(CatTags,Toks,X,MatchGoal,ValidGoal),
        category_match(CatTags,Opts),
        debug(qobol,'  Using: ~w',[qobol(CatTags,Toks,X,MatchGoal,ValidGoal)]),
        entity_label_scope(E,Label_1,Sc),
        %label_lexical_variant(Label_1,Label),
        downcase_atom(Label_1,Label),
        % hack for speed [CANT REMEMBER WHY THIS IS HERE]
        %\+ nb_current(Label,_),
        %nb_setval(Label,true),
        debug(qobol,'  Label: ~w',[Label]),
        opts_allowed_scope(Sc,Opts),
        % hack for antiporters
        %atomic_list_concat(Toks_tmp,':',Label),
        %atomic_list_concat(Toks_tmp,' : ',Label2),
        label_template_match(Label,Toks),
        debug(qobol,'  Match: ~w // Testing: ~w',[Toks,MatchGoal]),
        MatchGoal,
        debug(qobol,'  Valid: ~w // ~w',[Toks,MatchGoal]),
        (   ValidGoal
        ->  Msg=pass(ValidGoal)
        ;   Msg=fail(ValidGoal)),
        debug(qobol,'  Msg: ~w // repl(~w)',[Msg,X]),
        expr_repl_labels(X,X_Repl,[force(true)|Opts]), % no ?s
        debug(qobol,'  Repl: ~w --> ~w',[X,X_Repl]),
        !.

suggest_term_d(E,Label,X_Repl,NewTerm,Opts) :-
        debug(qobol,'Parsing: ~w',[E]),
        qobol(CatTags,Toks,X,MatchGoal,ValidGoal),
        category_match(CatTags,Opts),
        entity_label_scope(E,Label_1,Sc),
        (   nb_current(use_lexical_variants,true)
        ->  label_lexical_variant(Label_1,Label)
        ;   Label=Label_1),
        debug(qobol,'  Label: ~w',[Label]),
        opts_allowed_scope(Sc,Opts),
        label_template_match(Label,Toks,' '),
        MatchGoal,
        expr_repl_labels(X,X_Repl,Opts),
        (   expr_unresolved(X_Repl,NewTerm)
        ;   \+ ValidGoal,
            NewTerm=ValidGoal),
        \+ parse_entity(E,_,_,pass(_),[force(true)|Opts]),
        !.



%% compare_parse(+Class, ?Label, ?CDef, ?Msg, +Opts:list)
compare_parse(E,_,_,pass(no_existing),_) :-
        \+ genus(E,_),
        !.
compare_parse(E,Label,X_Repl,Msg,Opts) :-
        parse_entity(E,Label,X_Repl,pass(_),Opts),
        !,
        class_cdef(E,CDef),
        match_cdef(E,CDef,X_Repl,Msg).
compare_parse(E,_,_,fail(no_parse(E)),_).

match_cdef(E,CDef,X,Msg) :-
        owl2cdef(X,cdef(G,DL2)),
        !,
        CDef=cdef(G,DL), 
        CDef2=cdef(G,DL2),
        (   CDef=CDef2,
            match_diffs(DL,DL2)
        ->  Msg=pass(matches)
        ;   Msg=fail(mismatch(E,CDef\=CDef2))).
match_cdef(_,_,_,fail(owl2cdef)).

match_diffs([],[]).
match_diffs([D|DL_Rest],DL2) :-
        select(D2,DL2,DL2_Rest),
        match_diff(D,D2),
        !,
        match_diffs(DL_Rest,DL2_Rest).

match_diff(D,D).
match_diff(A,B) :-        entity_xref(A,B).
match_diff(A,B) :-        entity_xref(B,A).



% ----------------------------------------
% EXPRESSION PROCESSING
% ----------------------------------------



label_partition(L,S) :- in(L,_,E),entity_partition(E,S).

%% in(+Label,?Ont,?Entity)
%
% we need to hack Opts for now
in(L,Ont) :- in(L,Ont,_).
%in(L,Ont,E) :- label_lexical_variant(L,LV),entity_label_scope(E,LV,_),check_id_ont(E,Ont),\+entity_obsolete(E,_).
%in(L,Ont,E) :- nb_getval(qobol_opts,Opts),repl_label(L,E,Opts),E\='?'(_),check_id_ont(E,Ont),\+entity_obsolete(E,_),!. % USE GLOBALS FOR NOW
in(L,Ont,E) :- nb_getval(qobol_opts,Opts),repl_label(L,E,Opts),E\='?'(_),check_id_ont(E,Ont),\+entity_obsolete(E,_),!.

check_id_ont(ID,Ont) :- id_idspace_wrap(ID,Ont),!.
check_id_ont(ID,Onts) :- member(Ont,Onts),id_idspace_wrap(ID,Ont),!.

id_idspace_wrap(ID,Ont) :- id_idspace(ID,Ont),!.
%id_idspace_wrap(ID,'CHEBI') :- id_idspace(ID,'GOCHE'),!. % temp hack!


repl_label(L,X,Opts):-
        repl_label_1(L,X_1,Score,Opts),
        \+ entity_obsolete(X_1,_),
        !,
        (   atom(X_1),
            id_idspace(X_1,'UBERON'),
            repl_label_1(L,X,Alt_Score,Opts),
            atom(X),
            \+ id_idspace(X,'UBERON'),
            Alt_Score >= Score
        ->  true
        ;   X=X_1).

:- initialization(table_pred(repl_label_1/4)).
repl_label_1(X,X,5,_Opts) :- var(X),!.
repl_label_1(X,X,5,_Opts) :- ontol_db:class(X).
repl_label_1(X,X,5,_Opts) :- ontol_db:property(X).
repl_label_1(L,X,4,_Opts) :- entity_label(X,L).
repl_label_1(L,X,Score,Opts) :- downcase_atom(L,Ld),entity_label_scope_dn(X,Ld,Sc),opts_allowed_scope(Sc,Opts),scope_score(Sc,Score).
%repl_label_1(L,X,Score,Opts) :- nb_current(use_lexical_variants,true),downcase_atom(L,Ld),entity_label_scope_dn(X,Ld,Sc),opts_allowed_scope(Sc,Opts),scope_score(Sc,Score).
%repl_label_1(L,X,Score,Opts) :- nb_current(use_lexical_variants,false),entity_label_scope(X,L,Sc),opts_allowed_scope(Sc,Opts),scope_score(Sc,Score). % TODO - Opts
repl_label_1(L,X,Score,Opts) :- nb_current(use_lexical_variants,true),label_lexical_variant(L,LV),LV\=L,entity_label_scope_dn(X,LV,Sc),opts_allowed_scope(Sc,Opts),scope_score(Sc,ScoreFull),Score is ScoreFull-1.
repl_label_1(L,'?'(L),1,Opts) :- \+member(force(true),Opts).

expr_repl_labels(In,In,_Opts) :- var(In),!.
expr_repl_labels(In,Out,Opts) :- atom(In),!,repl_label(In,Out,Opts).
expr_repl_labels(X and Y,X2 and Y2,Opts) :- !,expr_repl_labels(X,X2,Opts),expr_repl_labels(Y,Y2,Opts).
expr_repl_labels(R some Y,R2 some Y2,Opts) :- !,expr_repl_labels(R,R2,Opts),expr_repl_labels(Y,Y2,Opts).
expr_repl_labels(X,X,Opts) :- print_message(error,no_parse(X,Opts)).

named_term(X) :- atom(X).
%named_term('?'(_)).

scope_score(label,3) :- !.
scope_score(exact,3) :- !.
scope_score(_,2) :- !.

% any unresolved terms will be parsed as '?'(Name)
expr_unresolved('?'(X),X).
expr_unresolved(X and _,Z) :- expr_unresolved(X,Z).
expr_unresolved(_ and Y,Z) :- expr_unresolved(Y,Z).
expr_unresolved(_ some Y,Z) :- expr_unresolved(Y,Z).



owl2cdef(X and Y,cdef(X,L)) :- named_term(X),!,owl2cdef(Y,cdef(X,L)).
owl2cdef(X and Y,cdef(X,L)) :- named_term(Y),!,owl2cdef(X,cdef(X,L)).
owl2cdef(X and Y,cdef(G,L)) :- owl2cdef(X,cdef(G,L1)),owl2cdef(Y,cdef(G,L2)),append(L1,L2,L).
owl2cdef(R some Y,cdef(_,[R=Y])) :- named_term(Y).

/*

  examples:

  blip-findall  -u qobol_mp -goal prep_mp_all "entity_parsed_expression(E,L,X,fail(in(X,_)),[undefined_only(true),ontology('MP')])" -select "x(E,L,X)" -no_pred  > z

  
*/

:- multifile prolog:message//1.
prolog:message(no_parse(X)) -->
        {class(X,N)},
        !,
        ['% No parse: ',X,' "',N,'"'].

