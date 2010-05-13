/* -*- Mode: Prolog -*- */

:- module(pkb_xmlmap_phenote,[]).

:- use_module(bio(xml_transform)).
:- use_module(bio(pkb_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

io:xml_to_preds(phenotexml,XML,PL):-
        apply_xmlpred(pkb_xmlmap_phenote,XML,PL).

xmlpred(annotation_set,_,[],
        [translate(annotation)
        ]).
xmlpred(annotation,_,[disease(D),
                      disease_label(D,DN),
                      organism(O),
                      organism_role_disease(O,patient,D),
                      organism_role(O,patient),
                      %organism_type(O,O), % TODO - too many in one type not scalable
                      organism_type(O,'NCBITaxon:9606^bearer_of(DOID:4)'),
                      organism_species(O,'NCBITaxon:9606'),
                      %organism_label(O,ON),
                      organism_phenotype(O,P_trans)
                     ],
        [
         let(D=['Disease',att(id)]),
         let(DN=['Disease',att(name)]),
         %let(O=['Disease',att(id)]),
         prolog(diseaseid_organismid(D,O)),
         %let(ON=['Disease',att(name)]),
         let(P=['Phenotype',att(id)]), % HP:123 -> purl URL
         let(E=['Entity',att(id)]),
         let(Q=['Quality',att(id)]),
         let(Dep=['Addl_Entity',att(id)]),
         prolog(id_url(E,E_trans)),
         prolog(id_url(Q,Q_trans)),
         prolog(id_url(Dep,D_trans)),
         prolog(PE=(E_trans,Q_trans,D_trans,-)),
         % use the pre-composed class if available, otherwise the quad itself
         prolog((P=''
                ->  P_trans=PE
                ;   id_url(P,P_trans))),
         translate('Description',in(P_trans)),
         translate('Quality',in(P_trans,PE))
        ]).



xmlpred('Quality',in(_,(_,'',_,_)),[],[]).
xmlpred('Quality',in(P_trans,PE),[phenotype_quad(P_trans,PE)],[]).

xmlpred('Entity',in(P,Q,D,P_trans),[phenotype_quad(P_trans,PE)],
        [
         let(E=att(id)),
         prolog(id_url(E,E_trans)),
         prolog(id_url(Q,Q_trans)),
         prolog(id_url(D,D_trans)),
         prolog(PE=(E_trans,Q_trans,D_trans,-)),
         % use the pre-composed class if available, otherwise the quad itself
         prolog((P=''
                ->  P_trans=PE
                ;   id_url(P,P_trans)))
        ]).

xmlpred('Description',in(P),[phenotype_description(P,Desc)],
        let(Desc='.')).

% now using shortened URIs
id_url('',(-)) :- !.
%id_url(ID,URL) :-
%        concat_atom([A,B],':',ID),
%        !,
%        concat_atom(['http://purl.org/obo/owl/',A,'#',A,'_',B],URL).
id_url(X,X).

        
        
diseaseid_organismid(D,O) :-
        concat_atom(['NCBI_Taxon:9606','^','bearer_of(',D,')'],O).

