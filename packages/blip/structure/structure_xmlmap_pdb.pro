/* -*- Mode: Prolog -*- */



:- module(structure_xmlmap_pdb,[]).

:- use_module(bio(xml_transform)).

io:xml_to_preds(pdb,XML,PL):-
        apply_xmlpred(structure_xmlmap_pdb,XML,PL).

xmlpred(datablock,_,[],
        [translate(ndb_struct_na_base_pairCategory),
         translate(entity_poly_seqCategory)]).
xmlpred(ndb_struct_na_base_pairCategory,_,[],translate(ndb_struct_na_base_pair)).
xmlpred(entity_poly_seqCategory,_,[],translate(entity_poly_seq)).
xmlpred(ndb_struct_na_base_pair,_,pair(SI,SJ),
        [let(SI=i_auth_seq_id),
         let(SJ=j_auth_seq_id)]).
xmlpred(entity_poly_seq,_,pos_base(P,Base),
        [let(P=att(num)),
         let(Base=att(mon_id))]).
        

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test)=
      load_biofile(pdb,'2j01-pdb.xml')/[]).

unittest(test(load_file,
            [_=load(test)],
            (   ensure_loaded(bio(structure_db)),
                setof(ID,N^reaction(ID,N),IDs),
                length(IDs,NumIDs)),
             (NumIDs=47))).

unittest(test(mathml,
            [_=load(bm18)],
            (   ensure_loaded(bio(structure_db)),
                ID='MTX5deg',
                format('Finding kinetic law for ~w~n',[ID]),
                kinetic_law(ID,MathTerm)),
            MathTerm=times([_,_,_]))).

        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2006/01/14 22:23:02 $
  @license LGPL


  ==
  :- use_module(bio(structure_xmlmap_pdb)).
  ==

  bridging layer from pdb-xml to native structure model

  you should not need this module directly - handled by module io
  
  */