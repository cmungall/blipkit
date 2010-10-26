/* -*- Mode: Prolog -*- */

:- use_module(bio(phylo_db)).

:- begin_tests(distance,[]).
test(rf) :-
        nlist_to_topology([a-b,c-d,e-f],T1),
        nlist_to_topology([a-c,d-f,b-e],T2),
        robinson_foulds_distance(T1,T2,6).
test(wrf) :-
        nlist_to_topology([a-b,c-d,e-f],T1),
        nlist_to_topology([a-c,d-f,b-e],T2),
        robinson_foulds_distance(T1,T2,6).
        
:- end_tests(distance).


% -------------------- TESTS --------------------
% TODO - convert these to plunit

unittest(load(test_nhx)=
      load_biofile(nhx,'test.nhx')/[]).
unittest(load(test2_nh)=
      load_biofile(nh,'test2.nh')/[]).
unittest(load(bcl2)=
      load_biofile(phyloxml,'bcl_2.phylo-xml')/[]).

/*
unittest(test(load_nhx_file,
             [_=load(test_nhx)],
             (   ensure_loaded(bio(phylo_db)),
                 phylotree_index(Node,Pairs),
                 writeln(Node-Pairs)),
            true)).
*/

unittest(test(monophyletic,
             [_=load(test2_nh)],
             (   ensure_loaded(bio(phylo_db)),
                 phylonode(Node1,hADH1),
                 phylonode(Node2,hADH2),
                 phylonode(OutGroup,yADH4)),
            phylotree_monophyletic([Node1,Node2],OutGroup))).

unittest(test(non_monophyletic,
             [_=load(test2_nh)],
             (   ensure_loaded(bio(phylo_db)),
                 setof(Node,(phylo_db:mixgroup(N),phylonode(Node,N)),Nodes),
                 phylonode(OutGroup,iADHX)),
            \+ phylotree_monophyletic(Nodes,OutGroup))).

unittest(test(orthology,
             [_=load(bcl2)],
             (   ensure_loaded(bio(phylo_db)),
                 ensure_loaded(bio(metadata_db)),
%                 forall(homologous_pair_relation(A,B,R),
%                        writeln([R,A,B]))),
                 forall((iso_orthologous_pair(A,B),A@<B,entity_label(A,AN),entity_label(B,BN)),
                        writeln([AN-BN]))),
            true)).

mixgroup(hADH1).
mixgroup(yADH2).
mixgroup(yADH3).

