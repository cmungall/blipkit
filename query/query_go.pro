:- use_module(bio(bioprolog_util)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(curation_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_sqlmap_go)).
:- use_module(bio(rdb_util)).

%:- pred go_sensu_differentiae('Parent'-'ParentName','Child'-'ChildSensu','Child2'-'Child2Sensu',
%                              'UniquePTo1','UniquePTo2','UniqueCTo1','UniqueCTo2').
:- mode go_sensu_differentiae(?,?,?,?,?,?,?) is nondet.
go_sensu_differentiae(Parent-ParentName,Child-ChildSensu,Child2-Child2Sensu,UniquePTo1,UniquePTo2,UniqueCTo1,UniqueCTo2):-
        class(Child,ChildName),
        name_base_sensu(ChildName,ParentName,ChildSensu), % child must have sensu suffix
        class(Parent,ParentName),
        parent(Child,Parent),   % only compare siblings, must also be direct children of base term
        parent(Child2,Parent),
        class(Child2,Child2Name),
        name_base_sensu(Child2Name,ParentName,Child2Sensu), % child2 has same base as child1
        Child2\=Child,
        unique_parents(Child,Child2,UniquePTo1),
        unique_parents(Child2,Child,UniquePTo2),
        unique_children(Child,Child2,UniqueCTo1),
        unique_children(Child2,Child,UniqueCTo2).

name_base_sensu(Name,Base,Sensu):-
        concat_atom([Base,Sensu1],' (sensu ',Name),
        % truncate final ')'
        atom_length(Sensu1,Sensu1Len),
        SensuLen is Sensu1Len-1,
        sub_atom(Sensu1,0,SensuLen,1,Sensu).

:- mode unique_child(+,+,?) is nondet.
unique_child(Class,Class2,Child,R):-
        parent(Child,R,Class),  % find a child of 1
        \+ parent(Child,Class2), % that is not a child of 2
        not_parallel_link_by_sensu(Class,Child),
        not_parallel_link_by_sensu(Class2,Child).

:- mode unique_parent(+,+,?) is nondet.
unique_parent(Class,Class2,Parent,R):-
        parent(Class,R,Parent),    % find a parent of 1
        \+ parent(Class2,Parent), % that is not a parent of 2
        not_parallel_link_by_sensu(Class,Parent),
        not_parallel_link_by_sensu(Class2,Parent).

not_parallel_link_by_sensu(From,To):-
        class(To,ToName),
        class(From,FromName),
                                % eliminate trivial uniqueness due to sensu;
                                % e.g. foo (sensu x) is_a bar (sensu x)
        \+ (   name_base_sensu(ToName,_,Sensu), % 
               name_base_sensu(FromName,_,Sensu)).

:- mode unique_children(+,+,?) is det.
unique_children(Class,Class2,UniqueChildren):-
        solutions(R/Child-ChildName,(unique_child(Class,Class2,Child,R),class(Child,ChildName)),UniqueChildren).

:- mode unique_parents(+,+,?) is det.
unique_parents(Class,Class2,UniqueParents):-
        solutions(R/Parent-ParentName,(unique_parent(Class,Class2,Parent,R),class(Parent,ParentName)),UniqueParents).
        
go_class_defined_by_part_link(Parent,ParentName,Child,ChildName,ChildDef):-
        class(Parent,ParentName),
        sub_atom(ParentName,_,_,0,part),
        restriction(Parent,_,_),
        subclass(Child,Parent),
        \+ (subclass(Child,Parent2),Parent2\=Parent),
        class(Child,ChildName),
        true leftjoin def(Child,ChildDef).

% useful for getting for example MetaCyc xref used in both MF and BP
shared_xref(X,C1-N1,C2-N2):-
        class_xref(C1,X),
        class_xref(C2,X),
        C1\=C2,
        belongs(C1,O1),
        belongs(C2,O2),
        O1\=O2,
        class(C1,N1),
        class(C2,N2).

regrel(regulates).
regrel(negatively_regulates).
regrel(positively_regulates).

unimplied_reg(RX-RXN,RY-RYN):-
        regrel(Rel),
        differentium(RX,Rel,X),
        subclass(RX,RY),
        differentium(RY,Rel,Y),
        genus(RX,G),
        genus(RY,G),
        \+ parentT(X,Y),
        class(RX,RXN),
        class(RY,RYN).

% pre-reasoned
unimplied_reg2(RX-RXN,RY-RYN):-
        ensure_loaded(bio(ontol_reasoner)),
        differentium(RX,regulates,_X),
        subclass(RX,RY),
        \+ entailed_by(subclass(RX,RY),_), % asserted
        \+ ((entailable(subclass(RX,RY),Rule),
             (   Rule=genus
             ;   Rule=genus_differentia))),
        class(RX,RXN),class(RY,RYN).

unimplied_reg3(RX-RXN,RY-RYN,P):-
        regrel(Rel),
        differentium(RX,Rel,X),
        subclass(RX,RY),
        differentium(RY,Rel,Y),
        genus(RX,GX),
        genus(RY,GY),
        \+((subclassT(X,Y),
            GX=GY)),
        X\=Y,
        (   parent_overT(part_of,X,Y)
        ->  P=1
        ;   P=0),
        class(RX,RXN),class(RY,RYN).


neg_pos_reg(X-XN,Y-YN):-
        class(NR,'negative regulation of biological process'),
        class(PR,'positive regulation of biological process'),
        genus(X,NR),
        subclassT(X,Y),
        genus(Y,PR),
        class(X,XN),
        class(Y,YN).

pos_neg_reg(X-XN,Y-YN):-
        class(NR,'negative regulation of biological process'),
        class(PR,'positive regulation of biological process'),
        genus(X,PR),
        subclassT(X,Y),
        genus(Y,NR),
        class(X,XN),
        class(Y,YN).

neu_dir_reg(X-XN,Y-YN):-
        class(NR,'negative regulation of biological process'),
        class(PR,'positive regulation of biological process'),
        class(R,'regulation of biological process'),
        genus(X,R),
        subclassT(X,Y),
        (   genus(Y,NR)
        ;   genus(Y,PR)),
        class(X,XN),
        class(Y,YN).

regulates_overloading(Z-ZN,X-XN,NumIsAParents):-
        regrel(Rel),
        differentium(X,Rel,_Y),
        subclass(Z,X),
        \+ genus(Z,_),
        setof(A,subclass(Z,A),As),
        length(As,NumIsAParents),
        class(X,XN),
        class(Z,ZN).

regulates_regulatesT(X-XN,R1,Y-YN,R2,Z-ZN):-
        regrel(R1),
        restriction(X,R1,Y),
        parentRT(Y,Y2),
        restriction(Y2,R2,Z),
        \+ ((parentRT(Y,Y3),restriction(Y3,R2,_),parentT(Y3,Y2))),
        regrel(R2),
        class(X,XN),
        class(Y,YN),
        class(Z,ZN).

% pre-reasoned
regulates_regulates(X,R1,Y,R2,Z):-
        regrel(R1),
        restriction(X,R1,Y),
        restriction(Y,R2,Z),
        regrel(R2).
regulates_regulates_wn(X-XN,R1,Y-YN,R2,Z-ZN):-
        regulates_regulates(X,R1,Y,R2,Z),
        class(X,XN),
        class(Y,YN),
        class(Z,ZN).
regulates_regulatesNR(X-XN,R1,Y-YN,R2,Z-ZN):-
        regulates_regulates(X,R1,Y,R2,Z),
        \+ ( (subclass(X1,X),
              regulates_regulates(X1,R1,Y1,R2,Z1),
              subclass(Y1,Y),
              subclass(Z1,Z),
              \+ ( (X1=X,Y1=Y,Z1=Z) ))),
        class(X,XN),
        class(Y,YN),
        class(Z,ZN).
regulates_regulatesNR2(X-XN,R1,Y-YN,R2,Z-ZN):-
        regulates_regulates(X,R1,Y,R2,Z),
        \+ ( (parent(X1,X),
              regulates_regulates(X1,R1,Y1,R2,Z1),
              parent(Y1,Y),
              parent(Z1,Z),
              \+ ( (X1=X,Y1=Y,Z1=Z) ))),
        class(X,XN),
        class(Y,YN),
        class(Z,ZN).

undeclared_regxp(R1,Y1):-
        differentium(X,R,Y),
        subclass(X,X2),
        differentium(X2,R2,Y2),
        subclassRT(R,R2),
        parentT(Y,Y2),
        parentT(Y,Y1),
        parentT(Y1,Y2),
        subclassRT(R,R1),
        subclassRT(R1,R2),
        \+ differentium(_X1,R1,Y1).

        
abduce_drug_link(Xc-XcN,Xp-XpN,Yc,Yp):-
        class(Yp,drug),
        abduce_link(Xc-XcN,Xp-XpN,Yc,Yp).


abduce_link(Xc-XcN,Xp-XpN,Yc-YcN,Yp-YpN):-
        subclassT(Xc,Xp),
        genus(Xc,G),
        genus(Xp,G),
        differentium(Xc,R,Yc),
        differentium(Xp,R,Yp),
        \+ parentRT(Yc,Yp),
        %belongs(Xc,Ont),        % must belong to same ontology
        %belongs(Yc,Ont),
        class(Xc,XcN),
        class(Xp,XpN),
        class(Yc,YcN),
        class(Yp,YpN).

cell_structure(Cell-CellN,Struct-StructN):-
        differentium(CD,'OBO_REL:results_in_acquisition_of_features_of',Cell),
        parent_overT(part_of,CD,P),
        differentium(P,'OBO_REL:results_in_formation_of',Struct),
        class(Cell,CellN),
        class(Struct,StructN).

development_formation_syn(ID,N,Syn,Def):-
        class(ID,N),
        atom_concat(Stem,' development',N),
        synonym(ID,exact,Syn),
        atom_concat(Stem,' formation',Syn),
        def(ID,Def).

% blip -i biological_process_xp_mouse_anatomy.obo -r go -r mouse_anatomy -u query_go findall developmental_linkcheck/4
developmental_linkcheck(X-XN,Y-YN,R,Link):-
        differentium(X,R,A1),
        restriction(A1,part_of,A2),
        differentium(Y,R,A2),
        class(X,XN),
        class(Y,YN),
        (   parentT(X,Link,Y)
        ->  true
        ;   Link=no).

developmental_linkcheck_abduce(X-XN,Y-YN,R):-
        differentium(X,R,A1),
        parent_overT(part_of,X,Y),
        differentium(Y,R,A2),
        \+ parent_overT(part_of,A1,A2),
        class(X,XN),
        class(Y,YN).

xp_irreg(X-XN,Y-YN):-
        differentium(X,_R,RX),subclassT(X,Y),differentium(Y,_R2,RY),RX \= RY,\+ subclassT(RX,RY),class(X,XN),class(Y,YN).
%xp_irreg2(X-XN,Y-YN):-
%        differentium(X,_R,RX),genus(X,XG),subclassT(X,Y),differentium(Y,_R2,RY),genus(Y,YG),YG\=XG,class(X,XN),class(Y,YN).
xp_irreg3(X-XN,Y-YN):-
        differentium(X,_R,RX),genus(X,XG),subclassT(RX,RY),differentium(Y,_R2,RY),genus(Y,YG),subclassRT(XG,YG),\+ subclassT(X,Y),class(X,XN),class(Y,YN).
xp_irreg4(AFoo-AFooN,XDev-XDevN,Dev,RX):-
        %RX='OBO_REL:results_in_complete_development_of',
        differentium(XDev,RX,X),
        genus(XDev,Dev),
        subclassT(AFoo,Dev),
        differentium(AFoo,RA,A),
        RX \= RA,
        subclassRT(A,X),
        \+ subclassRT(AFoo,XDev),
        class(XDev,XDevN),
        class(AFoo,AFooN).

wibble(G):-
        genus('GO:0001886',G).


receptor(C,RN,W):-
        entity_label(C,N),
        (   nonvar(RN)
        ->  sub_atom(N,0,_,_,RN)
        ;   true),
        concat_atom(Toks,' ',N),
        reverse(Toks,[W,receptor|L]),
        reverse([receptor|L],RNToks),
        concat_atom(RNToks,' ',RN).

receptor_binding_complex(RN,RC,BC,CC,AC):-
        receptor(_,RN,_W),
        (   receptor(RC,RN,activity) -> true ; RC='NULL'),
        (   receptor(BC,RN,binding) -> true ; BC='NULL'),
        (   receptor(CC,RN,complex) -> true ; CC='NULL'),
        (   atom_concat(BN,' receptor',RN),
            atom_concat(BN,' activity',AN),
            entity_label(AC,AN)
        ->  true
        ;   AC='NULL').


cc_func(C,F):-         differentium(C,'OBO_REL:has_function',F).
cc_func(C,F):-         differentium(C,'OBO_REL:realizes',F).

cc_func_check(C,F,P1,P2,NumC,NumF,NumBoth):-
        rdb_connect(Dbh,go),
        cc_func(C,F),
        rdb_query(Dbh,P1-NumBoth-NumF,class_conditional_prob(C,F,NumBoth,NumF,P1)),
        rdb_query(Dbh,P2-NumC,class_conditional_prob(F,C,_,NumC,P2)).

correl(T1,T2,NumBoth,NumT2,P):-
        rdb_connect(Dbh,go),
        class(T1),
        class(T2),
        not(parentRT(T1,T2)),
        not(parentRT(T2,T1)),
        rdb_query(Dbh,
                  NumBoth-NumT2-P,
                  (   class_conditional_prob(T1,T2,NumBoth,NumT2,P), P > 0.5, NumBoth > 8)).

%                  (   
%                      class_conditional_prob(T1,T2,NumBoth,NumT2,P),
%                      NumBoth > 4,
%                      P > 0.8)).

%% xp_correlation(C,R,D,NumBoth,NumT2,P)
% e.g. C = mt transport, D = mt, P = p(mt|mt-transport)
xp_correlation(C,R,D,NumBoth,NumT2,P):-
        rdb_connect(Dbh,go),
        differentium(C,R,D),
        debug(xp,'checking ~w ~w ~w',[C,R,D]),
        rdb_query(Dbh,
                  NumBoth-NumT2-P,
                  class_conditional_prob(D,C,NumBoth,NumT2,P)).

xp_correlation_avg(R,AvgP) :-
        xp_correlation_avg(R,AvgP,_,_).
xp_correlation_avg(R,AvgP,Sum,Count) :-
        aggregate(sum(P),X^Y^N1^N2^(xp_correlation(X,R,Y,N1,N2,P),number(P)),Sum),
        aggregate(count,X^Y^N1^N2^P^(xp_correlation(X,R,Y,N1,N2,P),number(P)),Count),
        AvgP is Sum/Count.


xp_inv_correlation(C,R,D,NumBoth,NumT2,P):-
        rdb_connect(Dbh,go),
        differentium(C,R,D),
        rdb_query(Dbh,
                  NumBoth-NumT2-P,
                  class_conditional_prob(C,D,NumBoth,NumT2,P)).


secretion_transport(S,T,R,SD):-
        class(S,SN),
        atom_concat(X,' secretion',SN),
        def(S,SD),
        class(T,TN),
        atom_concat(X,' transport',TN),
        (   subclassT(S,T)
        ->  R=subclass
        ;   subclass(S,Z),
            subclass(T,Z)
        ->  R=sibling
        ;   R=xxx).


suffix_match(FSuf,PSuf,F,P):-
        atom_concat(' ',FSuf,FSuf1),
        atom_concat(' ',PSuf,PSuf1),
        suffix_match1(FSuf1,PSuf1,F,P).
suffix_match(FSuf,PSuf,F,P):-
        class(F,FSuf),
        class(P,PSuf).
suffix_match1(FSuf,PSuf,F,P):-
        entity_label_exactmatch(F,FN),
        atom_concat(Ch,FSuf,FN),
        atom_concat(Ch,PSuf,PN),
        entity_label_exactmatch(P,PN).

entity_label_exactmatch(F,FN):- entity_label_scope(F,FN,exact).
entity_label_exactmatch(F,FN):- entity_label_scope(F,FN,label).


kinase_phosphorylation(F,P):-
        class(KA,'kinase activity'),
        subclassRT(F,KA),
        suffix_match('kinase activity','phosphorylation',F,P).

phosphatase_dephosphorylation(F,P):-
        class(KA,'phosphatase activity'),
        subclassRT(F,KA),
        suffix_match('phosphatase activity','dephosphorylation',F,P).

transporter_transport(F,P):-
        class(KA,'transporter activity'),
        subclassRT(F,KA),
        suffix_match('transporter activity','transport',F,P).

% word
metabolism_participant(P,C):-
	atom_concat(C,' metabolic process',PN),
	class(P,PN).

function_metabolism(F,P):-
	function_participant_token(F,C,_),
	metabolism_participant(P,C).

function_participant_token(F,C,Dir):-
        belongs(F,molecular_function),
        subclassT(F,'GO:0003824'),
	def_reaction(F,LA,RA),
	concat_atom(LToks,' + ',LA),
	concat_atom(RToks,' + ',RA),
	(   member(C,LToks),
	    Dir=reactant
	;   member(C,RToks),
	    Dir=product).

function_participants(F,LP,RP,Def):-
        belongs(F,molecular_function),
        subclassT(F,'GO:0003824'),
	def_reaction(F,LA,RA),
	atom_chebis(LA,LP),
	atom_chebis(RA,RP),
	def(F,Def).

atom_chebis(A,L):-
	concat_atom(Toks,' + ',A),
	maplist(atom_chebi,Toks,L).

atom_chebi(A,exactly(Num,C)):-
	concat_atom([NA|Toks],' ',A),
	catch(atom_number(NA,Num),_,fail),
	concat_atom(Toks,' ',A2),
	atom_chebi(A2,C),
	!.
atom_chebi(A,in(C)):-
	atom_concat(A2,'(in)',A),
	atom_chebi(A,C),
	!.
atom_chebi(A,out(C)):-
	atom_concat(A2,'(out)',A),
	atom_chebi(A,C),
	!.
atom_chebi(A,C):-
	entity_label(C,A),
	!.
atom_chebi(A,C):-
	entity_synonym(C,A),
	!.
atom_chebi(A,C):-
	atom_concat('an ',A1,A),
	!,
	atom_chebi(A1,C).
atom_chebi(A,C):-
	atom_concat('a ',A1,A),
	!,
	atom_chebi(A1,C).
atom_chebi(A,or(Cs)):-
	concat_atom(Toks,' or ',A),
	Toks=[_,_|_],
	!,
	maplist(atom_chebi,Toks,Cs).
atom_chebi(A,A) :-
	debug(chebi,'Not_in_chebi: ~w',[A]).



def_reaction(F,A,B):-
        def(F,Def),
        atom_concat('Catalysis of the reaction: ',Reac1,Def),
        %atom_concat(Reac1,'.',Reac),
        concat_atom([Reac|_],'.',Reac1),
        concat_atom([A,B],' = ',Reac),
        debug(reaction,'~w = ~w',[A,B]).

reverse_reaction(F1,F2,Def1):-
        belongs(F1,molecular_function),
        subclassT(F1,'GO:0003824'),
        def_reaction(F1,A,B),
        subclassT(F2,'GO:0003824'),
        def_reaction(F2,B,A),
        def(F1,Def1).

% e.g. GO:0006337-nucleosome disassembly       OBO_REL:results_in_breakdown_of-results_in_breakdown_of GO:0000786-nucleosome   GO:0034728-nucleosome organization      OBO_REL:results_in_organization_of-results_in_organization_of
xp_role_switch_pos(A,RA,X,B,RB):-
        differentium(A,RA,X),
        differentium(B,RB,X),
        RB\=RA,
        subclassT(A,B).

% e.g GO:0000046-autophagic vacuole fusion    OBO_REL:results_in_fusion_of-results_in_fusion_of       GO:0005776-autophagic vacuole
%     GO:0016044-membrane organization        OBO_REL:results_in_organization_of-results_in_organization_of   GO:0016020-membrane
% this means we must be careful with generic has_input
xp_role_switch(A,RA,XA,B,RB,XB):-
        differentium(A,RA,XA),
        subclassT(A,B),
        differentium(B,RB,XB),
        RB\=RA,
        \+ subclassRT(XA,XB).


xp_promote(G,P,PA,All) :-
        xp_promote('OBO_REL:unfolds_in',G,P,PA,All).

% blip -debug promote -r go -i biological_process_xp_cellular_component.obo -r go_assoc_local/sgd -u query_go findall xp_promote/3 -label
% blip -debug promote -r go -i biological_process_xp_cellular_component.obo -r rdb/go -u ontol_sqlmap_go -sqlbind curation_db:curation_statementT/5-go -u query_go findall xp_promote/3 -label
% lip -r implied/biological_process_xp_cellular_component-imports.obo -u curation_db -r go_assoc_local/sgd -u query_go findall xp_promote/4 -label
xp_promote(R,G,P,PA,All) :-
        genus(P,PG),           % e.g. mitochondrial_translation, translation
        differentium(P,R,CC),  % e.g. unfolds_in mitochondrion
        debug(promote,'finding candidate genes for genus: ~w',[PG]),
        solutions(G,curation_statementT(_,G,_,PG,_,part_of),Gs), % todo: curation_source/2
        %debug(promote,'  got candidate genes ~w',[Gs]),
        debug(promote,'  got candidate genes ',[]),
        member(G,Gs),
        xp_promote2(G,P,PA,PG,CC), % succeeds once, PA is semi-arbitrary
        solutions(A,curation_statementC(_,G,_,A),All).
        
% xp_promote2(+G,+P,?PA,+PG,+CC) is det
xp_promote2(G,P,PA,PG,CC) :-
        belongs(CC,CCO),
        curation_statementT(_,G,_,PG,PA,part_of),
        !,
        curation_statementT(_,G,_,CC,_,part_of),
        debug(promote,'  candidate ~w',[P]),
        % eliminate any with locations outside the loc of the process
        %\+ ((curation_statement(_,G,_,CC2),
        %     belongs(CC2,CCO),
        %     \+ subclassRT(CC2,CC),
        %     \+ parent_overT(part_of,CC2,CC))),
        \+ ((curation_statement(_,G,_,CC2),
             belongs(CC2,CCO),
             \+ subclass(CC2,CC),
             \+ restriction(CC2,part_of,CC))),
        debug(promote,'  has CC ~w (~w)',[G,CC]),
        % do not promote if we already have annotation
        \+ curation_statementT(_,G,_,P,_,part_of),
        !.

% assumes reasoner
curation_statementT(A,G,R,C,CA,_):-
        curation_statementC(A,G,R,CA),
        subclass(CA,C).
curation_statementT(A,G,R,C,CA,R2):-
        curation_statementC(A,G,R,CA),
        restriction(CA,R2,C).

xxcuration_statementT(A,G,R,C,CA,_):-
        curation_statementC(A,G,R,CA),
        subclassRT(CA,C).
xxcuration_statementT(A,G,R,C,CA,R2):-
        curation_statementC(A,G,R,CA),
        parent_overT(R2,CA,C).

curation_statementC(A,G,R,CA) :-
        curation_statement(A,G,R,CA),
        \+ curation_evidence_code(A,'IEA').


% blip -r go -i chebi_with_formula.obo -i ../xps/molecular_function_xp_chebi.obo -u query_go findall reaction_unbalanced/3 -label
reaction_unbalanced(F,A,IAC,OAC) :-
        genus(F,'GO:0003824'),
        chematom_count(F,'OBO_REL:has_input',IC),
        chematom_count(F,'OBO_REL:has_output',OC),
        IC\=OC,
        member(A-IAC,IC),
        member(A-OAC,OC),
        IAC\=OAC.



chematom_count(F,Rel,AtomTotalsFinal) :-
        findall(Chem-Num,(   differentium(F,Rel,Chem),Num=1 % TODO
                          ;  differentium(F,card(Rel,Num,Num),Chem)),
                ChemNums),
        findall(Atom-Total,
                (   member(Chem-Num,ChemNums),
                    subclass(Chem,Formula),
                    differentium(Formula,card(has_part,AC,AC),Atom),
                    Total is AC * Num),
                AtomTotals),
        solutions(Atom,member(Atom-_,AtomTotals),Atoms),
        solutions(Atom-Total,
                  (   member(Atom,Atoms),
                      findall(T1,member(Atom-T1,AtomTotals),T1s),
                      sumlist(T1s,Total)),
                  AtomTotalsFinal).


has_location(G,X) :- curation_statement(_,G,_,X),belongs(X,cellular_component).

has_unique_nr_location(G,X) :-
        has_location(G,X),
        debug(loc,'testing ~w ~w',[G,X]),
        \+ ((has_location(G,Y),
             X\=Y,
             \+ parentT(X,Y),
             debug(loc,'  counter ~w ~w',[G,Y]))).

has_function(G,X) :- curation_statement(_,G,_,X),belongs(X,molecular_function).

has_unique_nr_function(G,X) :-
        has_function(G,X),
        debug(loc,'testing ~w ~w',[G,X]),
        \+ ((has_function(G,Y),
             X\=Y,
             \+ parentT(X,Y),
             debug(loc,'  counter ~w ~w',[G,Y]))).

	
part_of_from_xref(SA,SB,A,B) :-
	entity_xref(A,X), % e.g. GO->NIF-Subc
	id_idspace(A,SA), % e.g GO
	subclassRT(X,Y),  % e.g. is_a regional part of Y
	differentium(Y,part_of,Z), % e.g. Neuron
	entity_xref(Z,B), % eg CL:neuron [note reverse xref dir]
	id_idspace(B,SB). % e.g. CL 

% suggest R=part_of
%  blip-godb -r ebi_go -sqlbind curation_db:all-ebi_go -i biological_process_xp_self-unvetted.obo -r ebi_go -u query_go -debug sql findall "infer_xp_annotation(part_of,G,C)" -label
infer_xp_annotation(R,G,C) :-
	differentium(C,R,Whole),
	class_cdef(C,cdef(Part,[_])),
	class_coannotated_with(Part,Whole,G),
	\+ ((curation_statementT(_,G,has_role,C))). % nr

infer_xp_annotation(Superclass,R,G,C) :-
	subclassRT(Part,Superclass), % e.g. signal transduction
	genus(C,Part),
	differentium(C,R,Whole),
	class_cdef(C,cdef(Part,[_])),
	class_coannotated_with(Part,Whole,G),
	\+ ((curation_statementT(_,G,has_role,C))). % nr

has_terminal_part(W,P) :-
	parent(W,has_part,P),
	\+((parent(_,R,P),
	    R\=has_part)).

% some leaf nodes will be terminal processes or complexes - if
% we assume that all gene products are the same we can infer
% back up has_part relationships.
has_terminal_part_gene(W,P,G) :-
	has_terminal_part(W,P),
	curation_statement(_,G,_,P),
	\+ curation_statement(_,G,_,W).

has_terminal_part_gene_u(W,P,G,S) :-
	has_terminal_part(W,P),
	curation_statement(A,G,_,P),
	curation_source(A,S),
	belongs(P,O),
	\+ ((curation_statement(_,G,_,X),
	     X\=P,
	     belongs(X,O))).

has_terminal_part_gene_recap(W,P,G,S) :-
	has_terminal_part(W,P),
	curation_statement(A,G,_,P),
	curation_source(A,S),
	curation_statement(A2,G,_,W),
	curation_source(A2,S).

% ----------------------------------------
% blip-findall -table_pred user:class_simplexp_promote/3 -table_pred ontol_db:subclassRT/2 -r goxp/biological_process_xp_cell -r cell -r go -u curation_db  -r go_assoc_local/mgi -u query_go "promote_annotation_by_xp(G,C,C2)" -label
% ----------------------------------------

class_simplexp_promote(C,Y,C_specific) :-
	subclassRT(C,Genus),
	genus(C_specific,Genus),
	differentium(C_specific,_,Y2),
	subclassRT(Y,Y2).
	
promote_annotation_by_xp(G,C,C_specific) :-
	curation_subject_property_value(Ann,C,_,Y),
	class_simplexp_promote(C,Y,C_specific),
	subclassT(C_specific,C),
	curation_statement(Ann,G,_,_).


