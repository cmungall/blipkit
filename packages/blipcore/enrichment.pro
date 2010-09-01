:- module(enrichment,
          [index_goal/2,
           index_goal/3,
           itemset_attribute_enrichment/2,
           itemset_attribute_enrichment/3
          ]).

% blip-findall -u enrichment -u curation_db -debug enrichment -i t/data/test-enrichment-genes.txt -f 'tbl(id)' -r go_assoc_local/GeneDB_SPombe  -r go_public -table_pred curation_db:curation_statementT/4 -table_pred ontol_db:parentT/2 "findall(Y,(id(X),atom_concat('GeneDB_Spombe:',X,Y)),IDs),index_goal(curation_statementT(_,I,_,A),I,A),itemset_attribute_enrichment(IDs,Stats),member(Stat,Stats)" -select Stat -label 

:- use_module(bio(stats_distributions)).

:- dynamic item/1.
:- dynamic item_attribute/2.
:- dynamic attribute_item/2.
:- dynamic sub_attribute_of/2.
:- multifile item/1.
:- multifile item_attribute/2.
:- multifile attribute_item/2.
:- multifile sub_attribute_of/2.

index_goal(G, SG) :-
        index_goal(G,SG,'').

index_goal(_, _, CacheFile) :-
        debug(enrichment,' checking cache ~w',[CacheFile]),
	file_name_extension(Base, _Ext, CacheFile),
	file_name_extension(Base, qlf, QlfFile),
        exists_file(QlfFile),
        !,
        consult(QlfFile).
index_goal(G-I-A, SG-A1-A2, CacheFile) :-
        retractall(item(_)),
        retractall(item_attribute(_,_)),
        retractall(attribute_item(_,_)),
        retractall(sub_attribute_of(_,_)),
        debug(enrichment,'indexing ~w',[G-I-A]),
        forall(G,
               (   item_attribute(I,A)
               ->  true
               ;   assert(item_attribute(I,A)),
                   assert(attribute_item(A,I)))),
        forall(SG,assert(sub_attribute_of(A1,A2))),
        debug(enrichment,'Indexed binary lookup in both directions',[]),
        setof(I,A^item_attribute(I,A),Is),
        forall(member(I,Is),
               assert(item(I))),
        save_cache(CacheFile).

save_cache('') :- !.
save_cache(F) :-
        open(F,write,IO,[]),
        forall(item(X),
               format(IO,'~q.~n',[item(X)])),
        forall(item_attribute(X,A),
               format(IO,'~q.~n',[item_attribute(X,A)])),
        forall(attribute_item(A,X),
               format(IO,'~q.~n',[attribute_item(A,X)])),
        forall(sub_attribute_of(A,X),
               format(IO,'~q.~n',[sub_attribute_of(A,X)])),
        close(IO),
        qcompile(F).

attribute_used_twice(Attribute) :-
        \+ \+ ((attribute_item(Attribute,F1), % expect this to be tabled..
                attribute_item(Attribute,F2), % expect this to be tabled..
                F1\=F2)).

itemset_attribute_enrichment(FSampleMembers,StatsSorted):-
        itemset_attribute_enrichment(FSampleMembers,StatsSorted,_).

%% itemset_attribute_enrichment(+FSampleMembers:list,?AttributeStats:list)
% FSampleMembers - each member must match a seqitem_db:item/1 
%
% TODO: call R here
itemset_attribute_enrichment(FSampleMembers,StatsSorted,DBSize):-
        length(FSampleMembers,FSampleSize),
        aggregate(count,F,item(F),NumItemsWithAttrs),
        (   var(DBSize)
        ->  DBSize=NumItemsWithAttrs
        ;   DBSize < NumItemsWithAttrs
        ->  throw(input('dbsize too small',DBSize < NumItemsWithAttrs))
        ;   true),
        debug(enrichment,'DBSize=~w',[DBSize]),
	% get all annotations for everything in the item set
        setof(Attribute,F^item_attribute(F,Attribute),
              Attributes),
        debug(enrichment,'Found all attributes used',[]),
	% bonferoni for attributees with >1 annotation
        setof(Attribute,(member(Attribute,Attributes),
                         attribute_used_twice(Attribute)),
              MAttributes),
	length(MAttributes,CorrectionFactor),
        debug(enrichment,'bonferoni correction factor (i.e. number of attributes with >1 annotation)=~w',[CorrectionFactor]),
	findall(P-Attribute-Stats,
		(   member(Attribute,Attributes),
		    annotations_attribute_probability(DBSize,CorrectionFactor,FSampleMembers,FSampleSize,Attribute,P,Stats)),
		AttributeStats),
        debug(enrichment,'removing redundant entries',[]),
        findall(P-Attribute-Stats,
                (   member(P-Attribute-Stats,AttributeStats),
                    \+ ((member(P2-Attribute2-_,AttributeStats),
                         Attribute2\=Attribute,
                         P2 < P,
                         sub_attribute_of(Attribute2,Attribute)))),
                AttributeStats2),
        sort(AttributeStats2,StatsSorted).


%% annotations_attribute_probability(FAttributePairs,DBSize,CorrectionFactor,FSampleSize,Attribute,CPValue)
annotations_attribute_probability(DBSize,CorrectionFactor,FSampleMembers,FSampleSize,Attribute,CPValue,Stats):-
        debug(enrichment,'attribute: ~w',[Attribute]),
        %solutions(F,member(F-Attribute,FAttributePairs),FSampleAnnotatedMembers),
        setof(F,(attribute_item(Attribute,F),member(F,FSampleMembers)),FSampleAnnotatedMembers),
        length(FSampleAnnotatedMembers,FSampleAnnotatedSize),
        debug(enrichment,'  annotated sample size: ~w',[FSampleAnnotatedSize]),
        aggregate(count,
                  F2,
                  attribute_item(Attribute,F2), % expect this to be tabled..
                  DBAnnotatedSize),
        debug(enrichment,'  annotated in db size: ~w',[DBAnnotatedSize]),
        p_value_by_hypergeometric(FSampleAnnotatedSize,FSampleSize,DBAnnotatedSize,DBSize,PValue),
	CPValue is CorrectionFactor * PValue,
        Stats=[uncorrected_p_value(PValue),hyper(FSampleAnnotatedSize,FSampleSize,DBAnnotatedSize,DBSize)].

