/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.3 $
  @date @cvskw $Date: 2005/09/28 22:01:01 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| wn_db - datalog module for wordnet

  @s1 Synopsis

  @cl
  :- use_module(wn_db,[s/6,hyp/2]).

  test_word(building).
  test_word(substance).
  test_word(transcription).
  
  demo:-
        load_biofile(pro(wn_db),'wn_s.pl'),
        load_biofile(pro(wn_db),'wn_hyp.pl'),
        forall( (test_word(W),s(ID,_,W,_,_,_)),
               show_hyps(ID)).

  % recursively show hypernyms
  show_hyps(ID):-
        s(ID,_,W,_,_,_),
        format('word: ~w~n',[W]),
        format('Hypernyms:~n'),
        forall(hyp(ID,HypID),
               show_hyps(HypID)).
  
  @/cl

  @s1 Description

  This is a blip-datalog wrapper for the prolog wordnet
database. wordnet data predicates can be accessed in a module-safe
way, and loaded using standard blip predicates such as
@rp|load_biofile/2| and @rp|load_bioresource|

  SWI-Prolog qlf files will be automatically generated the first time
a wordnet prolog file is loaded
  
  @s2 bioresources

  You can set up your wordnet prolog database as a bioresource for
convenience. Add this to your bioconf file or .plrc:

  @cl
% --WordNet--
user:file_search_path(wn, '/users/me/Data/ontologies/wn').
user:bioresource(wn_s,wn('wn_s.pl'),pro,wn_db).
user:bioresource(wn_hyp,wn('wn_hyp.pl'),pro,wn_db).
  @/cl

  You can now access each db directly via @rp|load_bioresource/2|
  
  @s2 Getting wordnet
  
  See:
  @link(url='http://wordnet.princeton.edu/obtain')|Obtaining-Wordnet|

  Download the prolog representation

  @s2 Data predicates

  Each WordNet relation is represented in a separate file by operator
name. Some operators are reflexive (i.e. the "reverse" relation is
implicit). So, for example, if x is a hypernym of y , y is necessarily
a hyponym of x . In the prolog database, reflected pointers are
usually implied for semantic relations.

Semantic relations are represented by a pair of synset_id s, in which
the first synset_id is generally the source of the relation and the
second is the target. If two pairs synset_id , w_num are present, the
operator represents a lexical relation between word forms. In the case
of the cls operator, the class_type indicates whether the
classification relation represented is topical, usage, or regional, as
indicated by the class_type of t , u , or r , repsectively.

@list

@li s(synset_id,w_num,'word',ss_type,sense_number,tag_count). 

A s operator is present for every word sense in WordNet. In wn_s.pl , w_num specifies the word number for word in the synset.


@li g(synset_id,'(gloss)'). 

The g operator specifies the gloss for a synset.


@li hyp(synset_id,synset_id). 

The hyp operator specifies that the second synset is a hypernym of the first synset. This relation holds for nouns and verbs. The reflexive operator, hyponym, implies that the first synset is a hyponym of the second synset.


@li ent(synset_id,synset_id). 

The ent operator specifies that the second synset is an entailment of first synset. This relation only holds for verbs.


@li sim(synset_id,synset_id). 

The sim operator specifies that the second synset is similar in meaning to the first synset. This means that the second synset is a satellite the first synset, which is the cluster head. This relation only holds for adjective synsets contained in adjective clusters.


@li mm(synset_id,synset_id). 

The mm operator specifies that the second synset is a member meronym of the first synset. This relation only holds for nouns. The reflexive operator, member holonym, can be implied.


@li ms(synset_id,synset_id). 

The ms operator specifies that the second synset is a substance meronym of the first synset. This relation only holds for nouns. The reflexive operator, substance holonym, can be implied.


@li mp(synset_id,synset_id). 

The mp operator specifies that the second synset is a part meronym of the first synset. This relation only holds for nouns. The reflexive operator, part holonym, can be implied.


@li der(synset_id,synset_id). 

The der operator specifies that there exists a reflexive lexical morphosemantic relation between the first and second synset terms representing derivational morphology.


@li cls(synset_id,synset_id,class_type). 

The cls operator specifies that the first synset has been classified as a member of the class represented by the second synset.


@li cs(synset_id,synset_id). 

The cs operator specifies that the second synset is a cause of the first synset. This relation only holds for verbs.


@li vgp(synset_id,synset_id). 

The vgp operator specifies verb synsets that are similar in meaning and should be grouped together when displayed in response to a grouped synset search.


@li at(synset_id,synset_id). 

The at operator defines the attribute relation between noun and adjective synset pairs in which the adjective is a value of the noun. For each pair, both relations are listed (ie. each synset_id is both a source and target).


@li ant(synset_id,w_num,synset_id,w_num). 

The ant operator specifies antonymous word s. This is a lexical relation that holds for all syntactic categories. For each antonymous pair, both relations are listed (ie. each synset_id,w_num pair is both a source and target word.)


@li sa(synset_id,w_num,synset_id,w_num). 

The sa operator specifies that additional information about the first word can be obtained by seeing the second word. This operator is only defined for verbs and adjectives. There is no reflexive relation (ie. it cannot be inferred that the additional information about the second word can be obtained from the first word).


@li ppl(synset_id,w_num,synset_id,w_num). 

The ppl operator specifies that the adjective first word is a participle of the verb second word. The reflexive operator can be implied.


@li per(synset_id,w_num,synset_id,w_num). 

The per operator specifies two different relations based on the parts of speech involved. If the first word is in an adjective synset, that word pertains to either the noun or adjective second word. If the first word is in an adverb synset, that word is derived from the adjective second word.


@li fr(synset_id,f_num,w_num). 

The fr operator specifies a generic sentence frame for one or all words in a synset. The operator is defined only for verbs.

@/list

@s2 See also

http://www.ai.sri.com/~oaa/contributions/wordnet/wn_agent.pl



**/


:- module(wn_db, [
                    s/6,
                    g/2,
                    hyp/2,
                    ent/2,
                    sim/2,
                    mm/2,
                    ms/2,
                    mp/2,
                  der/2,
                  cls/3,
                    cs/2,
                    vgp/2,
                    at/2,
                    ant/4,
                    sa/4,
                    ppl/4,
                    per/4,
                    fr/3,

                                % views
                  hypT/2
                  ]).

:- use_module(bio(dbmeta)).
:- datapreds(wn_db,[
                    s/6,
                    g/2,
                    hyp/2,
                    ent/2,
                    sim/2,
                    mm/2,
                    ms/2,
                    mp/2,
                    cs/2,
                  der/2,
                  cls/3,
                    vgp/2,
                    at/2,
                    ant/4,
                    sa/4,
                    ppl/4,
                    per/4,
                    fr/3
                    ]).

hypT(ID,PID):- hyp(ID,PID).
hypT(ID,PID):- hyp(ID,XID),hypT(XID,PID).

cat(noun,n).
cat(verb,v).
cat(adj,a).
cat(adjective_satellite,s).
cat(adv,r).
