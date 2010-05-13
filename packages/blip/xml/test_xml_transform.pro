/* -*- Mode: Prolog -*- */

:- module(test_xml_transform,
          [
          ]).

user:xmlpred(personset,_,[],translate(person)).
user:xmlpred(person,_,person(ID,Name),
             [let(ID=ssid),
              let(Name=fullname),
              prolog(M=in_person(ID)),
              translate(address,M)]).
user:xmlpred(address,in_person(PID),person_address(PID,Street,City),
             [let(Street=street),
              let(City=city)]).

% convert XML to prolog facts and write them out
demo:-
        File='personset.xml',
        load_structure(File,Els,[dialect(xml),space(remove)]),
        apply_xmlpred(user,Els,Preds),
        forall(member(Pred,Preds),
               writeln(Pred)).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(personset)=
      (   ensure_loaded(library(sgml)),
          File='personset.xml',
          load_structure(File,[Node],[dialect(xmlns),space(remove)])
      )/Node).

unittest(test(tr,
            [Node=load(personset)],
            (   ensure_loaded(bio(xml_transform)),
                apply_xmlpred(users,Node,Facts),
                forall(member(Fact,Facts),
                       writeln(Fact))),
            true)).

unittest(test(rev,
            [Node=load(personset)],
            (   ensure_loaded(bio(xml_transform)),
                apply_xmlpred(users,Node,Facts),
                reverse_xmlpred(users,Node2,Facts),
                writeln(Node2)),
            true)).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/12/03 00:06:24 $
  @license LGPL

  
  */