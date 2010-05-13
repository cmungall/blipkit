:- module(interaction_tblmap_regulators,
          [
           regulates_chain/2,
           regulated_by/3,
           regulated_byT/4
           ]).

:- use_module(interaction_db).
:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(seqfeature_db)).

%% regulated_by(?TargetGene,?RegulatorGene,?Prob:float)
:- extensional(regulated_by/3).

regulates_chain([X|L],MinP) :-
        regulates_chain(X,L,MinP).
regulates_chain(B,[A|L],MinP) :-
        regulated_by(B,A,P),
        P < MinP,
        regulates_chain(A,L,MinP).

% interaction_tblmap_regulators:regulated_byT('GAT1','HAL9',1e-3,Path).
regulated_byT(X,Y,MinP,[Y]):- regulated_by(X,Y,P),P<MinP.
regulated_byT(X,Y,MinP,[Z|Path]):- regulated_by(X,Z,P),P<MinP,regulated_byT(Z,Y,MinP,Path),\+member(Z,Path). % TODO




/** <module> 

  ---+ Synopsis

==
:- use_module(bio(interaction_tblmap_regulators)).

% 
demo:-
  nl.
  

==

---+ Details

http://jura.wi.mit.edu/cgi-bin/young_public/navframe.cgi?s=17&f=downloaddata.html


---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
