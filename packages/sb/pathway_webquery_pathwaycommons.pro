:- module(pathway_webquery_pathwaycommons,
          []).

/** <module> 

  ---+ Synopsis

==
:- use_module(bio(pathway_webquery_pathwaycommons)).

% 
demo:-
  nl.
  

==

---+ Details

See
http://www.pathwaycommons.org/pc/webservice.do?cmd=help

==
search:
http://www.pathwaycommons.org/pc/webservice.do?version=2.0&q=FGF&format=xml&cmd=search
(xml)

get_pathway
http://www.pathwaycommons.org/pc/webservice.do?cmd=get_pathways&version=2.0&q=O14763&input_id_type=UNIPROT
(   tab)


==

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
