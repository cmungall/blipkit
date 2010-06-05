/** @s1
  example serval application

  to run with test data:

  serval -p 8080 examples/serval/tree_viewer.pro\
                 examples/serval/test_tree_data.pro

  then open up

  http://127.0.0.1:8080/tv

  to run with real data:

  download ftp.ncbi.nih.gov/pub/taxonomy/

  add these lines to your .plrc

  user:bioresource(taxnames,'/users/me/my/data/names.dmp').
  user:bioresource(taxnodes,'/users/me/my/data/nodes.dmp').

  (the 2nd arg should point to where you have these two files
  downloaded; you can use standard SWI file expansion to specify a
  logical path here)

  now you are ready to start the server:

  serval -p 8080 examples/serval/load_tax.pro\
                 examples/serval/tree_viewer.pro

  then open up

  http://127.0.0.1:8080/tv

  the tree view works by recursively nesting HTML list elements; this
looks best with the following stylesheet:

  http://www.godatabase.org/amigo/css/formatting.css

  the list of open nodes is stored as a session param (open_ids). if
you close a node, all nodes beneath that node remain open. however,
open nodes are not visible unless there is a fully open path to root
  
  see @l|serval| for more details

  this same code can be used for DAGs - see the amigo2 prototype code
  
*/

:- use_module(bio(bioprolog_util)).

% --MODEL--

% all model logic in @l|taxon|
%  all we need is the predicate for getting root and leaf nodes
%  we can esily substitute different logic here; eg ontology DAGs
:- use_module(bio(taxon)).     

% --CONFIG--
init_page(treeview).            % only one page so far
app_name(tv).                   % URL app name; eg local:8080/tv
form_method('GET').             % GET/POST

% --VIEW--

/** @s1 VIEW

  the core view is tree_node(ID,OpenIDs) - this recursively builds a
nested HTML list from the root of the tree down through all open nodes
  
  */

sdefun(treeview/'Tree view of taxonomy',
       html(head(title('tree viewer'),
                 link(href='http://www.godatabase.org/amigo/css/formatting.css',
                      rel=stylesheet,
                      type='text/css')),
            body(div(class=tree,
                     getparam(open_ids,OpenIDs),
                     sform(dag,[],
                           setof(tree_node(ID,OpenIDs),
                                 memocall(taxroot(ID)))))))).
sdefun(tree_node(ID,OpenIDs)/'Recursive node with nested children',
       ul(li(if(member(ID,OpenIDs),
                then:[
                      ilink('-',close_id=ID),node_desc(ID),
                      findall(tree_node(IDc,OpenIDs),taxparent(IDc,ID))
                     ],
                else:[
                      if(taxleaf(ID),
                         then:data('#'),
                         else:[ilink('+',open_id=ID)]),
                      node_desc(ID)
                     ])))).
sdefun(node_desc(ID)/'HTMLized short one-line description of node',
       if(taxname(ID,N),then:N,else:ID)).

% --Controller--
/** @s1 controller

  currently two transitions are defined. both preserve the same page
(there is only one page in this application). the transitions change
session state, either adding or removing an open node

  @s3 parenthetical note:

  : whilst it is generally good to separate view from controller,
sometimes it may be beneficial to embed controller logic in the
view. this may be possible in future serval versions.

the idea is that session transform functions would be embedded inside
some kind of form submit button function in the sdefun. this is
similar to call-with-continuations in schema. however, because serval
function logic is more declarative our implementation can be much more
space efficient that schema call/cc webservers.
  
  */
transition(_,treeview,S,S2):-
        getparam_as_num(S,open_id,ID),
        !,
        append_session_data(S,open_ids,[ID],S2).
transition(_,treeview,S,S2):-
        getparam_as_num(S,close_id,IDclose),
        !,
        (getparam(S,open_ids,IDs)
        ->  findall(ID,(member(ID,IDs),ID\=IDclose),NewIDs)
        ;   NewIDs=[]),
        add_session_data(S,[[open_ids,NewIDs]],S2).
