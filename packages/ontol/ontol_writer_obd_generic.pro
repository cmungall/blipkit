/* -*- Mode: Prolog -*- */



:- module(ontol_writer_obd_generic,[
                                ]).

:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(serval)).
:- use_module(bio(xml_writer)).
:- use_module(bio(ontol_writer)).
:- use_module(library(sgml)).

io:redirect_stdout(obd).
io:write_all(obd,_,_):-
        xmldoc_start(X),
        xmlnode_start(X,X2,graphset,[xmlns='http://www.bioontology.org/obd/schema/obd-generic']),
        xmlnode_start(X2,X3,graph),

        % nodes
        write_preds(X3,class(_,_)),
        write_preds(X3,property(_,_)),
        write_preds(X3,inst(_,_)),

        % links
        write_preds(X3,subclass(_,_)),
        write_preds(X3,restriction(_,_,_)),
        write_preds(X3,inst_rel(_,_,_)),
        xmlnode_end(X3,X2),
        xmlnode_end(X2,X).

obd=>
  call(use_module(bio(ontol_db))),
  xml:graphset(xmlns='http://www.bioontology.org/obd/schema/obd-generic'),
  xml:graph(class(X) forall class(X),
            property(X) forall property(X),
            inst(X) forall inst(X),
            subclass(X,Y) forall subclass(X,Y),
            restriction(X,Y,Z) forall restriction(X,Y,Z),
            inst_rel(X,Y,Z) forall inst_rel(X,Y,Z)).

%class(X)=>
            
            
write_label(X,N):-
        xmlnode_start(X,X2,label),
        xmlnode(X2,N),
        xmlnode_end(X2,X).
write_linkref(X,T,About):-
        cvt_id(About,About1),
        xmlnode_start(X,X2,T,[about=About1]),
        xmlnode_end(X2,X).
write_struct(X,N,T,Data):-
        (   T=''
        ->  Atts=[]
        ;   Atts=[type=T]),
        xmlnode_start(X,X2,N,Atts),
        write_label(X2,Data),
        xmlnode_end(X2,X).

nodeatt(Node,is_anonymous=true):- is_anonymous(Node).

write_node(X,MetaType,C,N):-
        cvt_id(C,C1),
        findall(A,nodeatt(C,A),NodeAtts),
        xmlnode_start(X,X2,node,[id=C1,metatype=MetaType|NodeAtts]),
        (   N=''
        ->  true
        ;   write_label(X2,N)),
        write_preds(X2,belongs(C,_)),
        %write_preds(X2,subclass(C,_)),
        write_preds(X2,def(C,_)),
        write_preds(X2,synonym(C,_,_)),
        write_preds(X2,inst_sv(C,_,_,_)),
        xmlnode_end(X2,X).

write_pred(X,class(C,N)):-
        write_node(X,class,C,N).
write_pred(X,property(C,N)):-
        write_node(X,relation,C,N).

write_pred(X,belongs(_,N)):-
        xmlnode_start(X,X2,source,[about=N]),
        xmlnode_end(X2,X).
write_pred(X,subclass(S,O)):-
        write_link(X,S,is_a,O,[]).
        %write_linkref(X,is_a,P).
write_pred(X,def(_,D)):-
        write_struct(X,description,'oboMetamodel:definition',D).

write_pred(X,synonym(_,T,Label)):-
        (   T=''
        ->  Atts=[]
        ;   Atts=[scope=T]),
        xmlnode_start(X,X2,alias,Atts),
        write_label(X2,Label),
        xmlnode_end(X2,X).

write_pred(X,inst(C,N)):-
        cvt_id(C,C1),
        xmlnode_start(X,X2,node,[id=C1,metatype=instance]),
        write_label(X2,N),
        write_preds(X2,inst_of(C,_)),
        write_preds(X2,inst_sv(C,_,_,_)),
        xmlnode_end(X2,X).
write_pred(X,inst_of(_,P)):-
        write_linkref(X,instance_of,P).
write_pred(X,inst_rel(S,P,O)):-
        write_link(X,S,P,O,[]).
write_pred(X,inst_sv(_,P,V,T)):-
        cvt_id(P,P1),
        xmlnode_start(X,X2,tagval,[tag=P1,datatype=T]),
        xmlnode_start(X2,X3,val,[]),
        xmlnode(X3,V),
        xmlnode_end(X3,X2),
        xmlnode_end(X2,X).
        

write_pred(X,restriction(S,P,O)):-
        (   reification(ID,restriction(S,P,O))
        ->  cvt_id(ID,ID1),Atts=[id=ID1|Atts2]
        ;   Atts=Atts2),
        (   reification(O,_)
        ->  Atts2=[is_object_reified=true]
        ;   Atts2=[]),
        write_link(X,S,P,O,Atts).
write_link(X,S,P,O,Atts):-
        xmlnode_start(X,X2,link,Atts),
        write_linkref(X2,subject,S),
        write_linkref(X2,predicate,P),
        write_linkref(X2,object,O),
        xmlnode_end(X2,X).

write_preds(X,Pred):-
        forall(Pred,write_pred(X,Pred)).

cvt_id(X,X):-  atom(X).
cvt_id(X,Y):-  X=..L,maplist(cvt_id,L,L1),concat_atom(L1,'__',Y).

io:write_all(phenotbl,F):-
        fh(F,H),
        forall(phenotype_annot(P,G,EC,AV,SC),
               write_termrow(H,phenotype_annot(P,G,EC,AV,SC))),
        close(H).

fh(F,user_output):-   var(F),!.
fh(F,H):- open(F,write,H).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(convert,
            [],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(io)),
                ensure_loaded(bio(goa_bridge_to_class)),
                ensure_loaded(bio(seqfeature_bridge_to_class)),
                %load_bioresource(sofa),
                %load_bioresource(go),
                load_biofile(go_assoc,'gene_assoc.fb.tbl'),
                write_biofile(obd,'fb.obd')),
            true)).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 03:35:06 $
  @license LGPL


  ==
  :- use_module(bio(ontol_writer_obd_generic)).
  ==

  you should not need this module directly - handled by module io
  
  */
