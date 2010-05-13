% INTERNAL
link_implied(Node,Pred,Obj) <- graph_path(_,Obj,Node,Pred,_Dist). % 
link_n(Node,Pred,Obj) <- term2term(_,Pred,Obj,Node,0). % asserted
%link(Node,Pred,Obj,Implied)  <- link(_,_,Node,Pred,Obj,Implied,''). 
intersection_link(Node,Pred,Obj)  <- term2term(_,Pred,Obj,Node,1). % asserted
term0(IID,ID,N,NS) <- term(IID,N,NS,ID,0,_).
parent0(X,R,Y) <- term0(XI,X,_,_),link_n(XI,RI,YI),term0(RI,R,_,_),term0(YI,Y,_,_).
intersection0(X,R,Y) <- term0(XI,X,_,_),intersection_link(XI,RI,YI),term0(RI,R,_,_),term0(YI,Y,_,_).
product0(IID,ID,N) <- gene_product(IID,N,XIID,_S,_SS,_T,_FN),dbxref0(XIID,ID).
dbxref0(IID,Xref) <- dbxrefd(IID,_Acc,_,_DB,_,Xref).
