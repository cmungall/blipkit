:- use_module(bio(bioprolog_util)).

                                % alink.link_id, alink.node_id, alink.predicate_id, alink.object_id AS asserted_object_id, alink.source_id, ilink.predicate_id AS chaining_predicate_id, ilink.is_inferred, ilink.object_id

%ial(Link,Node,APred,Asserted,Source,Pred,IsInferred,Target)

n(N):- solutions(N,ial(_,N,_,_,_,_,_,_),Ns),member(N,Ns).

score(N1,N2,NumBoth):-
        n(N1),
        n(N2),
        findall(T,(ial(_,N1,_,_,_,_,_,T),
                   ial(_,N2,_,_,_,_,_,T)),
                Ts),
        length(Ts,NumBoth).

uscore(N1,N2,NumU):-
        n(N1),
        n(N2),
        findall(T,(   ial(_,N1,_,_,_,_,_,T)
                  ;   ial(_,N2,_,_,_,_,_,T)),
                Ts),
        length(Ts,NumU).


