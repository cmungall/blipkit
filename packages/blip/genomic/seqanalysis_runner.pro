
:- use_module(bio(pp)).

%% blast(+P,+Q,+DB,?Out,+Opts) is det
blast(P,Q,DB,Out,Opts) :-
        async_exec('blast -p ~w -i ~w -d ~w',[P,Q,DB] > 'blast-~w-~w-~w.out').

%% blast_each(+P,+QDB,+DB,+Opts) is nondet
blast_each(P,QDB,DB,Opts) :-
        fastadb_split_id(QDB,SeqFile),
        blast(P,SeqFile,DB,_,Opts).

        
        
