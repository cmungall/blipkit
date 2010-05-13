:- module(gff,
          [gffrow/2,
           load_gff/1,
           ffetch/2
          ]).

zztestq(ID1/ID2):-
        ffetch(ID2,[type='CDS',seq=S,range=R2]),
        writeln(id2=ID2),
        ffetch(ID1,[type='5_UTR',seq=S,range=R1]),
        writeln(id1=ID1),
        intersects(R1,R2).

ffetch(ID,CL):-
        gffrow(ID,D),
        %writeln(fmatch(ID,CL,D)),
        fmatch(CL,D).

fmatch([],_).
fmatch([C|CL],D):-
        fmatch1(C,D),
        fmatch(CL,D).
fmatch1(type=T,row(_,_,T,_,_,_,_,_,_)).
fmatch1(seq=S,row(S,_,_,_,_,_,_,_,_)).
fmatch1(range=R,row(_,_,_,Min,Max,_,Str,_,_)):-
        range_convert(gffrange(Min,Max,Str),R).

gffrow(ID,row(S,Src,T,Min,Max,F,Str,Sc,group(ID,PL,Gr))):-
        gffrow(S,Src,T,Min,Max,F,Str,Sc,group(ID,PL,Gr)).

range_convert(gffrange(Min1,Max,Str),range(X,Y)):-
        Min is Min1-1,
        (Str='-' ->
            X=Max,
            Y=Min
        ;
            X=Min,
            Y=Max
        ).

load_gff(PlFile) :-
	file_name_extension(Base, _Ext, PlFile),
	file_name_extension(Base, qlf, QlfFile),
	(   exists_file(QlfFile),
	    time_file(QlfFile, QlfTime),
	    time_file(PlFile, PlTime),
	    QlfTime >= PlTime
	->  load_files([QlfFile])
	;   access_file(QlfFile, write)
	->  qcompile(PlFile)
	;   load_files(PlFile)
	).

