#!/usr/bin/swipl -L0 -G0 -T0 -A0 -q -g main -t halt -s

/*

  totally hacky code for making an examples html input file by extracting pieces of code from prolog programs

  */

:- use_module(bio(bioprolog_util)).
:- use_module(library(sgml)).
:- use_module(library(readutil)).

main:-
        getopt([atom([dir,d],Dir,'lib/Test/data'),
                atom([output,o],Out,'doc/examples.html.in')],
               FileL),
        tell(Out),
        writeln('<page>'),
        writeln('<title>Blip: Examples</title>'),
        writeln('<section id="Examples">'),
        writeln('<p>This pages contains various examples auto-extracted from the blip <a href="modules.html">module documentation</a>, in no particular order</p>. The examples are executed in verbose mode. Output is shown in green. You should be able to run any of the examples below yourself - may require certain bioresources, or data in the blip lib/Test/data/ directory'),
        forall(member(File,FileL),
               extract_codelisting_from(Dir,File)),
        writeln('</section></page>'),
        told.

extract_codelisting_from(Dir,File):-
        load_structure(File,XML,[dialect(xml)]),
        XML=[element(_,AL,SubElts)],
        member(module=Mod,AL),
        findall(_,
                (   xmlqr(SubElts,element(codelisting,_,[Code])),
                    write_codelisting(Dir,Mod,Code)),
                _),
        !,
        writeln(user_error,extracted_from(File)).
extract_codelisting_from(_Dir,File):-
        writeln(user_error,no_demo_codelisting_in(File)).
               

write_codelisting(Dir,Mod,Code):-
        sub_atom(Code,_,_,_,'demo:-'),
        gensym(example,ID),
        concat_atom([Dir,'/',ID,'_',Mod,'.pro'],F),
        writeln(user_error,writing_to(F)),
        open(F,write,IO,[]),
        write(IO,Code),         % write to example file
        close(IO),
        format('<subsection id="~w_~w"~n>',[ID,Mod]),
        format('<p>Extracted from module : <moduleref to=''~w''/></p>~n',[Mod]),
        write('<code>'),
        %writeln('<![CDATA['),
        sformat(HiliteCmd,'source-highlight -sprolog -fxhtml -i ~w',[F]),
        open(pipe(HiliteCmd),read,HiIO,[]),
        read_stream_to_codes(HiIO,SrcCodes),
        atom_codes(SrcAtom,SrcCodes),
        write(SrcAtom),            % write to current open stream
        %writeln(']]>'),
        write('</code>'),

        % this is such a hack - it would be much easier to
        % change the encoding in SWI... duh
        sformat(Cmd,'echo | swipl -g "[''~w''],working_directory(_,''~w''),demo,halt." | tools/force-ascii >& prog.out',[F,Dir]),
        writeln(user_error,running(Cmd)),
        shell(Cmd,Err),
        (   Err=0
        ->  read_file_to_codes('prog.out',CL,[]),
            atom_codes(ProgOut,CL),
            writeln('<p>Output:</p>'),
            write('<output><pre>'),
            writeln('<![CDATA['),
            write(ProgOut),
            writeln(']]>'),
            write('</pre></output>')
        ;   writeln(user_error,problem_with(F)),
            shell('cat prog.out',_)),
        writeln('</subsection>').


        
        
xmlq([element(Elt,AL,Elts)|_],element(Elt,AL,Elts)).
xmlq([_|XML],Q):-
        xmlq(XML,Q).

xmlqr([Elt|_],Elt).
xmlqr([element(_Elt,_AL,Elts)|_],Q):-
        xmlqr(Elts,Q).
xmlqr([_|Elts],Q):-
        xmlqr(Elts,Q).


writeln(IO,X):-
        write(IO,X),
        nl(IO).
        
