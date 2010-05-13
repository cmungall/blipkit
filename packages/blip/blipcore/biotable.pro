/* -*- Mode: Prolog -*- */


:- module(biotable,
          [table_spec/2,
           connect_biotable/2,
           connect_biotable/3
          ]).
:- use_module(library(table)).

%% connect_biotable(+Resource,?Handle)
%   connects to a configured bioresource; this resource must already be defined using user:bioresource/3, and it must be a table format. See also connect_biotable/3
connect_biotable(R,H):-
        user:bioresource(R,P,Fmt),
        !,
        expand_file_search_path(P,Px),
        connect_biotable(Px,Fmt,H).
%% connect_biotable(+File,+Format,?Handle)
%
%   open a tabular file with a predefined header
%format. @param Handle
%  can then be used to query the table using in_table/3
%  
%  
connect_biotable(Fgz,gzip(Fmt),H):-
        !,
        tmp_file(Fgz,F),
        concat_atom([zcat,Fgz,'>',F],' ',UnzipCmd),
        shell(UnzipCmd),
        connect_biotable(F,Fmt,H).
connect_biotable(F,Fmt,H):-
        (table_spec(Fmt,Spec)
        ->  true
        ;   throw(no_such_fmt(Fmt))),
        new_table(F,
                  Spec,
                  [field_separator(9)],
                  H).

%% table_spec(?Fmt,?Spec)
%  @param Fmt
% 
%  atom identifier for the format
%  @param Spec
% 
%  list of columns, specified as terms with first arg as type
%
%  
%
%  you can specify the headers for your own formats like this:
%
%  ==
%  % define own file-format (3 columns) called 'myfmt'
%  biotable:table_spec(myfmt,[id(atom),name(atom),score(float)]).
%  ==
%
%  you can find out the full list of all formats defined by leaving Fmt
%unground, by typing =|biotable:table_spec(Fmt,_)|= interactively
% Gene Ontology association files
table_spec(go_assoc,
           [
            proddb(atom),
            prodacc(atom),
            prodsymbol(atom),
            qualifier(atom),
            termacc(atom),
            ref(atom),
            evcode(atom),
            with(atom),
            aspect(atom),
            prodname(atom),
            prodsyn(atom),
            prodtype(atom),
            prodtaxa(atom),
            assocdate(atom)
           ]).

% Inparanoid files
table_spec(inparanoid_tbl,
           [
            id(integer),
            id2(integer),
            spcode(atom),
            score(float),
            seq_id(atom)
            ]).

table_spec(fasta_tbl,
           [ id(atom),
             desc(atom),
             residues(atom)
           ]).

table_spec(blat,
           [match(atom),
            mismatch(integer),
            repmatch(integer),
            ns(integer),
            qgap_count(integer),
            qgap_bases(integer),
            tgap_count(integer),
            tgap_bases(integer),
            strand(integer),
            
            qname(integer),
            qsize(integer),
            qstart(integer),
            qend(integer),
            
            tname(integer),
            tsize(integer),
            tstart(integer),
            tend(integer),
            
            blockcnt(integer),
            blocksizes(integer),
            q_starts(integer),
            t_starts(integer)
           ]).

table_spec(blast_tbl,
           [
            query_name(atom),
            hit_name(atom),
            percent_identity(float),
            length(integer),
            mismatchcount(integer),
            gaps(integer),
            query_start(integer),
            query_end(integer),
            hit_start(integer),
            hit_end(integer),
            evalue(float),
            sw_score(float)
            ]).

table_spec(gff,
           [
            seq(atom),
            source(atom),
            type(atom),
            min(integer),
            max(integer),
            frame(integer),
            strand(atom),
            score(atom),
            group(atom)
           ]).

table_spec(gi_taxid,[gi(integer),taxid(integer)]).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality


unittest(load(blast)=
       connect_biotable('hsinsulin.blastcl3.blastn.m8',blast_tbl,Tbh)/Tbh).
unittest(load(gi_taxid)=
      connect_biotable('gi_taxid_nucl.test.gz',gzip(gi_taxid),Tbh)/
      Tbh).

unittest(test(blast_m8,
            [H=load(blast)],
            (   setof(Name,
                      Pos^E^(in_table(H,[hit_name(Name),evalue(E)],Pos),E<1e-40),
                      IDs),
                length(IDs,NumIDs)),
            (NumIDs=13))).

unittest(test(gi_taxid,
            [H=load(gi_taxid)],
            (   setof(GI,
                      Pos^in_table(H,[gi(GI),taxid(9615)],Pos),
                      GIs),
                length(GIs,NumGIs)),
            (NumGIs=141))).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.9 $
  @date  $Date: 2005/06/23 04:34:43 $
  @license LGPL

  ---+ Name
  ---++ biotable
- connect to a tabular data file

  ---+ Synopsis

  ==
  :- use_module(bio(biotable)).
  :- use_module(library(table)).
  
  demo:-
    % assumes we have run blast with -m8 option
    connect_biotable('hsinsulin.blastcl3.blastn.m8',blast_tbl,Tbh),
  
    % select all hit sequence IDs satisfying expect-value constraint
    setof(Name,
          Pos^E^(in_table(Tbh,[hit_name(Name),evalue(E)],Pos),E<1e-40),
          IDs),

    % show all IDs
    writeln(hit_ids = IDs).
  ==

  ---+ Description
  
  This module contains metadata on different common tabular
formats. this is for use in conjunction with the SWI table module,
which should be part of the default SWI setup. See the
@link(url='http://www.swi-prolog.org/packages/table.html')|SWI-Prolog
table module|

  ---++ Formats
  
  some of the built-in formats:

  
    * go_assoc : gene ontology gene association file format
    * inparanoid_tbl
    * blast_tbl : output of blast with -m8 option
    * blat : output of UCSC blat program
    * gff : GFF (all versions); (does not parse group field)
  

  see the module source for a full list, or query the module by
calling =|biotable:table_spec(Fmt,_)|= interactively

  ---++ Defining your own format

  In addition to the standard formats defined above, you can define
your own tabular formats. There are two ways of doing this: you can
either modify this prolog file (and send the patched file to the blip
developers), or you can add more definitions to this module, like
this:

  ==
  :- use_module(bio(biotable)).
  :- use_module(library(table)).   % standard SWI-Prolog module

  % define own file-format (3 columns) called 'myfmt'
  biotable:table_spec(myfmt,[id(atom),name(atom),score(float)]).

  % add a bioresource to bioconf: a file using own file format
  user:bioresource(my_res,'/users/me/data/myfile.myfmt',myfmt).
  
  % find all IDs with names beginning 'abc'
  t:-
    connect_biotable(my_res,Tbh),
    setof(ID,Pos^in_table(Tbh,[id(ID),name(abc,prefix)],Pos),IDs),
    writeln(IDs).
  ==

  
  */