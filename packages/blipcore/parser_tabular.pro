/* -*- Mode: Prolog -*- */


:- module(parser_tabular,
          [
          ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- [parser_general].

:- dynamic default_namespace/1.

% TODO - handle obsoletes

io:parse_stream(tabular_native,IO):-
        read_records(IO).
io:parse_stream(tabular,IO):-
        read_records(IO).
io:parse_stream_with_cache(tabular_native,IO,File):-
        read_records_with_cache(IO,File).
io:parse_stream_with_cache(tabular,IO,File):-
        read_records_with_cache(IO,File).

read_records_with_cache(IO,File):-
        tell(File),
        read_records(IO,on),
        told.

read_records(IO):-
        repeat,
        read_line_to_codes(IO,Codes),
        (   Codes=end_of_file
        ->  !
        ;   atom_codes(Line,Codes),
            parse_line(Line,Facts),
            maplist(assert(Facts)),
            fail).

parse_line(Line,Facts):-
        concat_atom(Vals,'\t',Line),
        record_to_facts(Vals,Facts).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/08/24 23:51:06 $
  @license LGPL

  ---+ Name
  ---++ parser_tabular
- parses tabular 1.0 and 1.2 ontology formats

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).

  demo:-
    load_biofile(tabular_native,'sofa.tabular'),
    write_biofile(tabular,'sofa.tabular'),
    class(ID,'transcript'),
    setof(ParentID,subclassRT(ID,ParentID),ParentIDs),
    writeln(parents=ParentIDs).    
  ==

  ---+ Description

  parses tabular format files - stores data in the ontol_db data module,
as predicates class/2, subclass/2, restriction/3, etc
  
  ---++ See also
  
  <http://www.geneontology.org> GO

  */