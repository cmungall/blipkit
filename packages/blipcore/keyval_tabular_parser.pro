:- module(tagval_parser,
          [stream_to_kvl_list/3,
           stream_to_kvl_list/4,
           line_to_kvl/4
          ]).

/** <module> parses tabular files into key-val pairs

  ---+ Synopsis

---+ Details

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/

%% line_to_kvl(+Line,+Hdrs:list,?KVL:list,+Opts:list) is det
line_to_kvl(Line,Hdrs,KVL,Opts) :-
        atomic_list_concat(Vals,'\t',Line),
        vals_to_kvl(Vals,Hdrs,KVL,Opts).

vals_to_kvl([],_,[],_).
vals_to_kvl([''|Vals],[_|Hdrs],KVL,_) :-
        !,
        vals_to_kvl(Vals,Hdrs,KVL).
vals_to_kvl([V|Vals],[H|Hdrs],[KV|KVL],Opts) :-
        make_kv(H,V,KV,Opts),
        vals_to_kvl(Vals,Hdrs,KVL,Opts).

lines_to_kvl_list([],_,[],_).
lines_to_kvl_list([A|As],Hdrs,[KVL|KVLs],Opts) :-
        line_to_kvl(A,Hdrs,KVL,Opts),
        lines_to_kvl_list(As,Hdrs,KVLs,Opts).

make_kv(H,V,KV,Opts) :-
        member(functor(key),Opts),
        !,
        KV=..[H,V].
make_kv(H,V,KV,Opts) :-
        member(functor(F),Opts),
        !,
        KV=..[F,H,V].
make_kv(H,V,H=V,_).

%% stream_to_kvl_list(+S,?KVLs:list,+Opts:list) is det
%% stream_to_kvl_list(+S,+Keys:list,?KVLs:list,+Opts:list) is det
stream_to_kvl_list(S,KVLs,Opts) :-
        read_keys_from_stream(S,Hdrs,Opts),
        stream_to_kvl_list(S,Hdrs,KVLs,Opts).

stream_to_kvl_list(S,_,[],_) :-
        at_end_of_stream(S),
        !.
stream_to_kvl_list(S,Hdrs,[KVL|KVLs],Opts) :-
        read_line_to_codes(S,Codes),
        atom_codes(Line,Codes),
        line_to_kvl(Line,Hdrs,KVL),
        stream_to_kvl_list(S,Hdrs,KVLs,Opts).

%% read_keys_from_stream(+S,?Hdrs:list,+Opts:list)
read_keys_from_stream(S,Hdrs,_Opts) :-
        read_line_to_codes(S,Codes),
        atom_codes(Line,Codes),
        atomic_list_concat(Hdrs,'\t',Line).

                
