/*

simple word-string based pattern matching

*/

wmatch([regulation,of|L],L,ID,cdef('GO:0050789',[regulates=ID])).
wmatch([Dir,regulation,of|L],L,ID,cdef('GO:0050789',
                                      [regulates=cdef(ID,[]),
                                       regulation_direction=cdef(Dir,[])])).

                             
