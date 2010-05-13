/*

simple word-string based pattern matching

*/



rpattern([regulation,of|L],
         L,
         ID,
         cdef('GO:0050789',[regulates=cdef(ID,[])]),
         belongs(ID,biological_process)).
rpattern([negative,regulation,of|L],
         L,
         ID,
         cdef('GO:0048519',[regulates=cdef(ID,[])]),
         belongs(ID,biological_process)).
rpattern([positive,regulation,of|L],
         L,
         ID,
         cdef('GO:0048518',[regulates=cdef(ID,[])]),
         belongs(ID,biological_process)).
