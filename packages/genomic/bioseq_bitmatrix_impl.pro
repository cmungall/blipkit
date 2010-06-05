
:- dynamic bioseq_atom/2.
:- dynamic bioseq_bm/2.

sub_sequence_bm(Bits,(B,E),Sub) :-
        E>=B,
        Len is E-B,
        Sub1 is Bits >> B,
        Rest is (Bits >> E) << E,
        Sub is Sub1 xor Rest.

        
        

