type lfsr = { length : int; base : int list; branch : int list; }
val ( +! ) : int -> int -> int
val ( *! ) : int -> int -> int
exception NotSameLengthLists
val calc : int list -> int list -> int
val lfsr_value : lfsr -> int -> int
val poly_from_lfsr_branch : lfsr -> int list
val until_i : 'a list -> int -> 'a list
val f2_sum : int list -> int list -> int
val gx_calc : lfsr -> int list
val lgxrx_from_lfsr : lfsr -> int * int list * int list
val lgxrx_length : 'a * 'b * 'c -> 'a
val lgxrx_gx : 'a * 'b * 'c -> 'b
val lgxrx_rx : 'a * 'b * 'c -> 'c
val branch_calc : int * 'a * int list -> int list
val base_calc : int * int list * int list -> int list
val lfsr_from_lgxrx : int * int list * int list -> lfsr
