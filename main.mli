val sum_poly : int list -> int list -> int list
val multNaive : int list -> int list -> int list
val multKaratsuba : int list -> int list -> int list
val inverse_mod : int list -> int -> int list
exception DivisionByNullPolynom
val quotient : int list -> int list -> int list * int list
val euclide : int list -> int list -> int list
exception InvalidPolynom
val ordre : int list -> int
val next_poly : int -> int list -> int list
val apply_sum : int list list -> int list -> int list list
val all_poly_below : int -> int list list
val est_irreductible : int list -> bool
exception InvalidDegree
val irreductible : int -> int list
val primitif : int -> int list
