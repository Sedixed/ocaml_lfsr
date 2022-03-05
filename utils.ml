(* On d�finit le type f2poly comme le type repr�sentant les polyn�mes *)
(* � coefficient dans F2. Nous utiliserons la repr�sentation creuse, et les *)
(* valeurs contenues dans la liste repr�sentent les degr�s o� sont pr�sent le coefficient 1. *)
(* Ces valeurs sont rang�es par ordre croissant selon le degr�.*)
(* Par exemple, le polyn�me 1 + X + X� sera repr�sent� de cette fa�on :*)
(* [0; 1; 3] *)

type f2poly = int list;;

(* Renvoie le degr� du polyn�me p � coefficients dans F2, -1 si la liste est vide. *)
let degree p = if p = [] then -1 else List.hd (List.rev p);;

(* Renvoie true ssi le coefficient au degr� i n'est pas 0 *)
let is_element_at poly i = 
	let rec aux i poly =
		match poly with
			| [] -> false
			| h :: t -> if i = h then true else aux i t
	in aux i poly;;

(* Multiplie les coefficients de p par a, a appartenant � F2 *)
let mult_coeff p a = if a mod 2 = 0 then [] else p;;

(* Multiplie chaque mon�me de p par X^n *)
let multXn p n =
	let rec multXn_aux n aux = function
		| [] -> List.rev aux
		| h :: t -> multXn_aux n ((h + n):: aux) t
	in multXn_aux n [] p;;

(* Renvoie le couple de polyn�mes de F2 (p0, p1) tels que p = p0 + X^i * p1 *)
let cut p i =
	let rec cut_aux p0 p1 i = function
		| [] -> (List.rev p0, List.rev p1)
		| h :: t -> if h < i then 
									cut_aux (h :: p0) p1 i t 
								else 
									cut_aux p0 ((h - i) :: p1) i t
	in cut_aux [] [] i p;;

(* Renvoie le renvers� d'ordre k du polyn�me p *)
let renverse p k =
	let rec renv_aux k acc = function
		| [] -> multXn acc k
		| h :: t -> renv_aux k (-h :: acc) t
	in renv_aux k [] p;;

(* Renvoie le polyn�me p modulo X^n *)
let moduloXn p n = 
	let rec mod_aux n acc = function
		| [] -> List.rev acc
		| h :: t -> if h < n then mod_aux n (h :: acc) t else mod_aux n acc t
	in mod_aux n [] p;;

(* Renvoie 2^n *)
let twoPowN n = int_of_float (2. ** float_of_int n);;

(* Renvoie la partie enti�re sup�rieure de log2(n) *)
let int_log2 n = int_of_float (Float.ceil (Float.log (float_of_int n) /. Float.log 2.));;





