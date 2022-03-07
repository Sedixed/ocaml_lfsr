open Utils;;

(* Somme de 2 polynômes p1 et p2 à coefficients dans F2 *)
let sum_poly p1 p2 =
	let max = let d1 = degree p1 and d2 = degree p2 in if d1 < d2 then d2 else d1 in
	let rec sum_aux p1 p2 aux i max =
		if i > max then List.rev aux else
		match (p1, p2) with
		| ([], []) -> List.rev aux
		| _ -> let c1 = (is_element_at p1 i) and c2 = (is_element_at p2 i) in 
				let c = ((c1 && not c2) || (not c1 && c2)) in 
				if c then
					sum_aux p1 p2 (i :: aux) (i + 1) max 
				else 
					sum_aux p1 p2 aux (i + 1) max
	in sum_aux p1 p2 [] 0 max;;


(* Multiplication naïve de 2 polynômes p et q à coefficients dans F2 *)
let multNaive p q =
	let rec multNaive_aux p q aux =
		match p, q with
		| [], _ -> aux
		| h1 :: t1, q ->  let acc = (multXn q h1) in 
						multNaive_aux t1 q (sum_poly acc aux)
	in multNaive_aux p q [];;


(* Multiplication rapide de 2 polynômes p et q à coefficients dans F2 *)
let rec multKaratsuba p q =
	if (max (degree p) (degree q)) < 5 then 
		multNaive p q 
	else 
		let k = let t = max (degree p) (degree q) in if t mod 2 = 0 then t else t + 1 in
		let (a0, a1) = cut p (k / 2) and (b0, b1) = cut q (k / 2) in
		let c0 = multKaratsuba a0 b0 and c2 = multKaratsuba a1 b1 in
		let u = multKaratsuba (sum_poly a0 a1) (sum_poly b0 b1) in
		let c1 = sum_poly (sum_poly u c0) c2 in
			sum_poly c0 (sum_poly (multXn c1 (k / 2)) (multXn c2 k));;


(* Inverse de p mod X^n *)
let inverse_mod p n =
	let rec inv_aux n acc p i =
		if i > int_log2 n then
			acc
		else
  		let fst_member = (mult_coeff acc 2) in
			let snd_member = mult_coeff (multKaratsuba p (multKaratsuba acc acc)) (-1) in
			let sub = sum_poly fst_member snd_member in
			let r = moduloXn sub (twoPowN i) in
				inv_aux n r p (i + 1)
	in inv_aux n [0] p 1;;


(* Exception utilisée en cas de division par le polynôme nul (liste vide) *)
exception DivisionByNullPolynom;;


(* Quotient rapide du polynôme a par le polynôme b, tous deux à coefficients dans F2 *)
let quotient a b =
	if b = [] then 
		raise DivisionByNullPolynom 
	else if (degree a) < (degree b) then 
		([], a) 
	else
		let n = (degree a) and m = (degree b)
		and renvB = renverse b (degree b)
		and renvA = renverse a (degree a)
		in let borne = (n - m + 1) in
			let p = inverse_mod renvB borne in
			let mult = multKaratsuba renvA p in 
			let modMult = moduloXn mult borne in
			let q = renverse (modMult) (n - m)
		in (q, (sum_poly a (mult_coeff (multKaratsuba b q) (-1))));;


(* Algorithme d'euclide sur les polynômes de F2 *)
let rec euclide p q =
	if q = [] then 
		p
	else
		let (q1, r1) = quotient p q in
		euclide q r1;;

exception InvalidPolynom;;


(* Renvoie l'ordre du polynôme p *)
let ordre p =
	if p = [] || List.hd p <> 0 || (degree p) < 2 then
		raise InvalidPolynom
	else
		let rec aux order p =
			let (q, r) = quotient [order] p in
			if r = [0] then 
				order
			else
				aux (order + 1) p
		in aux 1 p;;


(* Calcule le prochain polynôme à tester dans la fonction est_irreductible *)
let next_poly bd = function
	| p when List.length p = bd -> []
	| p when List.length p = List.hd (List.rev p) + 1 -> [List.hd (List.rev p) + 1]
	| h :: [] as p -> 0 :: p
	| h :: t as p when h = (List.hd t) - 1 -> 0 :: p
	| h :: t -> (h + 1) :: t
	| _ -> [];;


(* Applique la somme avec le monôme x à tous les éléments de list *)
let apply_sum list x =
	let rec aux acc x = function
		| [] -> List.rev acc
		| h :: t -> aux ((sum_poly x h) :: acc) x t
	in aux [] x list;;


(* Renvoie la liste de tous les polynômes de degré inférieur à n *)
let all_poly_below n =
	let rec aux acc n = function
	| i when i = 0 -> aux ([0] :: acc) n (i + 1)
	| i when i = 1 -> aux (([1] :: ([0;1] :: acc))) n (i + 1)
	| i when i = n -> List.rev acc
	| i -> let e = [i] in let newlist = apply_sum acc e in aux (acc @ newlist) n (i + 1)
	in aux [] n 0;;


(* Retourne true ssi le polynôme p est irréductible *)
let est_irreductible p =
	if (degree p) = 1 then true else
	let rec aux p = function
		| [] -> true
		| h :: t -> let pgcd = euclide h p in 
				if pgcd <> [0] then false else aux p t
	in aux p (all_poly_below (degree p));;


exception InvalidDegree;;

(* Génère un polynôme irréductible de degré n *)		
let irreductible n =
	if n <= 0 then 
		raise InvalidDegree
	else
		let rec aux n = function
			| [] -> failwith "Polynôme non existant"
			| h :: t -> if (degree h) < n || not (est_irreductible h) then
							aux n t
						else
							h
		in aux n (all_poly_below (n + 1));;


(* Génère un polynôme primitif de degré n > 0 *)
let primitif n =
	if n <= 0 then
		raise InvalidDegree
	else
		let rec aux n = function
			| [] -> failwith "Polynôme non existant"
			| h :: t -> if (degree h) < n || not (est_irreductible h) || not (ordre h = (twoPowN n) - 1) then
										aux n t
									else
										h
		in aux n (all_poly_below (n + 1));;






	