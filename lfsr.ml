open Utils;;
open Main;;

(* length : longueur du LFSR *)
(* base : valeurs r0, r1, .., r(length - 1) *)
(* branch : valeurs des branchements a1, a2, .., a(length) *)

(* r0, r1, r2, ..r(length -1) *)

type lfsr = {
	length : int;
	base : int list;
	branch : int list
};;


(* Le + de F2 *)
let ( +! ) x y = 
	match x, y with
	| 0, 1 | 1, 0 -> 1
	| _ -> 0;;


(* Le * de F2 *)
let ( *! ) x y = 
	match x, y with
	| 1, 1 -> 1
	| _ -> 0;;


exception NotSameLengthLists;;

(* Calcul d'un r_i selon les valeurs de branchements d'un LFSR et d'un accumulateur contenant les valeurs *)
(* n�cessaires au calcul *)
let calc branch acc = 
  let rec aux branch acc res = 
    match branch, acc with
    | [], [] -> res
    | h1 :: t1, h2 :: t2 -> aux t1 t2 (res +! (h1 *! h2))
    | _, _ -> raise NotSameLengthLists
  in aux branch (List.rev acc) 0;;


(* Calcule la valeur au rang n produite par le LFSR lfsr *)
let lfsr_value lfsr n =
  if n < lfsr.length then 
    List.nth lfsr.base n
  else
    let rec aux n lfsr acc r = function
      | i when i > n ->  r
      | i -> let vi = calc lfsr.branch acc in aux n lfsr (List.rev (vi :: (List.rev (List.tl acc)))) vi (i + 1)
    in aux n lfsr lfsr.base 0 lfsr.length;;

(* Affiche les n premi�res valeurs g�n�r�es par le LFSR lfsr *)
let display_n_value lfsr n =
  let () = Printf.printf "\n" in
  let rec aux lfsr n = function
    | i when i = n -> ()
    | i -> let () = Printf.printf "r_%d = %d\n" i (lfsr_value lfsr i) in aux lfsr n (i + 1)
  in aux lfsr n 0;;


(* Affiche les n premi�res valeurs g�n�r�es par le LFSR lfsr sous forme de blocs de longueur celle du LFSR *)
(* afin de voir plus facilement la p�riodicit� *)
let display_n_value_debug lfsr n =
  let () = Printf.printf "\n" in
  let rec aux lfsr n = function
    | i when i = n -> ()
    | i -> let () = if i mod lfsr.length = 0 then Printf.printf "\n%2d   " i else Printf.printf "" in let () = Printf.printf "%d " (lfsr_value lfsr i) in aux lfsr n (i + 1)
  in aux lfsr n 0;; 


(* Calcul de R(X) *)
let rx_calc lfsr = 
  let rec aux acc degree = function
    | [] -> List.rev acc
    | h :: t when h = 1 -> aux (degree :: acc) (degree + 1) t
    | h :: t -> aux acc (degree + 1) t
  in aux [0] 1 lfsr.branch;;


(* Retourne la liste l priv�e de ses �l�ments d�passant l'indice i *)
let until_i l i =
  let rec aux acc j i = function
    | l when j > i -> List.rev acc
    | h :: t -> aux (h :: acc) (j + 1) i t
    | _ -> List.rev acc
  in aux [] 0 i l;;


(* Retourne la somme des produits des �l�ments des 2 listes *)
let f2_sum l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | h1 :: t1, h2 :: t2 -> let c = h1 *! h2 in aux (acc +! c) t1 t2
    | _ -> raise NotSameLengthLists
  in aux 0 l1 l2;;


(* Calcul de G(X) *)
let gx_calc lfsr = 
  let rec aux poly j lfsr =
    let branch = until_i (1 :: lfsr.branch) j and base = until_i lfsr.base j in
    match (List.rev branch), base with
    | br, b when j = lfsr.length -> List.rev poly
    | br, b -> let s = f2_sum br b in 
        if s = 0 then 
          aux poly (j + 1) lfsr 
        else 
          aux (j :: poly) (j + 1) lfsr
  in aux [] 0 lfsr;;


(* Calcul du triplet (l, G(X), R(X)) � partir d'un LFSR *)
let lgxrx_from_lfsr lfsr = (lfsr.length, (gx_calc lfsr), (rx_calc lfsr));;


(* Fonctions pour r�cup�rer les divers �l�ments du triplet (l, G(X), R(X)) *)
let lgxrx_length (l, gx, rx) = l;;
let lgxrx_gx (l, gx, rx) = gx;;
let lgxrx_rx (l, gx, rx) = rx;;


(* Calcul des valeurs de branchement � partir du triplet (l, G(X), R(X)) *)
let branch_calc lgxrx =
	if (lgxrx_rx lgxrx) = [] then failwith "liste vide" else 
  let rx = List.tl (lgxrx_rx lgxrx) in List.tl (poly_to_binary rx (lgxrx_length lgxrx + 1));;


(* Calcul des valeurs initiales r0.. r_l-1 � partir du triplet (l, G(X), R(X)) *)
let base_calc lgxrx =
  let gx = (lgxrx_gx lgxrx) and rx = (lgxrx_rx lgxrx) in
  let (a, b) = quotient gx rx and len = lgxrx_length lgxrx in
  let s = moduloXn (sum_poly a (multKaratsuba b (inverse_mod rx len))) len in
  poly_to_binary s (lgxrx_length lgxrx);; 
  

(* Calcul d'un LFSR � partir d'un triplet (l, G(X), R(X)) *)
let lfsr_from_lgxrx lgxrx = {length=(lgxrx_length lgxrx); base=(base_calc lgxrx); branch=(branch_calc lgxrx)};;


(* Produit le triplet (l, G(X), R(X)) minimal � partir du triplet lgxrx *)
(* � revoir : degr� � v�rifier *)
let smallest_lgxrx lgxrx =
	if (lgxrx_gx lgxrx) = (lgxrx_rx lgxrx) then lgxrx else
  let tx = euclide (lgxrx_gx lgxrx) (lgxrx_rx lgxrx) in
	if tx = [0] then lgxrx else
  let (q1, r1) = quotient (lgxrx_gx lgxrx) tx and (q2, r2) = quotient (lgxrx_rx lgxrx) tx in
	let () = Printf.printf "rx : " in
	let () = List.iter (Printf.printf "%d ") (lgxrx_rx lgxrx) in let () = Printf.printf "\nq2 : " in 
	let () = List.iter (Printf.printf "%d ") q2 in let () = Printf.printf "\ntx : " in
	let () = List.iter (Printf.printf "%d ") tx in let () = Printf.printf "\n" in 
  ((degree q2), q1, q2);;


(* Produit le LFSR minimal � partir du LFSR lfsr *)
let smallest_lfsr lfsr =
  lfsr_from_lgxrx (smallest_lgxrx (lgxrx_from_lfsr lfsr));;


(* G�n�re un polyn�me pseudo-al�atoire sous forme de liste binaire de longueur l *)
let random_poly l = 
  let rec aux acc l = function
    | i when i = l -> List.rev acc
    | i -> let b = Random.bool () in if b = true then aux (1 :: acc) l (i + 1) else aux (0 :: acc) l (i + 1)
  in aux [] l 0;;


(* revoir complexit� *)
(* G�n�re un bon LFSR de longueur l *)
let rec bon_lfsr l =
  let rxp = primitif (l - 1) and sxp = binary_to_poly (random_poly l) in
  let gxp = moduloXn (multKaratsuba rxp sxp) l in
  if gxp = [] then bon_lfsr l else lfsr_from_lgxrx (l, gxp, rxp);; 


(* pas s�r du mod lfsr.length, ni du fonctionnement de la base *)
let plaintext_coding lfsr text =
  let rec aux acc lfsr i base = function
    | [] -> List.rev acc
    | h :: t -> let ri = (lfsr_value lfsr i) in let c = h +! ri in
        let newbase = (List.rev (ri :: (List.rev (List.tl base)))) in
        aux (c :: acc) lfsr ((i + 1) mod lfsr.length) newbase t
  in aux [] lfsr 0 lfsr.base text;;


(* m�me fonction car m�me fonctionnement MDR *)
let plaintext_decoding lfsr text = plaintext_coding lfsr text;;


(* todo : remplacer failwith par une exception *)
(* revoir l'algo comme dans le cahier *)
let ciphertext_decoding base_seq =
  let len = (List.length base_seq) in
  if len mod 2 <> 0 then 
    failwith "Nombre impair : 2*l pas possible" 
  else
    let r0 = [len] and r1 = binary_to_poly base_seq and
		(* [] <=> 0, [0] <=> 1 *)
    a0 = [0] and a1 = [] and b0 = [] and b1 = [0] in
    let rec aux rk rk_minus am1 am2 bm1 bm2 qm = 
      let (q, r) = quotient rk_minus rk in 
      if r = [] then
        (bm1, rk) (* peut �tre pas rk mais rk_m / rk et peut�tre bm1 *)
      else 
        let am = sum_poly am2 (multKaratsuba am1 qm)
				and bm = sum_poly bm2 (multKaratsuba bm1 qm)
        in aux r rk am am1 bm bm1 q 
    in let (bm, rm) = aux r0 r1 a0 a1 b0 b1 (* jsp quoi mettre comme q de base donc [0] *) [0] in
		let d = max (degree bm) ((degree rm) + 1) in let rxtmp = renverse bm d in
		let rx = if List.hd rxtmp = 0 then rxtmp else 0 :: rxtmp in
		let gx = multKaratsuba rx (binary_to_poly base_seq) in 
		(smallest_lfsr (lfsr_from_lgxrx (len, gx, rx)),
		lfsr_from_lgxrx (len, gx, rx));;


(* todo : revoir ciphertext, smallest *)




