open Main;;
open Utils;;
open Lfsr;;

(* Polyn�mes de test *)
let p1 = [1; 3; 4];;
let p2 = [0; 2; 3];;
let p3 = [];;
let p4 = [1;2];;
let p5 = [1];;
let p6 = [0; 1; 2; 4; 8];;

(* Somme *)
sum_poly p1 p2;;
sum_poly p1 p3;;
sum_poly p1 p4;;

(* Multiplication na�ve *)
multNaive p1 p2;;
multNaive p1 p4;;
multNaive p5 p5;;

(* Multiplication de Karatsuba *)
multKaratsuba p1 p2;;
multKaratsuba p1 p4;;
multKaratsuba p5 p5;;

(* Quotient *)
quotient p6 p5;;
quotient p1 p4;;
quotient p1 p5;;

(* Algorithme d'Euclide : PGCD *)
euclide p1 p4;;
euclide p1 p2;;

(* Ordre, irreductible, primitif *)
ordre [0; 1; 2; 3];;

est_irreductible [0; 1; 3];;
est_irreductible [1; 2; 3];;

irreductible 1;;
irreductible 2;;
irreductible 3;;

let prim = primitif 5;;
est_irreductible prim;;
ordre prim;;

(* LFSR de test *)
let lfsr1 = {length=10; base=[1;0;0;1;0;0;1;0;0;1]; branch=[1;0;1;1;0;0;1;0;0;1]};;
let lgxrx = lgxrx_from_lfsr lfsr1;;
lfsr_from_lgxrx lgxrx;;

let lfsr2 = {length=3; base= [1;0;0]; branch=[0;0;1]};;
let lgxrx2 = lgxrx_from_lfsr lfsr2;;
lfsr_from_lgxrx lgxrx2;;

(* Affichage des 20 premi�res valeurs des 2 LFSR *)
(*
display_n_value lfsr1 20;;
display_n_value lfsr2 20;;
*)

(* LFSR minimal *)
lfsr_from_lgxrx (smallest_lgxrx lgxrx);;

(* Bon LFSR *)
let bl = bon_lfsr 8;; 

(* Chiffrement / d�chiffrement *)
let text = random_poly ((twoPowN 8) + 5) ;;
let encoded = plaintext_coding bl text;;
let decoded = plaintext_decoding bl encoded;;
text = decoded;;

let rd = (random_poly 10);;
let (lfsr_r, lfsr_notr) = ciphertext_decoding rd;;


display_n_value lfsr_r 10;;





