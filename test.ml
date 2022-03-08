open Main;;
open Utils;;
open Lfsr;;

let p1 = [1; 3; 4];;
let p2 = [0; 2; 3];;
let p3 = [];;
let p4 = [1;2];;
let p5 = [1];;
let p6 = [0; 1; 2; 4; 8];;

sum_poly p1 p2;;
sum_poly p1 p3;;
sum_poly p1 p4;;

multNaive p1 p2;;
multNaive p1 p4;;
multNaive p5 p5;;

multKaratsuba p1 p2;;
multKaratsuba p1 p4;;
multKaratsuba p5 p5;;

quotient p6 p5;;
quotient p1 p4;;
quotient p1 p5;;

euclide p1 p4;;
euclide p1 p2;;

ordre [0; 1; 2; 3];;

est_irreductible [0; 1; 3];;
est_irreductible [1; 2; 3];;

irreductible 1;;
irreductible 2;;
irreductible 3;;

let prim = primitif 5;;
est_irreductible prim;;
ordre prim;;

let lfsr1 = {length=10; base=[1;0;0;1;0;0;1;0;0;1]; branch=[1;0;1;1;0;0;1;0;0;1]};;
let lgxrx = lgxrx_from_lfsr lfsr1;;
lfsr_from_lgxrx lgxrx;;


let lfsr2 = {length=3; base= [1;0;0]; branch=[0;0;1]};;
let lgxrx2 = lgxrx_from_lfsr lfsr2;;
lfsr_from_lgxrx lgxrx2;;

(*
display_n_value lfsr1 20;;
display_n_value lfsr2 20;;
*)

(*
let s = smallest_lgxrx lgxrx;;
lfsr_from_lgxrx s;;
*)

let bl = bon_lfsr 6;;
(*
display_n_value bl 72;;
*)



