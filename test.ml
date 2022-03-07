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
let s = smallest_lgxrx lgxrx;;
lfsr_from_lgxrx s;;
*)

let bl = bon_lfsr 6;;

(*
lfsr_value lfsr1 0;;
lfsr_value lfsr1 1;;
lfsr_value lfsr1 2;;
lfsr_value lfsr1 3;;
lfsr_value lfsr1 4;;
lfsr_value lfsr1 5;;
lfsr_value lfsr1 6;;
lfsr_value lfsr1 7;;
lfsr_value lfsr1 8;;
lfsr_value lfsr1 9;;
lfsr_value lfsr1 10;;
lfsr_value lfsr1 11;;
lfsr_value lfsr1 12;;
lfsr_value lfsr1 13;;
lfsr_value lfsr1 14;;
lfsr_value lfsr1 15;;
lfsr_value lfsr1 16;;
lfsr_value lfsr1 17;;
lfsr_value lfsr1 18;;
lfsr_value lfsr1 19;;
Printf.printf "\n\n\n";;
lfsr_value lfsr2 0;;
lfsr_value lfsr2 1;;
lfsr_value lfsr2 2;;
lfsr_value lfsr2 3;;
lfsr_value lfsr2 4;;
lfsr_value lfsr2 5;;
lfsr_value lfsr2 6;;
lfsr_value lfsr2 7;;
lfsr_value lfsr2 8;;
lfsr_value lfsr2 9;;
lfsr_value lfsr2 10;;
lfsr_value lfsr2 11;;
lfsr_value lfsr2 12;;
lfsr_value lfsr2 13;;
lfsr_value lfsr2 14;;
lfsr_value lfsr2 15;;
lfsr_value lfsr2 16;;
lfsr_value lfsr2 17;;
lfsr_value lfsr2 18;;
lfsr_value lfsr2 19;;
*)
