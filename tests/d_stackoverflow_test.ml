(* Copyright (C) 2018 Antoni Żewierżejew *)

open ISet;;

print_endline "Test stack overflow (may take a while)
Also don't forget to limit stack memory.
Should work even on 64 KB";;

let rec construct n acc = if n = 0 then acc else construct (n - 1) ((n * 2, n * 2)  :: acc);;

let print_int a = print_int a; print_newline ();;


print_endline "List of length 100000";;

let long = construct 100000 [];;

let b = List.fold_left (fun a i -> add i a) empty long;;

let sum = fold (fun (x, y) s -> s + y - x + 1) b 0;;

assert(sum = 100000);;

let e = elements b;;

assert(e = long);;

iter (fun (x, y) -> if x = x land (-x) then print_int x) b;;

print_endline "Correct";;


print_endline "List of length 1000000";;

let long = construct 1000000 [];;

let b = List.fold_left (fun a i -> add i a) empty long;;

let sum = fold (fun (x, y) s -> s + y - x + 1) b 0;;

assert(sum = 1000000);;

let e = elements b;;

assert(e = long);;

iter (fun (x, y) -> if x = x land (-x) then print_int x) b;;

print_endline "Stack overflow test passed";;
