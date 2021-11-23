(* Tests to iSet.ml *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    assert (false);
  end

open ISet;;

let a = empty;;
let a = add (2, 5) a;;
let a = add (7, 10) a;;
let a = add (12, 20) a;;
let a = add (0, 0) a;;

test 1 (mem 1 a = false);;
test 2 (mem 2 a = true);;
test 3 (mem 9 a = true);;
test 4 (mem 21 a = false);;

let elem = elements a;;

test 5 (elem = [(0,0);(2,5);(7,10);(12,20)]);;
test 6 (below 6 a == 5);;
test 7 (below 10 a == 9);;
test 8 (below 19 a == 17);;

let (l,_,r) = split 15 a;;

test 9 (elements l = [(0,0);(2,5);(7,10);(12,14)]);;
test 10 (elements r = [(16,20)]);;

let (l,_,r) = split 8 a;;

test 11 (elements l = [(0,0);(2,5);(7,7);]);;
test 12 (elements r = [(9,10);(12,20)]);;


let a = add (6, 6) a;;
let b = add (11, 11) a;;

test 13 (elements a = [(0,0);(2,10);(12,20)]);;
test 14 (elements b = [(0,0);(2,20)]);;

let b = empty;;
let b = add (-10, 5) b;;
let b = add (10, 34) b;;
test 15 (elements b  = [(-10,5);(10,34)]);;

let b = add (22, 40) b;;
test 16 (elements b  = [(-10, 5);(10, 40)]);;

let b = add (41, 45) b;;
test 17 (elements b  = [(-10, 5);(10, 45)]);;

let b = add (80, 102) b;;
let b = add (130, 220) b;;
test 18 (elements b  = [(-10, 5);(10, 45);(80,102);(130,220)]);;

let b = add (45, 140) b;;
test 19 (elements b  = [(-10, 5);(10, 220)]);;


let c = empty;;
let c = add (4, max_int) c;;
let c = add (min_int, 0) c;;

test 20 (mem 4 c = true);;
test 21 (mem 0 c = true);;
test 22 (mem 20 c = true);;
test 23 (mem 2 c = false);;
test 24 (elements c = [(min_int, 0);(4, max_int)]);;
test 25 (below 0 c = max_int);;
test 26 (below max_int c = max_int);;


let d = empty;;
let d = add (min_int, max_int) d;;

test 27 (below 0 c = max_int);;
test 28 (below (-2) c = max_int);;
test 29 (below min_int c = 1);;
test 30 (below (min_int+1) c = 2);;



(*	(elements a = [(0,0);(2,10);(12,20)])
	(elements b  = [(-10, 5);(10, 220)])
	(elements c = [(min_int, 0);(4, max_int)]) *)

let a = remove (5,6) a;;
test 50 (elements a = [(0,0);(2,4);(7,10);(12,20)]);;
test 51 (below 11 a = 8);;
let (l,x,_) = split 1 a;;
test 52 ((elements l, x) = ([(0,0)], false));;
let (_,x,r) = split 20 a;;
test 53 ((x, r) = (true, empty));;

let pom = a;;


let a = remove (12,19) a;;
test 54 (elements a = [(0,0);(2,4);(7,10);(20,20)]);;
test 55 (mem 19 a = false);;
test 56 (below 19 a = 8);;
test 57 (below 1 a = 1);;
test 58 (below 5 a = 4);;

let (l, x, r) = split 7 a;;
test 59 (x = true);;
test 60 (elements l = [(0,0);(2,4)]);;
test 61 (elements r = [(8,10);(20,20)]);;


let a = remove (1,1) a;;
test 62 (elements a = [(0,0);(2,4);(7,10);(20,20)]);;
test 63 (mem 19 a = false);;
test 64 (below 20 a = 9);;
test 65 (below 1 a = 1);;
test 66 (below 5 a = 4);;

let (l, x, r) = split 7 a;;
test 67 (x = true);;
test 68 (elements l = [(0,0);(2,4)]);;
test 69 (elements r = [(8,10);(20,20)]);;


let a = remove (0,20) a;;
test 70 (elements a = []);;
test 71 (below max_int a = 0);;
test 72 (is_empty a = true);;


(* elements pom = [(0,0);(2,4);(7,10);(12,20)] *)
let x = ref 0;;
let f (c, d) = 
	x := !x + c;;
iter f pom;;
test 100 (!x = 21);;

let x = fold (fun (c,d) acc -> acc + (d-c+1)) pom 0;;
test 101 (x = below 100 pom);;