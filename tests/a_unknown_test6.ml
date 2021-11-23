let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Fail: %d\n" id;
        assert (false);
    end;;

open ISet;;



let s = empty;;
test 1 (is_empty s) true;;


let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 2 (mem 10 (remove (10, 10) s)) false;;
test 3 (mem 7 (remove (8, 15) s)) true;;

let s = add (10, 12) empty;;
test 4 (mem 9 s) false;;
test 5 (mem 10 s) true;;
let s = add (14, 15) s;;
test 6 (mem 15 s) true;;
test 7 (mem 13 s) false;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 8 (mem (-10) s) false;;
test 9 (mem (-15) s) true;;

let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 10 (below 2 s = 0) true;;
test 11 (below 3 s = 1) true;;
test 12 (below 10 s = 5) true;;

let l, pres, r = split 9 s;;
test 13 (mem 9 l) false;;
test 14 (mem 9 r) false;;
test 15 (mem 8 l) true;;
test 16 (mem 10 r) true;;