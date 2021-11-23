(* oficjalny test z około 2016 czas wykonywania pomiędzy 1 a 3 min *)

(* emacs:
do przerabiania outputu toplevela na test 2y
M-x query-replace-regexp
# \(.*\);;[ 	]*
.* =[ 	
]*\(.*\)
na
test 8 ( = );;
*)

let zle = ref 0

let test n b =
  if not b then (
    Printf.printf "Zly wynik testu %d!!\n" n ;
    incr zle )

open ISet

(* pierwsze testy *)
let s = empty

let s = add (3, 7) s

;;
test 23 (elements s = [(3, 7)])

let s = add (3, 7) s

;;
test 25 (elements s = [(3, 7)])

let s = add (7, 9) s

;;
test 27 (elements s = [(3, 9)])

let s = add (10, 13) s

;;
test 29 (elements s = [(3, 13)])

let s = add (1, 2) s

;;
test 31 (elements s = [(1, 13)])

let s = add (15, 28) s

;;
test 33 (elements s = [(1, 13); (15, 28)])

let s = add (14, 14) s

;;
test 35 (elements s = [(1, 28)])

;;
test 36 (below 17 s = 17)

;;
test 37 (below 17 s = 17)

;;
test 38 (below 50 s = 28)

(* testy konkretne *)

let s = empty

;;
test 45 (is_empty s)

let s = add (-1, 1) s

;;
test 47 (not (is_empty s))

let s = add (3, 4) s

let s = add (6, 7) s

let s = add (9, 10) s

;;
test 51 (not (is_empty s))

let s = add (14, 15) s

let s = add (17, 18) s

let s = add (20, 21) s

;;
test 55 (not (is_empty s))

;;
test 56
  ( elements s
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 57 (fold (fun _ i -> i + 1) s 0 = 7)

(* add *)

;;
test 61
  ( elements (add (-10, -5) s)
  = [(-10, -5); (-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)]
  )

;;
test 62
  ( elements (add (-10, -2) s)
  = [(-10, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 63
  ( elements (add (-10, -1) s)
  = [(-10, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 64
  ( elements (add (-10, 0) s)
  = [(-10, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 65
  ( elements (add (-10, 1) s)
  = [(-10, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 66
  ( elements (add (-10, 2) s)
  = [(-10, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 67 (elements (add (-10, 8) s) = [(-10, 10); (14, 15); (17, 18); (20, 21)])

;;
test 68 (elements (add (-10, 12) s) = [(-10, 12); (14, 15); (17, 18); (20, 21)])

;;
test 69 (elements (add (-10, 17) s) = [(-10, 18); (20, 21)])

;;
test 70 (elements (add (-10, 19) s) = [(-10, 21)])

;;
test 71 (elements (add (-10, 21) s) = [(-10, 21)])

;;
test 72 (elements (add (-10, 25) s) = [(-10, 25)])

;;
test 73 (elements (add (-1, 25) s) = [(-1, 25)])

;;
test 74 (elements (add (0, 25) s) = [(-1, 25)])

;;
test 75 (elements (add (1, 25) s) = [(-1, 25)])

;;
test 76 (elements (add (2, 25) s) = [(-1, 25)])

;;
test 77 (elements (add (3, 25) s) = [(-1, 1); (3, 25)])

;;
test 78 (elements (add (11, 25) s) = [(-1, 1); (3, 4); (6, 7); (9, 25)])

;;
test 79
  (elements (add (12, 25) s) = [(-1, 1); (3, 4); (6, 7); (9, 10); (12, 25)])

;;
test 80
  (elements (add (15, 25) s) = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 25)])

;;
test 81
  ( elements (add (19, 25) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 25)] )

;;
test 82
  ( elements (add (20, 25) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 25)] )

;;
test 83
  ( elements (add (21, 25) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 25)] )

;;
test 84
  ( elements (add (22, 25) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 25)] )

;;
test 85
  ( elements (add (23, 25) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21); (23, 25)]
  )

;;
test 86
  ( elements (add (25, 25) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21); (25, 25)]
  )

;;
test 87 (elements (add (2, 12) s) = [(-1, 12); (14, 15); (17, 18); (20, 21)])

;;
test 88
  (elements (add (3, 12) s) = [(-1, 1); (3, 12); (14, 15); (17, 18); (20, 21)])

;;
test 89
  (elements (add (4, 12) s) = [(-1, 1); (3, 12); (14, 15); (17, 18); (20, 21)])

;;
test 90
  (elements (add (5, 12) s) = [(-1, 1); (3, 12); (14, 15); (17, 18); (20, 21)])

;;
test 91
  ( elements (add (6, 12) s)
  = [(-1, 1); (3, 4); (6, 12); (14, 15); (17, 18); (20, 21)] )

;;
test 92
  (elements (add (6, 13) s) = [(-1, 1); (3, 4); (6, 15); (17, 18); (20, 21)])

;;
test 93 (elements (add (5, 13) s) = [(-1, 1); (3, 15); (17, 18); (20, 21)])

(* remove *)

;;
test 97
  ( elements (remove (-10, -2) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 98
  ( elements (remove (-10, -1) s)
  = [(0, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 99
  ( elements (remove (-10, 0) s)
  = [(1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 100
  ( elements (remove (-10, 1) s)
  = [(3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 101
  ( elements (remove (-10, 2) s)
  = [(3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 102
  (elements (remove (-10, 9) s) = [(10, 10); (14, 15); (17, 18); (20, 21)])

;;
test 103 (elements (remove (-10, 20) s) = [(21, 21)])

;;
test 104 (elements (remove (-10, 21) s) = [])

;;
test 105 (elements (remove (-10, 28) s) = [])

;;
test 106 (elements (remove (-2, 28) s) = [])

;;
test 107 (elements (remove (-1, 28) s) = [])

;;
test 108 (elements (remove (0, 28) s) = [(-1, -1)])

;;
test 109 (elements (remove (1, 28) s) = [(-1, 0)])

;;
test 110 (elements (remove (2, 28) s) = [(-1, 1)])

;;
test 111 (elements (remove (3, 28) s) = [(-1, 1)])

;;
test 112 (elements (remove (4, 28) s) = [(-1, 1); (3, 3)])

;;
test 113 (elements (remove (5, 28) s) = [(-1, 1); (3, 4)])

;;
test 114 (elements (remove (6, 28) s) = [(-1, 1); (3, 4)])

;;
test 115 (elements (remove (10, 28) s) = [(-1, 1); (3, 4); (6, 7); (9, 9)])

;;
test 116
  ( elements (remove (20, 28) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18)] )

;;
test 117
  ( elements (remove (21, 28) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 20)] )

;;
test 118
  ( elements (remove (25, 28) s)
  = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 119
  ( elements (remove (1, 5) s)
  = [(-1, 0); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 120
  ( elements (remove (1, 6) s)
  = [(-1, 0); (7, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 121
  ( elements (remove (1, 7) s)
  = [(-1, 0); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 122
  ( elements (remove (2, 5) s)
  = [(-1, 1); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 123
  ( elements (remove (3, 5) s)
  = [(-1, 1); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 124
  ( elements (remove (3, 6) s)
  = [(-1, 1); (7, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 125
  ( elements (remove (3, 7) s)
  = [(-1, 1); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 126
  ( elements (remove (2, 6) s)
  = [(-1, 1); (7, 7); (9, 10); (14, 15); (17, 18); (20, 21)] )

;;
test 127
  ( elements (remove (2, 7) s)
  = [(-1, 1); (9, 10); (14, 15); (17, 18); (20, 21)] )

(* mem *)

;;
test 131 (mem (-10) s = false)

;;
test 132 (mem (-2) s = false)

;;
test 133 (mem (-1) s = true)

;;
test 134 (mem 0 s = true)

;;
test 135 (mem 1 s = true)

;;
test 136 (mem 2 s = false)

;;
test 137 (mem 10 s = true)

;;
test 138 (mem 11 s = false)

;;
test 139 (mem 13 s = false)

;;
test 140 (mem 14 s = true)

;;
test 141 (mem 21 s = true)

;;
test 142 (mem 22 s = false)

(* iter *)

;;
test 146
  (let l = ref [] in
   iter (fun x -> l := x :: !l) s ;
   List.rev !l = elements s)

;;
test 147
  (let t = ref true in
   iter (fun _ -> t := false) empty ;
   !t)

(* fold *)

;;
test 151 (fold (fun x l -> x :: l) s [] = List.rev (elements s))

;;
test 152 (fold (fun _ _ -> false) empty true)

(* below *)

;;
test 156 (below (-10) s = 0)

;;
test 157 (below (-1) s = 1)

;;
test 158 (below 0 s = 2)

;;
test 159 (below 1 s = 3)

;;
test 160 (below 2 s = 3)

;;
test 161 (below 6 s = 6)

;;
test 162 (below 7 s = 7)

;;
test 163 (below 8 s = 7)

;;
test 164 (below 9 s = 8)

;;
test 165 (below 20 s = 14)

;;
test 166 (below 21 s = 15)

;;
test 167 (below 22 s = 15)

;;
test 168 (below 50 s = 15)

(* split *)
let splituj x s =
  let s1, b, s2 = split x s in
  (elements s1, b, elements s2)

;;
test 172
  ( splituj (-10) s
  = ( []
    , false
    , [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] ) )

;;
test 173
  ( splituj (-1) s
  = ([], true, [(0, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)])
  )

;;
test 174
  ( splituj 0 s
  = ( [(-1, -1)]
    , true
    , [(1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)] ) )

;;
test 175
  ( splituj 1 s
  = ([(-1, 0)], true, [(3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)])
  )

;;
test 176
  ( splituj 2 s
  = ([(-1, 1)], false, [(3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)])
  )

;;
test 177
  ( splituj 9 s
  = ([(-1, 1); (3, 4); (6, 7)], true, [(10, 10); (14, 15); (17, 18); (20, 21)])
  )

;;
test 178
  ( splituj 10 s
  = ([(-1, 1); (3, 4); (6, 7); (9, 9)], true, [(14, 15); (17, 18); (20, 21)])
  )

;;
test 179
  ( splituj 11 s
  = ([(-1, 1); (3, 4); (6, 7); (9, 10)], false, [(14, 15); (17, 18); (20, 21)])
  )

;;
test 180
  ( splituj 19 s
  = ([(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18)], false, [(20, 21)])
  )

;;
test 181
  ( splituj 20 s
  = ([(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18)], true, [(21, 21)])
  )

;;
test 182
  ( splituj 21 s
  = ([(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 20)], true, [])
  )

;;
test 183
  ( splituj 22 s
  = ( [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)]
    , false
    , [] ) )

;;
test 184
  ( splituj 25 s
  = ( [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 21)]
    , false
    , [] ) )

(* testy na duuuuuże liczby *)
(* za duży przedział i całościowe below *)
let s = add (min_int + 5, max_int - 7) empty

;;
test 189 (below (max_int - 3) s = max_int)

(* pytanie o za duży kawałek *)

;;
test 192 (below 10000 s = max_int)

(* pytanie o mały kawałek *)

;;
test 195 (below (min_int + 20) s = 16)

(* dwa spore przedziały i całościowy below *)
let s = add (min_int + 5, -7) empty

let s = add (8, max_int - 5) s

;;
test 201 (below (max_int - 3) s = max_int)

(* spory kawałek + cały > max_int *)

;;
test 204 (below 10000 s = max_int)

(* mały kawałek + cały < max_int *)

;;
test 207 (below 16 s = max_int - 1)

(* testy na problemy z maxintami *)

;;
test 211
  ( elements (add (max_int, max_int) (add (max_int, max_int) empty))
  = [(max_int, max_int)] )

;;
test 213
  ( elements (add (max_int - 1, max_int) (add (max_int, max_int) empty))
  = [(max_int - 1, max_int)] )

;;
test 215
  ( elements (add (max_int, max_int) (add (max_int - 1, max_int) empty))
  = [(max_int - 1, max_int)] )

;;
test 217
  ( elements (add (max_int - 1, max_int) (add (max_int - 1, max_int) empty))
  = [(max_int - 1, max_int)] )

;;
test 219
  ( elements (add (max_int - 1, max_int - 1) (add (max_int, max_int) empty))
  = [(max_int - 1, max_int)] )

;;
test 221
  ( elements (add (max_int, max_int) (add (max_int - 1, max_int - 1) empty))
  = [(max_int - 1, max_int)] )

;;
test 223
  ( elements (add (max_int - 1, max_int - 1) (add (max_int - 1, max_int) empty))
  = [(max_int - 1, max_int)] )

;;
test 225
  ( elements (add (max_int - 1, max_int) (add (max_int - 1, max_int - 1) empty))
  = [(max_int - 1, max_int)] )

;;
test 228
  ( elements (add (min_int, min_int) (add (min_int, min_int) empty))
  = [(min_int, min_int)] )

;;
test 230
  ( elements (add (min_int, min_int + 1) (add (min_int, min_int) empty))
  = [(min_int, min_int + 1)] )

;;
test 232
  ( elements (add (min_int, min_int) (add (min_int, min_int + 1) empty))
  = [(min_int, min_int + 1)] )

;;
test 234
  ( elements (add (min_int, min_int + 1) (add (min_int, min_int + 1) empty))
  = [(min_int, min_int + 1)] )

;;
test 236
  ( elements (add (min_int + 1, min_int + 1) (add (min_int, min_int) empty))
  = [(min_int, min_int + 1)] )

;;
test 238
  ( elements (add (min_int, min_int) (add (min_int + 1, min_int + 1) empty))
  = [(min_int, min_int + 1)] )

;;
test 240
  ( elements (add (min_int + 1, min_int + 1) (add (min_int, min_int + 1) empty))
  = [(min_int, min_int + 1)] )

;;
test 242
  ( elements (add (min_int, min_int + 1) (add (min_int + 1, min_int + 1) empty))
  = [(min_int, min_int + 1)] )

(* od konca do konca *)

;;
test 246 (below max_int (add (min_int, max_int) empty) = max_int)

(* od srodka do konca *)

;;
test 249 (below 0 (add (min_int, max_int) empty) = max_int)

;;
test 250 (below 10 (add (min_int, max_int) empty) = max_int)

(* dwa duże nachodzące *)

;;
test 253
  ( elements (add (min_int, 12) (add (10, max_int) empty))
  = [(min_int, max_int)] )

(* dwa duże ledwo nachodzące *)

;;
test 256
  (elements (add (0, max_int) (add (min_int, 0) empty)) = [(min_int, max_int)])

(* dwa duże sąsiadujące *)

;;
test 259
  (elements (add (0, max_int) (add (min_int, -1) empty)) = [(min_int, max_int)])

(* dwa duże rozłączne *)

;;
test 262
  ( elements (add (min_int, -12) (add (10, max_int) empty))
  = [(min_int, -12); (10, max_int)] )

(* testy brzegowe na max_int *)

;;
test 266 (below max_int (add (0, max_int) empty) = max_int)

;;
test 267 (below max_int (add (1, max_int) empty) = max_int)

;;
test 268 (below max_int (add (2, max_int) empty) = max_int - 1)

;;
test 269 (below max_int (add (-1, max_int - 1) empty) = max_int)

;;
test 270 (below max_int (add (0, max_int - 1) empty) = max_int)

;;
test 271 (below max_int (add (1, max_int - 1) empty) = max_int - 1)

;;
test 273 (below (max_int - 3) (add (-3, max_int - 3) empty) = max_int)

;;
test 274 (below (max_int - 3) (add (-2, max_int - 3) empty) = max_int)

;;
test 275 (below (max_int - 3) (add (-1, max_int - 3) empty) = max_int - 1)

;;
test 276 (below (max_int - 3) (add (-4, max_int - 4) empty) = max_int)

;;
test 277 (below (max_int - 3) (add (-3, max_int - 4) empty) = max_int)

;;
test 278 (below (max_int - 3) (add (-2, max_int - 4) empty) = max_int - 1)

(* max_inty w dużych kawałkach *)

;;
test 281
  ( below max_int (add (min_int, -2) (add (0, 0) (add (2, max_int - 10) empty)))
  = max_int )

;;
test 282
  ( below max_int (add (min_int, -2) (add (2, max_int - 10) (add (0, 0) empty)))
  = max_int )

;;
test 283
  ( below max_int (add (0, 0) (add (min_int, -2) (add (2, max_int - 10) empty)))
  = max_int )

;;
test 284
  ( below max_int (add (0, 0) (add (2, max_int - 10) (add (min_int, -2) empty)))
  = max_int )

;;
test 285
  ( below max_int (add (2, max_int - 10) (add (min_int, -2) (add (0, 0) empty)))
  = max_int )

;;
test 286
  ( below max_int (add (2, max_int - 10) (add (0, 0) (add (min_int, -2) empty)))
  = max_int )

let _ = if !zle = 0 then Printf.printf "Testy poprawnosci OK!\n\n"

(* testy wydajnosciowe *)

;;
Printf.printf "n' ≈ n/500,  t' ~ t/(n·log n)\n\n"

let dodawaj minim maxim start step s =
  let rec dod a s =
    if minim <= a && a <= maxim then
      dod (a + (2 * step)) (add (min a (a + step), max a (a + step)) s)
    else s
  in
  dod start s

(* Printf.printf "rrr1: %d rrr2: %d rrr3: %d rrr4: %d
" (!rrr1) (!rrr2) (!rrr3) (!rrr4);; *)
(* rrr1: 0 rrr2: 0 rrr3: 5 rrr4: 0 *)

let duzedrzewo opis zrob n =
  let start = Sys.time () in
  let z = zrob n in
  let czas = Sys.time () -. start in
  let n' = n / 1000 in
  let f = float_of_int n in
  let t' = czas *. 100000000. /. (f *. log f) in
  Printf.printf "Budowanie (%s): n'=%5d, t=%2.3f t'=%f\n%!" opis n' czas t' ;
  z

let rosnace n = dodawaj 0 n 0 5 empty

let z = duzedrzewo "rosnące" rosnace 625000

let z = duzedrzewo "rosnące" rosnace 1250000

let z = duzedrzewo "rosnące" rosnace 2500000

let z = duzedrzewo "rosnące" rosnace 5000000

let z = duzedrzewo "rosnące" rosnace 10000000

let start = Sys.time ()

;;
for i = 1 to 10 do
  test 330 (below 100000000 z = 6000006) ;
  test 331 (below 10000000 z = 6000001) ;
  test 332 (below 8000000 z = 4800001) ;
  test 333 (below 6000000 z = 3600001) ;
  test 334 (below 4000000 z = 2400001) ;
  test 335 (below 2000000 z = 1200001) ;
  test 336 (below 1000000 z = 600001) ;
  test 337 (below 100000 z = 60001) ;
  test 338 (below 10000 z = 6001) ;
  test 339 (below 1000 z = 601) ;
  test 340 (below 100 z = 61) ;
  test 341 (below 10 z = 7) ;
  test 342 (below 1 z = 2) ;
  test 343 (below (-10) z = 0)
done

;;
Printf.printf "Below  (rosnace): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 349 (List.length (elements (remove (100, 10000000 - 100) z)) = 21)

;;
Printf.printf "Remove (rosnace): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 353 (List.length (elements (add (100, 10000000 - 100) z)) = 21)

;;
Printf.printf "Add    (rosnace): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 357 (fold (fun _ i -> i + 1) z 0 = 1000001)

;;
Printf.printf "Fold   (rosnace): %f\n%!" (Sys.time () -. start)

(* Printf.printf "rrr1: %d rrr2: %d rrr3: %d rrr4: %d
" (!rrr1) (!rrr2) (!rrr3) (!rrr4);; *)
(* rrr1: 0 rrr2: 0 rrr3: 49988 rrr4: 0 *)

let skos_lewy n =
  let z = add (0, 0) (add (max_int, max_int) empty) in
  let z = dodawaj 0 n 5 5 z in
  z

let z = duzedrzewo "skos_lewy" skos_lewy 625000

let z = duzedrzewo "skos_lewy" skos_lewy 1250000

let z = duzedrzewo "skos_lewy" skos_lewy 2500000

let z = duzedrzewo "skos_lewy" skos_lewy 5000000

let z = duzedrzewo "skos_lewy" skos_lewy 10000000

let start = Sys.time ()

;;
for i = 1 to 10 do
  test 377 (below 10000000 z = 6000001) ;
  test 378 (below 8000000 z = 4800001) ;
  test 379 (below 6000000 z = 3600001) ;
  test 380 (below 4000000 z = 2400001) ;
  test 381 (below 2000000 z = 1200001) ;
  test 382 (below 1000000 z = 600001) ;
  test 383 (below 100000 z = 60001) ;
  test 384 (below 10000 z = 6001) ;
  test 385 (below 1000 z = 601) ;
  test 386 (below 100 z = 61) ;
  test 387 (below 10 z = 7) ;
  test 388 (below 1 z = 1) ;
  test 389 (below (-10) z = 0)
done

;;
Printf.printf "Below  (skos_lewy): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 395 (List.length (elements (remove (100, 10000000 - 100) z)) = 22)

;;
Printf.printf "Remove (skos_lewy): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 399 (List.length (elements (add (100, 10000000 - 100) z)) = 22)

;;
Printf.printf "Add    (skos_lewy): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 403 (fold (fun _ i -> i + 1) z 0 = 1000002)

;;
Printf.printf "Fold   (skos_lewy): %f\n%!" (Sys.time () -. start)

(* Printf.printf "rrr1: %d rrr2: %d rrr3: %d rrr4: %d
" (!rrr1) (!rrr2) (!rrr3) (!rrr4);; *)
(* rrr1: 191722 rrr2: 8268 rrr3: 99885 rrr4: 0 *)

let skos_prawy n =
  let z = add (max_int, max_int) (add (0, 0) empty) in
  let z = dodawaj 100 n n (-5) z in
  z

let z = duzedrzewo "skos_prawy" skos_prawy 625000

let z = duzedrzewo "skos_prawy" skos_prawy 1250000

let z = duzedrzewo "skos_prawy" skos_prawy 2500000

let z = duzedrzewo "skos_prawy" skos_prawy 5000000

let z = duzedrzewo "skos_prawy" skos_prawy 10000000

let start = Sys.time ()

;;
for i = 1 to 10 do
  test 423 (below 10000000 z = 5999947) ;
  test 424 (below 8000000 z = 4799947) ;
  test 425 (below 6000000 z = 3599947) ;
  test 426 (below 4000000 z = 2399947) ;
  test 427 (below 2000000 z = 1199947) ;
  test 428 (below 1000000 z = 599947) ;
  test 429 (below 100000 z = 59947) ;
  test 430 (below 10000 z = 5947) ;
  test 431 (below 1000 z = 547) ;
  test 432 (below 100 z = 7) ;
  test 433 (below 10 z = 1) ;
  test 434 (below 1 z = 1) ;
  test 435 (below (-10) z = 0)
done

;;
Printf.printf "Below  (skos_prawy): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 441 (below max_int (remove (2, 10000000 - 1) z) = 3)

;;
Printf.printf "Remove (skos_prawy): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 445 (below max_int (add (2, 10000000 - 1) z) = 10000001)

;;
Printf.printf "Add    (skos_prawy): %f\n%!" (Sys.time () -. start)

let start = Sys.time ()

;;
test 449 (fold (fun _ i -> i + 1) z 0 = 999993)

;;
Printf.printf "Fold   (skos_prawy): %f\n%!" (Sys.time () -. start)

let masakra n =
  let z = add (0, 0) (add (max_int, max_int) empty) in
  (* dodajemy przedziały [995,1000] [985,990] itd *)
  let z = dodawaj 0 n n (-5) z in
  (* a potem [0,37] [74,111] itd *)
  let z = dodawaj 0 n 0 37 z in
  (* to wymusza sporo łączeń przedziałów... *)
  z

let z = duzedrzewo "masakra" masakra 625000

let z = duzedrzewo "masakra" masakra 1250000

let z = duzedrzewo "masakra" masakra 2500000

let z = duzedrzewo "masakra" masakra 5000000

let z = duzedrzewo "masakra" masakra 10000000

let _ =
  if !zle = 0 then Printf.printf "\nTesty OK!\n"
  else Printf.printf "\nBlednych testow: %d...\n" !zle

(* Printf.printf "rrr1: %d rrr2: %d rrr3: %d rrr4: %d
" (!rrr1) (!rrr2) (!rrr3) (!rrr4);; *)
(* rrr1: 258234 rrr2: 8268 rrr3: 399763 rrr4: 57 *)
