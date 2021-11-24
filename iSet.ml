(*
    Autor: Florian Ficek
    Code review: Jan Szot
*)


(* zdefiniowany przedział *)
type przedzial = int*int

(* zdefiniowane drzewo jako 
    (
    lewe poddrzewo; 
    przedział;
    prawe poddrzewo;
    wysokośc;
    suma elementów w poddrzewie i w wierzchołku;
    ) *)
type t =
    |Empty
    |Node of t * przedzial * t * int * int
;;

(* #### FUNKCJE POMOCNICZE #### *)

(* funkcja do sprawdzania czy zbiory będą się łączyć:
    0 gdy nachodzą na siebie
    -1,1 gdy sąsiadują ze sobą
    -2,2 gdy są odległe
 *)
let rec cmp x y =
    if fst x > fst y then -cmp y x
    else if snd x >= fst y then 0
    else if snd x = (fst y)-1 then -1
    else -2
;;

(* spradzenie czy x należy do zbioru (a,b) *)
let in_range x ((a,b):przedzial) =
    if x>=a && x<=b then
        true
    else
        false
;;

(* otrzymywanie suma elementów w poddrzewie i w wierzchołku *)
let get_sum t = 
    match t with
        |Empty -> 0
        |Node(_,_,_,_,s) -> s
;;

(* otrzymywanie wysokości drzewa *) (* z pliku pSet.ml *)
let height = function
    |Node (_, _, _, h, _) -> h
    |Empty -> 0
;;

(* tworzenie drzewa *) (* z pliku pSet.ml *)
let make l (k:przedzial) r = Node (l, k, r, max (height l) (height r) + 1, (snd k - fst k + 1 + (get_sum l) + (get_sum r) ))

(* funkcja balansująca *) (* z pliku pSet.ml *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
        |Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
            |Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
            |Empty -> assert false)
        |Empty -> assert false
  else if hr > hl + 2 then
    match r with
        |Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
            |Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
            |Empty -> assert false)
        |Empty -> assert false
  else make l k r
;;

(* z pliku pSet.ml *)
let rec add_min_element k = function
        |Empty -> make Empty k Empty
        |Node (l, x, r, _, _) ->
            bal (add_min_element k l) x r
;;

(* z pliku pSet.ml *)
let rec add_max_element k = function
        |Empty -> make Empty k Empty
        |Node (l, x, r, _, _) ->
            bal l x (add_max_element k r)
;;

(* z pliku pSet.ml *)
let rec min_elt = function
    |Node (Empty, k, _, _, _) -> k
    |Node (l, _, _, _, _) -> min_elt l
    |Empty -> raise Not_found
;;

(* z pliku pSet.ml *)
let rec remove_min_elt = function
    |Node (Empty, _, r, _, _) -> r
    |Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
    |Empty -> invalid_arg "PSet.remove_min_elt"
;;

(* z pliku pSet.ml *)
let rec join l v r =
    match (l, r) with
    (Empty, _) -> add_min_element v r
    |(_, Empty) -> add_max_element v l
    |(Node(ll, lv, lr, lh,_), Node(rl, rv, rr, rh,_)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
        make l v r
;;

(* z pliku pSet.ml *) (* mergowanie dwóch drzew *)
let concat t1 t2 =
    match (t1, t2) with
        |(Empty, t) -> t
        |(t, Empty) -> t
        |(_, _) -> join t1 (min_elt t2) (remove_min_elt t2)
;;

(* 
    funkcja pomocnicza do sumowania przedziałów w drzewie
    wykonywana najpierw dla mniejsze liczby z wstawianego przedziału,
    a potem dla większej
 *)
let split2 x t dg= (* do funkcji add i remove*)
    let rec loop x t =
    match t with
        |Empty -> (Empty, x, Empty)
        |Node(l, k, r, _, _) ->
            let path = cmp x k in 
            if in_range path dg then (l,k,r)
            else if path < 0 then
                let (ll, pres, rl) = loop x l in (ll, pres, join rl k r)
            else
                let (lr, pres, rr) = loop x r in (join l k lr, pres, rr)
    in loop x t
;;

(* FUNKCJE PROGRAMU *)

let empty = Empty;;

let is_empty t = t = Empty;; 

(* dodawanie przedziału do drzewa gdzie nie łączy się ten przedział *)
let rec add_one ((x,y):przedzial) t = 
    match t with
        |Node (l, k, r, h, _) ->
            if fst k = x && snd k = y then make l (x,y) r
            else if y <= fst k then
                let nl = add_one (x,y) l in
                bal nl k r
            else
            let nr = add_one (x,y) r in
            bal l k nr
        |Empty -> make Empty (x,y) Empty
;;

(* dodawanie przedziału do drzewa, najpierw tworzone jest nowe drzewo gdzie można bez problemowo dodać nowy przedział *)
let add ((x,y):przedzial) t = 
    let (l,(a,_),_) = split2 (x,x) t (0,1) and (_,(_,b),r) = split2 (y,y) t (-1,0) in
    add_one (a,b) (concat l r)
;;


let rec split x t = 
    match t with
        |Empty -> (Empty, false, Empty)
        |Node(l, k, r, h, _) ->
            if in_range x k then 
                if fst k = x && snd k = x then (l, true, r)
                else if fst k = x then (l, true, (make Empty (x+1, snd k) r))
                else if snd k = x then ((make l (fst k, x-1) Empty), true, r)
                else ((make l (fst k, x-1) Empty), true, (make Empty (x+1, snd k) r))
            else if x <= fst k then
                let (ll, pres, rl) = split x l in (ll, pres, join rl k r)
            else
                let (lr, pres, rr) = split x r in (join l k lr, pres, rr)
;;

(* z pliku pSet.ml *)
let remove ((x,y):przedzial) t = let (l,_,_) = split x t and (_,_,r) = split y t in
    concat l r;;

(* z pliku pSet.ml *)
let mem x t =
  let rec helper t = 
    match t with
        |Node (l, k, r, _, _) ->
            if in_range x k then
            true
            else
            (helper l) || (helper r)
        |Empty -> false 
  in helper t
;;

(* z pliku pSet.ml *)
let iter f t =
  let rec loop = function
    |Empty -> ()
    |Node (l, k, r, _, _) -> loop l; f k; loop r 
  in loop t
;;

(* z pliku pSet.ml *)
let fold f t acc =
  let rec loop acc = function
    |Empty -> acc
    |Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r 
  in loop acc t
;;

(* z pliku pSet.ml *)
let elements t = 
  let rec loop acc = function
    Empty -> acc
    |Node(l, k, r, _, _) -> loop (k :: loop acc r) l 
  in loop [] t
;;

(* funkcja sprawdzająca ile elementów drzewa jest mniejsze od danej liczby *)
let below x t = 
    let rec loop t acc =
        match t with
            |Empty -> if acc < 0 then max_int else acc
            |Node(l,k,r,_,s) ->
            if x < fst k then loop l acc
            else if x > snd k then loop r (acc + (get_sum l) + snd k - fst k +1)
            else let check = acc + (get_sum l) + x - fst k +1 in
            if check <=0 then max_int else check
    in loop t 0
;;
