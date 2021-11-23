(*
    Autor: Florian Ficek
    Code review: 
*)

type przedzial = int*int

type t =
    |Empty
    |Node of t * przedzial * t * int
;;

(* FUNKCJE POMOCNICZE *)

let rec fold_tree f a t = 
    match t with
        |Empty -> a
        |Node (l, k, r,_) -> f k (fold_tree f a l) (fold_tree f a r);;

let in_przedzial x ((a,b):przedzial) =
    if x>=a && x<=b then
        true
    else
        false
;;

let height = function
    |Node (_, _, _, h) -> h
    |Empty -> 0
;;

let make l (k:przedzial) r = Node (l, k, r, max (height l) (height r) + 1)

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
        |Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
            |Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
            |Empty -> assert false)
        |Empty -> assert false
  else if hr > hl + 2 then
    match r with
        |Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
            |Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
            |Empty -> assert false)
        |Empty -> assert false
  else Node (l, k, r, max hl hr + 1)
;;

let rec add_min_element k = function
        |Empty -> Node(Empty, k, Empty, 1)
        |Node (l, x, r, h) ->
            bal (add_min_element k l) x r
;;

let rec add_max_element k = function
        |Empty -> Node(Empty, k, Empty, 1)
        |Node (l, x, r, h) ->
            bal l x (add_max_element k r)
;;

let rec min_elt = function
    |Node (Empty, k, _, _) -> k
    |Node (l, _, _, _) -> min_elt l
    |Empty -> raise Not_found
;;

let rec remove_min_elt = function
    |Node (Empty, _, r, _) -> r
    |Node (l, k, r, _) -> bal (remove_min_elt l) k r
    |Empty -> invalid_arg "PSet.remove_min_elt"
;;

let rec join l v r =
    match (l, r) with
    (Empty, _) -> add_min_element v r
    |(_, Empty) -> add_max_element v l
    |(Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
        make l v r
;;

let concat t1 t2 =
    match (t1, t2) with
        |(Empty, t) -> t
        |(t, Empty) -> t
        |(_, _) -> join t1 (min_elt t2) (remove_min_elt t2)
;;

let rec split2 x t = (* do funkcji add *)
    match t with
        |Empty -> (Empty, (x,x), Empty)
        |Node(l, k, r, h) ->
            if in_przedzial x k then 
                if fst k = x && snd k = x then (l, k, r)
                else if fst k = x || fst k = x+1 || fst k = x-1 then (l, k, (Node(Empty, k, r, h)))
                else if snd k = x || snd k = x-1 || snd k = x+1 then ((Node(l, k, Empty, h)), k, r)
                else ((Node(l, k, Empty, h)), k, (Node(Empty, k, r, h)))
            else if x <= fst k then
                let (ll, prev, rl) = split2 x l in (ll, prev, join rl k r)
            else
                let (lr, prev, rr) = split2 x r in (join l k lr, prev, rr)
;;

(* FUNKCJE PROGRAMU *)

let empty = Empty;;

let is_empty t = t = Empty;; 

let rec add_one ((x,y):przedzial) t = 
    match t with
        |Node (l, k, r, h) ->
            if fst k = x && snd k = y then Node (l, (x,y), r, h)
            else if y <= fst k then
                let nl = add_one (x,y) l in
                bal nl k r
            else
            let nr = add_one (x,y) r in
            bal l k nr
        |Empty -> Node (Empty, (x,y), Empty, 1)
;;

let add ((x,y):przedzial) t = 
    let (l,(a,_),_) = split2 x t and (_,(_,b),r) = split2 y t in
    add_one (a,b) (concat l r) 
;;

let remove ((x,y):przedzial) t = t;;

let rec split x t = 
    match t with
        |Empty -> (Empty, false, Empty)
        |Node(l, k, r, h) ->
            if in_przedzial x k then 
                if fst k = x && snd k = x then (l, true, r)
                else if fst k = x then (l, true, (Node(Empty, ((fst k)+1, snd k), r, h)))
                else if snd k = x then ((Node(l, (fst k, (snd k)-1), Empty, h)), true, r)
                else ((Node(l,(fst k,(snd k)-1),Empty,h)), true, (Node(Empty,((fst k)+1,snd k),r,h)))
            else if x <= fst k then
                let (ll, pres, rl) = split x l in (ll, pres, join rl k r)
            else
                let (lr, pres, rr) = split x r in (join l k lr, pres, rr)
;;

let mem x t =
  let rec helper t = 
    match t with
        |Node (l, k, r, _) ->
            if in_przedzial x k then
            true
            else
            (helper l) || (helper r)
        |Empty -> false 
  in helper t
;;

let iter f t =
  let rec loop = function
    |Empty -> ()
    |Node (l, k, r, _) -> loop l; f k; loop r 
  in loop t
;;

let fold f t acc =
  let rec loop acc = function
    |Empty -> acc
    |Node (l, k, r, _) ->
          loop (f k (loop acc l)) r 
  in loop acc t
;;

let elements t = 
  let rec loop acc = function
    Empty -> acc
    |Node(l, k, r, _) -> loop (k :: loop acc r) l 
  in loop [] t
;;

let below x t = 1;;