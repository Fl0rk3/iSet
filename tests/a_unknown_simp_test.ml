open ISet
let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
let c = remove (2, 10) a;;
assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
assert(below 12 c = 5);;
