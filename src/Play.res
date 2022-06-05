open Belt
let a = [1, 2, 3]
let f = xs => Array.concat(xs, [10])
let b = f(a)
a->Js.log
b->Js.log
