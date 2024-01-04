open Belt
open Array2D
let log = Js.Console.log
let a = Array2D.make((0, 0), 0)
let b = Array2D.make((0, 0), 0)

let eq1 = (t, u) => {
  t->lengthX === u->lengthX &&
  t->lengthY === u->lengthY &&
  Array.reduceReverse2(t, u, true, (c, a, b) => {
    Js.Console.log("================== diu")
    c && Array.eq(a, b, (a, b) => {a === b})
  })
}
eq1(a, b)->log
