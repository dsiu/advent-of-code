open Stdlib

module TC = Tablecloth

type t = Tuple3.t<int, int, int>

let intCompare = (a, b) => Int.compare(a, b)->Ordering.toInt
let compare = Tuple3.compare(~f=intCompare, ~g=intCompare, ~h=intCompare)

include TC.Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b, c), (a', b', c')) => (a + a', b + b', c + c')
let mul = ((a, b, c), x) => (a * x, b * x, c * x)
