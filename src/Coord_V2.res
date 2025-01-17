open Stdlib

module TC = Tablecloth

type t = Tuple2.t<int, int>

let intCompare = (a, b) => Int.compare(a, b)->Ordering.toInt
let compare = (a, b) => Tuple2.compare(~f=intCompare, ~g=intCompare, a, b)

include TC.Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b), (a', b')) => (a + a', b + b')
let mul = ((a, b), x) => (a * x, b * x)

let show = ((a, b)) => `(${a->Int.toString}, ${b->Int.toString})`
