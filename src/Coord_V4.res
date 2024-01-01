module TC = Tablecloth

open TC
type t = TableclothTuple4.t<int, int, int, int>

let intCompare = (a, b) => Int.compare(a, b)->Stdlib.Ordering.toInt
let compare = TableclothTuple4.compare(~f=intCompare, ~g=intCompare, ~h=intCompare, ~i=intCompare)

include Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b, c, d), (a', b', c', d')) => (a + a', b + b', c + c', d + d')
let mul = ((a, b, c, d), x) => (a * x, b * x, c * x, d * x)
