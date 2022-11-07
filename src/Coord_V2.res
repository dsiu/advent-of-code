module TC = Tablecloth

open TC
type t = Tuple2.t<int, int>
let compare = Tuple2.compare(~f=TC.Int.compare, ~g=TC.Int.compare)

include Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b), (a', b')) => (a + a', b + b')
let mul = ((a, b), x) => (a * x, b * x)
