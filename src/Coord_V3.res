module TC = Tablecloth

open TC
type t = Tuple3.t<int, int, int>
let compare = Tuple3.compare(~f=TC.Int.compare, ~g=TC.Int.compare, ~h=TC.Int.compare)

include Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b, c), (a', b', c')) => (a + a', b + b', c + c')
let mul = ((a, b, c), x) => (a * x, b * x, c * x)
