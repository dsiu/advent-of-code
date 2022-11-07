module TC = Tablecloth

open TC
type t = TableclothTuple4.t<int, int, int, int>
let compare = TableclothTuple4.compare(
  ~f=TC.Int.compare,
  ~g=TC.Int.compare,
  ~h=TC.Int.compare,
  ~i=TC.Int.compare,
)

include Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b, c, d), (a', b', c', d')) => (a + a', b + b', c + c', d + d')
