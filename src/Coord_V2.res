open Stdlib

module TC = Tablecloth

// todo: make it works with types other than int
type t = Tuple2.t<int, int>

let intCompare = (a, b) => Int.compare(a, b)->Ordering.toInt
let compare = (a, b) => Tuple2.compare(~f=intCompare, ~g=intCompare, a, b)

include TC.Comparator.Make({
  type t = t
  let compare = compare
})

let add = ((a, b), (a', b')) => (a + a', b + b')
let sub = ((a, b), (a', b')) => (a - a', b - b')
let mul = ((a, b), x) => (a * x, b * x)

// serialization
let toString: t => string = t =>
  t->Tuple2.toArray->Array.map(JSON.Encode.int)->JSON.Encode.array->JSON.stringify

let fromString: string => option<t> = str =>
  str
  ->JSON.parseExn
  ->JSON.Decode.array
  ->Option.map(arr =>
    arr->Array.map(num =>
      num->JSON.Decode.float->Option.flatMap(x => x->Float.toInt->Some)->Option.getUnsafe
    )
  )
  ->Option.flatMap(Tuple2.fromArray)

let show = toString
