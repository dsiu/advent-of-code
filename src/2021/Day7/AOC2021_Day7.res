open Belt
open Utils
let log = Js.Console.log

let median = xs => {
  let sorted = xs->SortArray.Int.stableSort
  let half = xs->Array.length / 2
  (mod(xs->Array.length, 2)==0) ? 
    sorted->Array.getExn(half)
  :
    (sorted->Array.getExn(half - 1) + sorted->Array.getExn(half)) / 2
}

let distance = (xs, m) => {
  xs->Array.reduce(0, (a,x)=> a+ Js.Math.abs_int(x-m))
}


let parse = data => data->Js.String2.trim->Js.String2.split(",")->Array.map(x=>x->Int.fromString->Option.getExn)

let solvePart1 = data => {
  let xs = data->parse
  let m = xs->median
  xs->distance(m)
}

let solvePart2 = data => {
  data->ignore
  2
}
