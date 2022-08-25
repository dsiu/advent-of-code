open Belt
open Utils
let log = Js.Console.log

let findDiff = xs => {
  let (_, result) =
    xs
    ->Array.sliceToEnd(1)
    ->Array.reduceU((xs->Array.get(0)->Option.getExn, []), (. (last, result), x) => {
      (x, Array.concat(result, [x - last]))
    })

  result->Array.keepU((. x) => x > 0)
}

// use zip to find the differences between the two arrays
let findDiff2 = xs => {
  Array.zip(xs->Array.sliceToEnd(1), xs)->Array.keep(((a, b)) => a > b)
}

let sum2Array = (a1, a2) => FP_Utils.combinationArray2(a1, a2, (a, b) => a + b)

let roll3sum = xs => {
  let size = xs->Array.length - 2
  let a1 = xs->Array.slice(~offset=0, ~len=size)
  let a2 = xs->Array.slice(~offset=1, ~len=size)
  let a3 = xs->Array.slice(~offset=2, ~len=size)

  a1->sum2Array(a2)->sum2Array(a3)
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->intFromStringExn
  })

let solvePart1 = data => {
  data->parse->findDiff2->Array.length
}

let solvePart2 = data => {
  data->parse->roll3sum->findDiff2->Array.length
}
