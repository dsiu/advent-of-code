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

  result
}

let sum2Array = (a1, a2) =>
  Array.reduceReverse2(a1->Array.reverse, a2->Array.reverse, [], (acc, x, y) =>
    Array.concat(acc, [x + y])
  )

let roll3sum = xs => {
  let size = xs->Array.length - 2
  let a1 = xs->Array.slice(~offset=0, ~len=size)
  let a2 = xs->Array.slice(~offset=1, ~len=size)
  let a3 = xs->Array.slice(~offset=2, ~len=size)

  sum2Array(a1, a2)->sum2Array(a3)
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim->Int.fromString->Option.getExn
  })

let solvePart1 = data => {
  data->parse->findDiff->Array.keepU((. x) => x > 0)->Array.length
}

let solvePart2 = data => {
  data->parse->roll3sum->findDiff->Array.keepU((. x) => x > 0)->Array.length
}
