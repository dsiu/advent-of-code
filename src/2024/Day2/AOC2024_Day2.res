open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

let parse = data =>
  data
  ->splitNewline
  ->Array.map(String.trim)
  ->Array.map(x => x->String.split(" ")->Array.filterMap(Int.fromString(_)))

let countInc = xs => xs->Array.filter(x => x > 0)->Array.length
let isInc = xs => xs->countInc == xs->Array.length

let countDec = xs => xs->Array.filter(x => x < 0)->Array.length
let isDec = xs => xs->countDec == xs->Array.length

let countDiffMinOne = xs => xs->Array.filter(x => Math.Int.abs(x) >= 1)->Array.length
let isDiffMinOne = xs => xs->countDiffMinOne == xs->Array.length

let countDiffMaxThree = xs => xs->Array.filter(x => Math.Int.abs(x) <= 3)->Array.length
let isDiffMaxThree = xs => xs->countDiffMaxThree == xs->Array.length

let isSafe = xs => {
  let a = xs->Array.slice(~start=0, ~end=xs->Array.length - 1)
  let b = xs->Array.sliceToEnd(~start=1)
  let diff = Array.zipWith(a, b, (x, y) => y - x)
  let safe = (diff->isInc || diff->isDec) && diff->isDiffMinOne && diff->isDiffMaxThree
  safe ? Some(xs) : None
}

let part1 = reports => {
  reports->Array.filterMap(isSafe)->Array.length
}

let part2 = reports => {
  reports
  ->Array.filterMap(r => {
    //    r->(log2("r=", _))
    r->isSafe->Option.isSome
      ? Some(r)
      : {
          let subReports = r->Array.reduceWithIndex([], (acc, _, i) => {
            acc->Array.push(r->Array.toSpliced(~start=i, ~remove=1, ~insert=[]))
            acc
          })

          subReports->Array.find(r => r->isSafe->Option.isSome)
        }
  })
  ->Array.length
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
