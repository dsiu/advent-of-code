open Belt
open Utils

let log = Js.Console.log
let log2 = Js.Console.log2

let diffs = (xs, ys) => {
  xs->Array.mapWithIndex((i, x) => {
    ys->Array.getExn(i) - x
  })
}

let tally = xs => {
  let max = xs->maxIntInArray
  let min = xs->minIntInArray
  let keyRange = Array.range(min, max)

  let result = MutableMap.Int.make()

  keyRange->Array.forEach(k => {
    let (p, _) = xs->Array.partition(x => x == k)
    let n = p->Array.length
    n > 0 ? result->MutableMap.Int.set(k, n) : ()
  })

  result
}

let groupOnes = xs => {
  let rec helper = (arr, res) => {
    let threeIdx = arr->Array.getIndexBy(x => x === 3)
    switch threeIdx {
    | Some(i) => {
        let res = Array.concat(res, [i])
        i + 1 <= arr->Array.length ? arr->Array.sliceToEnd(i + 1)->helper(res) : res
      }
    | None => res
    }
  }

  helper(xs, [])->Array.keep(x => x > 0)
}

// 2 for (1,1) combination(1,0) + combination(1,1) = 2
// 3 for (1,1,1) combination(2,0) + combination(2,1) + combination(2,2) = 4
// 4 for (1,1,1,1) combination(3,1) + combination(3,2) + combination(3,3) = 7
// 5 for (1,1,1,1,1) combination(4,2) + combination(4,3) + combination(4,4) = 11
let convertToMultiplier = xs => {
  xs->Array.map(x => {
    switch x {
    | 2 => 2
    | 3 => 4
    | 4 => 7
    | 5 => 11
    | _ => x
    }
  })
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim->Int.fromString->Option.getExn
  })

let solvePart1 = data => {
  let adapters = data->parse->SortArray.Int.stableSort
  //  adapters->log
  let a = [0]->Array.concat(adapters)
  let b = adapters->Array.concat([adapters->Utils.maxIntInArray + 3])

  let diffs = diffs(a, b)
  //  diffs->log
  let tally = diffs->tally
  //  tally->MutableMap.Int.toArray->log
  tally->MutableMap.Int.getExn(1) * tally->MutableMap.Int.getExn(3)
}

// 2 for (1,1) combination(1,0) + combination(1,1) = 2
// 3 for (1,1,1) combination(2,0) + combination(2,1) = 4+ combination(2,2)
// 4 for (1,1,1,1) combination(3,1) + combination(3,2) + combination(3,3) = 7
// 5 for (1,1,1,1,1) combination(4,2) + combination(4,3) + combination(4,4) = 11

let solvePart2 = data => {
  let adapters = data->parse->SortArray.Int.stableSort
  //  log2("adapters", adapters)
  let a = [0]->Array.concat(adapters)
  let b = adapters->Array.concat([adapters->Utils.maxIntInArray + 3])

  let diffs = diffs(a, b)
  //  log2("diffs", diffs)
  let ones = diffs->groupOnes
  //  log2("ones", ones)
  let multiplers = ones->convertToMultiplier
  //  log2("multiplers", multiplers)

  open! Belt.Float
  let result = multiplers->Array.reduce(1.0, (acc, x) => x->Belt.Int.toFloat * acc)
  result
}
