open Belt
open Utils
let log = Utils.log

module Xmax = {
  type t = {
    codes: array<int>,
    runLength: int,
  }
  let codes = t => t.codes
  let runLength = t => t.runLength

  // is x sum of 2 elem in xs
  let isSumOf = (xs, x) => true

  // is code at index i (0-base) a valid code
  let isCodeValid = (t, i) => {
    let lastSet = t->codes->Array.slice(~offset=i - t->runLength, ~len=t->runLength)
    let code = t.codes->Array.getExn(i)
    code->log

    lastSet->log

    isSumOf(lastSet, code)
  }

  let make = (codes, runLength) => {codes: codes, runLength: runLength}
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim->Int.fromString->Option.getExn
  })

let solvePart1 = data => {
  let xmax = Xmax.make(data->parse, 5)
  xmax->log
  xmax->Xmax.isCodeValid(5)
}

let solvePart2 = data => {
  data->ignore
  2
}
