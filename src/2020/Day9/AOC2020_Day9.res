open Belt
open Utils
let log = Utils.log

module Xmax = {
  type t = {
    codes: array<int>,
    runLength: int,
  }

  type xmaxError = InvalidRunLength | InvalidIndex

  let codes = t => t.codes
  let runLength = t => t.runLength

  // is sum sum of x with one of xs
  // if so, return array of matches
  let isSumofWith = (xs, x, ~sum) => {
    xs->Array.keep(a => {
      a != x && a + x == sum
    })
  }

  // is sum sum of 2 elem in xs
  let isSumOf = (xs, sum) => {
    xs->Array.keep(a => {
      xs->isSumofWith(a, ~sum)->Array.size > 0
    })
  }

  // is code at index i (0-base) a valid code
  let isCodeValid = (t, i) => {
    let lastSet = t->codes->Array.slice(~offset=i - t->runLength, ~len=t->runLength)
    let c = t.codes->Array.get(i)
    switch c {
    | Some(code) => {
        code->log
        lastSet->log
        isSumOf(lastSet, code)->Ok
      }
    | None => Error(InvalidIndex)
    }
  }

  type makeResult = Result.t<t, xmaxError>

  let make = (codes, runLength): makeResult => {
    runLength->log
    codes->Array.size->log
    switch codes->Array.size > runLength {
    | true => Ok({codes: codes, runLength: runLength})
    | false => Error(InvalidRunLength)
    }
  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim->Int.fromString->Option.getExn
  })

let solvePart1 = data => {
  let xmax = Xmax.make(data->parse, 5)
  xmax->Result.getExn->log
  %raw("Xmax")->consoleDir
  xmax->Result.getExn->Xmax.isCodeValid(14)->Result.getExn
}

let solvePart2 = data => {
  data->ignore
  2
}
