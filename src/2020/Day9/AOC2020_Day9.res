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
  let getCode = (t, i) => t.codes->Array.get(i)
  let getCodeExn = (t, i) => t.codes->Array.getExn(i)
  let codeSize = t => t.codes->Array.size
  let runLength = t => t.runLength
  let preambles = t => t->codes->Array.slice(~offset=0, ~len=t->runLength)

  type makeResult = Result.t<t, xmaxError>

  let make = (codes, runLength): makeResult => {
    //    runLength->log
    //    codes->Array.size->log
    switch codes->Array.size > runLength {
    | true => Ok({codes: codes, runLength: runLength})
    | false => Error(InvalidRunLength)
    }
  }

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

  let findSumOf = (xs, sum) => {
    let sumArray = xs->isSumOf(sum)
    sumArray->Array.size == 0 ? None : Some(sumArray)
  }

  // is code at index i (0-base) a valid code
  let isCodeValid = (t, i) => {
    let lastSet = t->codes->Array.slice(~offset=i - t->runLength, ~len=t->runLength)
    let c = t->getCode(i)
    switch c {
    | Some(code) =>
      //      "isCodeValid looking at code"->log
      //      code->log
      //      "lastSet"->log
      //      lastSet->log
      findSumOf(lastSet, code)->Ok
    | None => Error(InvalidIndex)
    }
  }

  let findInvalidCode = t => {
    let rec findInvalidInner = (t, i) => {
      //      "i"->log
      //      i->log
      i >= t->codeSize
        ? None
        : switch t->isCodeValid(i) {
          | Ok(sumOk) =>
            switch sumOk {
            | Some(sums) => t->findInvalidInner(i + 1)
            | None => Some(t->getCodeExn(i)) // report bad code index
            }
          | Error(e) => None
          }
    }
    t->findInvalidInner(t->runLength)
  }

  // sum up elements of array from begin to end (inclusive)
  let sumRange = (xs, ~offset, ~len) => {
    let elems = xs->Array.slice(~offset, ~len)
    let total = ref(0)
    elems->Array.forEach(x => total := total.contents + x)
    total.contents
  }

  //  let findContiguousSet = (t, badCode) => {
  //    let inner =
  //  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim->Int.fromString->Option.getExn
  })

let solvePart1 = (data, preambleSize) => {
  let xmax = Xmax.make(data->parse, preambleSize)->Result.getExn
  //  xmax->log
  //  %raw("Xmax")->consoleDir
  //  xmax->Xmax.isCodeValid(14)->Result.getExn->log
  let result = xmax->Xmax.findInvalidCode->Option.getExn
  result
}

let solvePart2 = (data, preambleSize) => {
  let xmax = Xmax.make(data->parse, preambleSize)->Result.getExn
  xmax->log

  let badCode = xmax->Xmax.findInvalidCode->Option.getExn
  badCode->log
  xmax->Xmax.codes->Xmax.sumRange(~offset=0, ~len=2)->log
  2
}
