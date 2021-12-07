open Belt
let log = Js.Console.log
//open Utils

module BitString = {
  type t = {nBits: int, zero: string, one: string, raw: string}

  let make = (~zero, ~one, ~raw) => {
    nBits: raw->Js.String2.length,
    zero: zero,
    one: one,
    raw: raw,
  }

  let replace = (src, from, to_) => src == from ? to_ : src

  let get = t => {
    t.raw
    ->Js.String2.split("")
    ->Array.map(c => c->replace(_, t.zero, "0")->replace(_, t.one, "1"))
    ->Js.Array2.joinWith(_, "")
    ->Utils.parseInt(~x=_, ~base=2)
  }
}

module BoardingPass = {
  type t = {row: BitString.t, column: BitString.t}
  let make = code => {
    row: BitString.make(~zero="F", ~one="B", ~raw=Js.String2.slice(code, ~from=0, ~to_=7)),
    column: BitString.make(~zero="L", ~one="R", ~raw=code->Js.String2.slice(_, ~from=7, ~to_=10)),
  }

  let getRow = t => t.row->BitString.get
  let getColumn = t => t.column->BitString.get
  let getSeatId = t => t->getRow * 8 + t->getColumn
}

let parse = data => data->Js.String2.split("\n")->Array.map(Js.String2.trim)

let maxReducer = (a, x) => x > a ? x : a
let findGap = (a, x) => x - a == 1 ? x : a

let solvePart1 = data => {
  let passes = data->parse->Array.map(BoardingPass.make)
  //  "rows"->log
  //  passes->Array.map(BoardingPass.getRow)->log
  //  "columns"->log
  //  passes->Array.map(BoardingPass.getColumn)->log
  //  "seatIds"->log
  let seatIds = passes->Array.map(BoardingPass.getSeatId)
  seatIds->Array.reduce(_, 0, maxReducer)
}

let solvePart2 = data => {
  let passes = data->parse->Array.map(BoardingPass.make)
  let seatIds = passes->Array.map(BoardingPass.getSeatId)
  let sortedSeatIds = seatIds->Belt.SortArray.Int.stableSort
  //  "sortedSeatIds"->log
  //  sortedSeatIds->log
  let init = sortedSeatIds->Array.getExn(0) - 1
  sortedSeatIds->Array.reduce(_, init, findGap) + 1
}
