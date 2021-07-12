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
    t.raw->Js.String2.split("")->Array.map(c => c->replace(_, t.zero, "0")->replace(_, t.one, "1"))
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
}

let parse = data => data->Js.String2.split("\n")

let solvePart1 = data => {
  let passes = data->parse->Array.map(BoardingPass.make)
  "rows"->log
  passes->Array.map(BoardingPass.getRow)->log
  "columns"->log
  passes->Array.map(BoardingPass.getColumn)->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
