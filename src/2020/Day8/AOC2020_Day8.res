open Belt
open Utils
let log = Js.Console.log

module Instruction = {
  type opcode = NOP | ACC | JMP
  type arg = int

  type t = {
    opcode: opcode,
    arg: arg,
  }

  let make = (op, arg) => {
    (op, arg)->log
    1
  }
}

let parseLine = l => {
  l
  ->Js.String2.trim
  ->Js.String2.split(_, " ")
  ->(
    x => {
      let op = x[0]
      let arg = x[1]
      Instruction.make(op, arg)
    }
  )
}

let parse = data => data->splitNewline->Array.map(parseLine)

let solvePart1 = data => {
  data->parse
  //  let res = "234"->Int.fromString
  //  res
}

let solvePart2 = data => {
  data->ignore
  2
}
