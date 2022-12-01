open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2
let log3 = Js.Console.log3

module P = Res_parser

module Math = {
  type rec expr =
    | E1(expr, addop, term)
    | ET(term)
  and term = T1(term, mulop, factor) | TF(factor)
  and factor = Int(int) | Paren(expr)
  and addop = Plus
  and mulop = Times

  type expr_ = P.t<expr>
  let expr: expr_ = P.makeRecursive(p => {
    ()
  })

  type result = P.parseResult<expr>
  let parse = (s): result => P.run(expr, s)
}

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  let d = data
  let l = Math.parse(d)
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
