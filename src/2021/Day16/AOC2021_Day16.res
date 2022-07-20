open Belt
open Utils
let log = Js.Console.log

module P = Res_parser

// Define our AST
type sign = Plus | Subtract | Multiply
type number = SingleDigit(int)
type expression = Expression(number, sign, number)

// Parse sign and transform into AST node
type sign_ = P.t<sign>
let sign: sign_ = P.choice([
  P.char('+')->P.map(_ => Plus),
  P.char('-')->P.map(_ => Subtract),
  P.char('x')->P.map(_ => Multiply),
])

// Helper that returns any parser
// that's found to be wrapped in whitespace
type manyWhitespace = P.t<list<char>>
let manyWhitespace: manyWhitespace = P.many(P.char(' '))

// Parse single digit and transform into AST node
type singleDigit = P.t<number>
let singleDigit: singleDigit =
  P.satisfy(char => char >= '0' && char <= '9')
  ->P.map(Char.escaped)
  ->P.map(int_of_string)
  ->P.map(x => SingleDigit(x))
  ->P.between(manyWhitespace, manyWhitespace)

// Combine sign and digit parsers into fully typed AST.
type parser = P.t<expression>
let parser: parser =
  singleDigit
  ->P.andThen(sign)
  ->P.andThen(singleDigit)
  ->P.map((((left, sign), right)) => Expression(left, sign, right))

// Run our parser against an input string.
type result = result<(expression, string), string>
let result = P.run(parser, " 1 +  4  ")

// Result is a tuple of a valid AST and the state object.

// result == Ok(Expression(SingleDigit(1), Plus, SingleDigit(4)), _)

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
