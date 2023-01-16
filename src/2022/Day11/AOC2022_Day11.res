open Belt
open Utils
let log = Js.Console.log

type operator = Plus | Times
type operand = Literal(int) | Old
type expression = Expression(operator, operand)

type monkeyCode = MonkeyCode({operation: expression, test: int, trueTarget: int, falseTarget: int})

module IntMap = {
  include Relude.Map.WithOrd(Relude.Int.Ord)
}

type monkeyCodes = IntMap.t<monkeyCode>

type monkeyDescription = MonkeyDescription({limit: int => int, codes: monkeyCodes})

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
