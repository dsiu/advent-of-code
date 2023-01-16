// let's try to use Relude
let log = Js.Console.log

open Relude
//open Utils

// common functions
let compose = Function.compose
module S = String
module A = Array
module O = Option

type operator = Plus | Times
type operand = Literal(int) | Old
type expression = Expression(operator, operand)
type monkeyCode = MonkeyCode({operation: expression, test: int, trueTarget: int, falseTarget: int})

module IntMap = {
  include Relude.Map.WithOrd(Relude.Int.Ord)
}

type monkeyCodes = IntMap.t<monkeyCode>

type monkeyDescription = MonkeyDescription({limit: int => int, codes: monkeyCodes}) // reader
type monkeyHolds = IntMap.t<array<int>> // state
type monkeyLog = MonkeyLog(int, int) // writer -- monkey ID, number of items handled this round
// type monkeyHandler = RWS MonkeyDescription [MonkeyLog] monkeyHolds

let parseEachMonkey = s => {
  let s = s->S.splitArray(~delimiter="\n", _)->A.map(S.trim, _)

  let parseMId = s => {
    s->O.bind(x => {
      x
      ->S.splitArray(~delimiter=" ", _)
      ->A.at(1, _)
      ->O.bind(x => {
        x
        ->S.splitArray(~delimiter=":", _)
        ->A.at(0, _)
        ->O.bind({
          x => x->S.toInt
        })
      })
    })
  }

  let parseStarting = s => {
    s->O.bind(x => {
      x
      ->S.splitArray(~delimiter="Starting items: ", _)
      ->A.at(1, _)
      ->O.bind(x => {
        x->S.splitArray(~delimiter=",", _)->A.map({x => x->S.trim->S.toInt}, _)->O.pure
      })
    })
  }

  let mId = A.at(0, s)->parseMId
  let holding = A.at(1, s)->parseStarting
  (mId, holding)
}

let parse = data =>
  data->S.splitArray(~delimiter="\n\n", _)->A.map(compose(parseEachMonkey, S.trim), _)

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
