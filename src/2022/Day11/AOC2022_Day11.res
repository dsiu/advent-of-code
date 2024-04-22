// let's try to use Relude
let log = Stdlib.Console.log
let log2 = Stdlib.Console.log2

// common functions
module S = Relude.String
module A = Relude.Array
module L = Relude.List
module O = Relude.Option
module R = Relude.Result
module F = Relude.Function
let compose = F.compose

type operator = Plus | Times
type operand = Literal(int) | Old
let makeLiteral = i => Literal(i)
type expression = Expression(operator, operand)

let showExpression = (Expression(op, operand)) => {
  switch op {
  | Plus => " + "
  | Times => " * "
  } ++
  switch operand {
  | Literal(i) => i->string_of_int
  | Old => "old"
  }
}
let logExp = compose(log, showExpression, ...)

let makeExpression = op => operand => Expression(op, operand)

type monkeyCode = MonkeyCode({operation: expression, test: int, trueTarget: int, falseTarget: int})

module IntMap = {
  include Relude.Map.WithOrd(Relude.Int.Ord)
}

type monkeyCodes = IntMap.t<monkeyCode>

type monkeyDescription = MonkeyDescription({limit: int => int, codes: monkeyCodes}) // reader
type monkeyHolds = IntMap.t<array<int>> // state
type monkeyLog = MonkeyLog(int, int) // writer -- monkey ID, number of items handled this round
// type monkeyHandler = RWS MonkeyDescription [MonkeyLog] monkeyHolds

let updateWorry: (int, expression, int => int) => int = (
  current,
  Expression(operator, operand),
  threshold,
) => {
  let evalOperand = operand => {
    switch operand {
    | Literal(k) => k
    | Old => current
    }
  }
  let n = evalOperand(operand)

  switch operator {
  | Plus => threshold(current + n)
  | Times => threshold(current * n)
  }
}

let worryTest: (int, int) => bool = (divisor, worry) => {mod(worry, divisor) == 0}

let receivesItem: (int, int, monkeyHolds) => monkeyHolds = (mId, worry, items) => {
  IntMap.update(
    mId,
    maybeItems => {
      switch maybeItems {
      | Some(items) => Some(A.concat(items, [worry]))
      | None => None
      }
    },
    items,
  )
}

module MonkeyParser = {
  module P = ReludeParse.Parser
  open P.Infix

  // Monkey 0:
  //  Starting items: 79, 98
  //  Operation: new = old * 19
  //  Test: divisible by 23
  //    If true: throw to monkey 2
  //    If false: throw to monkey 3

  let mIdP = P.str("Monkey ")->\"*>"(P.anyInt)->\"<*"(P.str(":"))->\"<*"(P.eol)
  // let _ = mIdP->P.runParser("Monkey 123:\n", _)->R.tap(log, _)

  let startingP =
    P.str("  Starting items: ")->\"*>"(P.anyInt->(P.sepBy(P.str(", "), _)))->\"<*"(P.eol)
  // let _ = startingP->P.runParser("  Starting items: 79, 98, 100\n", _)->R.tap(log, _)

  let operatorP = {
    let expressionP = {
      let opP = Plus->\"<$"(P.str("+"))->\"<|>"(Times->\"<$"(P.str("*")))
      let operandP = makeLiteral->\"<$>"(P.anyInt)->\"<|>"(Old->\"<$"(P.str("old")))
      makeExpression->\"<$>"(opP->\"<*"(P.str(" ")))->\"<*>"(operandP)
    }

    P.str("  Operation: new = old ")->\"*>"(expressionP)->\"<*"(P.eol)
  }
  //  let _ = operatorP->P.runParser("  Operation: new = old + 19\n", _)->R.tap( logExp, _)
  //  let _ = operatorP->P.runParser("  Operation: new = old * 20\n", _)->R.tap( logExp, _)
  //  let _ = operatorP->P.runParser("  Operation: new = old + old\n", _)->R.tap(logExp, _)
  //  let _ = operatorP->P.runParser("  Operation: new = old * old\n", _)->R.tap(logExp, _)

  let testP = P.str("  Test: divisible by ")->\"*>"(P.anyInt)->\"<*"(P.eol)
  // let _ = testP->P.runParser("  Test: divisible by 23\n", _)->R.tap(log, _)
  let trueTargetP = P.str("    If true: throw to monkey ")->\"*>"(P.anyInt)->\"<*"(P.eol)
  //  let _ = trueTargetP->P.runParser("    If true: throw to monkey 12\n", _)->R.tap(log, _)
  let falseTargetP = P.str("    If false: throw to monkey ")->\"*>"(P.anyInt)
  //  let _ = falseTargetP->P.runParser("    If false: throw to monkey 33", _)->R.tap(log, _)

  let monkeyP = {
    let mkMonkeyPair = mId => holding => operation => test => trueTarget => falseTarget => {
      // (mId, holding->L.toArray, operation->showExpression, test, trueTarget, falseTarget)
      ((mId, MonkeyCode({operation, test, trueTarget, falseTarget})), (mId, holding))
    }

    mkMonkeyPair
    ->\"<$>"(mIdP)
    ->\"<*>"(startingP)
    ->\"<*>"(operatorP)
    ->\"<*>"(testP)
    ->\"<*>"(trueTargetP)
    ->\"<*>"(falseTargetP)
  }
  //  let _ =
  //    monkeyP
  //    ->P.runParser(
  //      "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n",
  //      _,
  //    )
  //    ->R.tap(log, _)

  let monkeysP = {
    let makeMonkeyMaps = monkeys => {
      (IntMap.fromList(L.map(fst, monkeys)), IntMap.fromList(L.map(snd, monkeys)))
    }
    makeMonkeyMaps->\"<$>"(monkeyP->(P.sepBy(P.eol->\"<*"(P.eol), _)))
    //    monkeyP->P.sepBy(P.eol->\"<*"(P.eol), _)
  }

  let parse = s => monkeysP->(P.runParser(s, _))
}

//let parse = data =>
//  data->S.splitArray(~delimiter="\n\n", _)->A.map(compose(parseEachMonkey, S.trim), _)

let solvePart1 = data => {
  let result = data->MonkeyParser.parse
  let _ = result->(R.tap(log, _))
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
