@@uncurried

open RescriptCore
open Utils
module TC = Tablecloth

let log = Console.log

let numberNames = [
  ("zero", "0"),
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9"),
]

type matchDirection = Left2Right | Right2Left

// only need to run with Right2Left to work
let spelledOutToDigits = (str, matchDir: matchDirection) => {
  switch matchDir {
  | Left2Right => str
  | Right2Left => str->TC.String.reverse
  }
  ->Utils.splitChars
  ->Belt.Array.reduce("", (acc, c) => {
    let replaced = numberNames->Belt.Array.reduce(
      switch matchDir {
      | Left2Right => acc ++ c
      | Right2Left => c ++ acc
      },
      (curStr, (name, digit)) => {
        let result = curStr->String.replace(name, digit)
        result
      },
    )
    replaced
  })
}

let get2Digits = str => {
  let digits = str->splitChars->Array.map(Int.fromString(~radix=10))->Array.keepSome
  let first = digits->Array.at(0)->Option.getUnsafe
  let last = digits->Array.at(-1)->Option.getUnsafe
  (first, last)
}

let combineFirstAndLast = ((first, last)) => {first * 10 + last}

let part2 = xs => {
  let result = xs->Array.map(x => {
    //    let (first, _) = x->spelledOutToDigits(Left2Right)->get2Digits
    let (first, last) = x->spelledOutToDigits(Right2Left)->get2Digits
    combineFirstAndLast((first, last))
  })
  result->sumIntArray
}

let part1 = xs => {
  let result = xs->Array.map(x => x->get2Digits->combineFirstAndLast)
  result->sumIntArray
}

let parse = data => data->splitNewline->Array.map(String.trim)

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
