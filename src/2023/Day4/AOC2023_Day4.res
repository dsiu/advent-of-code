@@uncurried

open RescriptCore
open Utils
let log = Console.log

type card = {
  id: int,
  winners: array<int>,
  actuals: array<int>,
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => {
    let cardAndNumbers = l->String.trim->String.split(": ")
    let cardId =
      cardAndNumbers[0]->Option.flatMap(s => s->String.replace("Card ", "")->Int.fromString)

    let numberStrs = cardAndNumbers[1]->Option.flatMap(s => {
      s
      ->String.split(" | ")
      ->Array.map(
        nums => {
          nums->String.trim->String.split(" ")->Array.filterMap(Int.fromString(~radix=10))
        },
      )
      ->Some
    })
    {
      id: cardId->Option.getExn,
      winners: numberStrs->Option.flatMap(Array.at(_, 0))->Option.getExn,
      actuals: numberStrs->Option.flatMap(Array.at(_, 1))->Option.getExn,
    }
  })

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
