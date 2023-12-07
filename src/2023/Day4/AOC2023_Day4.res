@@uncurried

open RescriptCore
open Utils
let log = Console.log
let log2 = Console.log2

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

let part1: array<card> => int = cards => {
  cards
  ->Array.map(({id, winners, actuals}) => {
    open Belt.Set.Int
    let winnerSet = Belt.Set.Int.fromArray(winners)
    let actualsSet = Belt.Set.Int.fromArray(actuals)
    let match = Belt.Set.Int.intersect(winnerSet, actualsSet)
    log2("match", match->toArray)
    let nMatches = match->size
    nMatches == 0 ? 0 : Math.Int.pow(2, ~exp=nMatches - 1)
  })
  ->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
