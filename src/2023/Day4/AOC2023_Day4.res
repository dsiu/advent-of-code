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

type queuedCard = {
  numMatches: int,
  queuedQuantity: int,
}

type queue = array<queuedCard>

let matchPerCard: card => int = ({winners, actuals}: card) => {
  module IntSet = Belt.Set.Int
  let winnerSet = IntSet.fromArray(winners)
  let actualSet = IntSet.fromArray(actuals)
  IntSet.intersect(winnerSet, actualSet)->IntSet.size
}

let mkQueue: array<card> => queue = cards => {
  cards->Array.map(c => {numMatches: c->matchPerCard, queuedQuantity: 1})
}

let duplicateCards: (int, int, queue) => queue = (n, scale, queue) => {
  let qPre = queue->Array.slice(~start=0, ~end=n)
  let qPost = queue->Array.sliceToEnd(~start=n)
  //  log2("qPre", qPre)
  //  log2("qPost", qPost)
  let dup = qPre->Array.map(({numMatches, queuedQuantity}) => {
    numMatches,
    queuedQuantity: queuedQuantity + scale,
  })
  Array.concat(dup, qPost)
}

let calculatePoint: int => int = n => {
  n == 0 ? 0 : Math.Int.pow(2, ~exp=n - 1)
}

let part1: array<card> => int = cards => {
  cards
  ->Array.map(Utils.compose(matchPerCard, calculatePoint))
  ->sumIntArray
}

let part2: array<card> => int = cards => {
  let queue = cards->mkQueue
  let rec loop = (n, q) => {
    switch (n, q->Array.length) {
    | (n, 0) => n
    | (n, _) => {
        let {numMatches, queuedQuantity} = q->Array.get(0)->Option.getExn
        let n' = n + queuedQuantity
        let queue' = duplicateCards(numMatches, queuedQuantity, q->Array.sliceToEnd(~start=1))
        loop(n', queue')
      }
    }
  }
  loop(0, queue)
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
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
