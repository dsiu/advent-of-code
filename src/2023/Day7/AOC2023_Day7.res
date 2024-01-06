@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type card =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type handClass =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind

type hand = Hand(array<card>, int)

type classifiedHand = CHand(handClass, array<card>, int)

module HandsParser = {
  module P = ReludeParse.Parser
  open P.Infix
  let justSpace: P.t<unit> = P.void(P.many(P.str(" ")))

  let handC = (a, b) => {
    let cards = a->List.toArray
    Hand(cards, b)
  }

  let cardP =
    Two
    ->\"<$"(P.str("2"))
    ->\"<|>"(Three->\"<$"(P.str("3")))
    ->\"<|>"(Four->\"<$"(P.str("4")))
    ->\"<|>"(Five->\"<$"(P.str("5")))
    ->\"<|>"(Six->\"<$"(P.str("6")))
    ->\"<|>"(Seven->\"<$"(P.str("7")))
    ->\"<|>"(Eight->\"<$"(P.str("8")))
    ->\"<|>"(Nine->\"<$"(P.str("9")))
    ->\"<|>"(Ten->\"<$"(P.str("T")))
    ->\"<|>"(Jack->\"<$"(P.str("J")))
    ->\"<|>"(Queen->\"<$"(P.str("Q")))
    ->\"<|>"(King->\"<$"(P.str("K")))
    ->\"<|>"(Ace->\"<$"(P.str("A")))

  let handP = handC->\"<$>"(justSpace->\"*>"(P.many(cardP)->\"<*"(justSpace)))->\"<*>"(P.anyInt)

  let handsP = P.sepBy(P.eol, handP)

  let cardP =
    Two
    ->\"<$"(P.str("2"))
    ->\"<|>"(Three->\"<$"(P.str("3")))
    ->\"<|>"(Four->\"<$"(P.str("4")))
    ->\"<|>"(Five->\"<$"(P.str("5")))
    ->\"<|>"(Six->\"<$"(P.str("6")))
    ->\"<|>"(Seven->\"<$"(P.str("7")))
    ->\"<|>"(Eight->\"<$"(P.str("8")))
    ->\"<|>"(Nine->\"<$"(P.str("9")))
    ->\"<|>"(Ten->\"<$"(P.str("T")))
    ->\"<|>"(Jack->\"<$"(P.str("J")))
    ->\"<|>"(Queen->\"<$"(P.str("Q")))
    ->\"<|>"(King->\"<$"(P.str("K")))
    ->\"<|>"(Ace->\"<$"(P.str("A")))

  let run = str => {
    P.runParser(str, handsP)->Result.getExn->List.toArray
  }
}
let solvePart1 = data => {
  let hand = data->HandsParser.run->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
