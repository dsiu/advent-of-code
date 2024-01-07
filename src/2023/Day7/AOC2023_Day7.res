// @@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

module Card = {
  type t = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

  include TableclothComparator.Make({
    type t = t

    let compare = (a, b) => compare(a, b)
  })

  let names: list<string> = "23456789TJQKA"->String.split("")->List.fromArray

  let make = str => {
    switch str {
    | "2" => Two
    | "3" => Three
    | "4" => Four
    | "5" => Five
    | "6" => Six
    | "7" => Seven
    | "8" => Eight
    | "9" => Nine
    | "T" => Ten
    | "J" => Jack
    | "Q" => Queen
    | "K" => King
    | "A" => Ace
    | _ => failwith("invalid card")
    }
  }

  let value = t => {
    switch t {
    | Two => 2
    | Three => 3
    | Four => 4
    | Five => 5
    | Six => 6
    | Seven => 7
    | Eight => 8
    | Nine => 9
    | Ten => 10
    | Jack => 11
    | Queen => 12
    | King => 13
    | Ace => 14
    }
  }
  let compare_Ord = (a, b) => Int.compare(value(a), value(b))
  let compare = (a, b) => compare_Ord(a, b)->Ordering.toInt
}

type handClass = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind

type hand = Hand(array<Card.t>, int)

type classifiedHand = CHand(handClass, array<Card.t>, int)

type signatureElement = (int, list<Card.t>)
type signature = array<signatureElement>

//sign :: [Card] -> Signature
//sign = reverse . sort . fmap (\g -> (length g, g)) . group . sort
//let sign: array<card> => signature = cards => {}
//let sign = (cards: array<Card.t>): array<(int, array<Card.t>)> => {
let sign = (cards: array<Card.t>): signature => {
  let sortedCards = cards->Array.toSorted(Card.compare_Ord)
  let groupedCards = sortedCards->Array.groupBy(module(Card), ~f=a => a)
  let mappedCards = groupedCards->Map.map(group => (List.length(group), group))->Map.valuesToArray
  let sortedMappedCards = mappedCards->Array.toSorted(((a1, a2), (b1, b2)) => {
    let c = Int.compare(a1, b1)
    c->Ordering.isEqual ? Card.compare_Ord(a2->List.getExn(0), b2->List.getExn(0)) : c
  })

  Array.toReversed(sortedMappedCards)
}

module HandsParser = {
  module P = ReludeParse.Parser
  open P.Infix
  let justSpace: P.t<unit> = P.void(P.many(P.str(" ")))

  let handC = (a, b) => {
    let cards = a->List.toArray
    Hand(cards, b)
  }

  //  let cardP =
  //    Two
  //    ->\"<$"(P.str("2"))
  //    ->\"<|>"(Three->\"<$"(P.str("3")))
  //    ->\"<|>"(Four->\"<$"(P.str("4")))
  //    ->\"<|>"(Five->\"<$"(P.str("5")))
  //    ->\"<|>"(Six->\"<$"(P.str("6")))
  //    ->\"<|>"(Seven->\"<$"(P.str("7")))
  //    ->\"<|>"(Eight->\"<$"(P.str("8")))
  //    ->\"<|>"(Nine->\"<$"(P.str("9")))
  //    ->\"<|>"(Ten->\"<$"(P.str("T")))
  //    ->\"<|>"(Jack->\"<$"(P.str("J")))
  //    ->\"<|>"(Queen->\"<$"(P.str("Q")))
  //    ->\"<|>"(King->\"<$"(P.str("K")))
  //    ->\"<|>"(Ace->\"<$"(P.str("A")))

  let cardP = Card.make->\"<$>"(P.anyOfStr(Card.names))

  let handP = handC->\"<$>"(justSpace->\"*>"(P.many(cardP)->\"<*"(justSpace)))->\"<*>"(P.anyInt)

  let handsP = P.sepBy(P.eol, handP)

  let run = str => {
    P.runParser(str, handsP)->Result.getExn->List.toArray
  }
}
let solvePart1 = data => {
  let hands = data->HandsParser.run
  let signed = hands->Array.map((Hand(cards, _)) => sign(cards))
  signed->Array.forEach(log)
  let cardsAsList = signed->Array.map(List.fromArray)

  let c = cardsAsList->Array.getUnsafe(0)
  let test = list{(2, [Card.Three, Card.Three]), (1, [Card.King])}
  c->log2("c", _)
  switch c {
  | list{(5, _), ..._} => FiveOfAKind
  | list{(4, _), ..._} => FourOfAKind
  | list{(3, _), (2, _)} => FullHouse
  | list{(3, _), ..._} => ThreeOfAKind
  | list{(2, _), (2, _), _} => TwoPair
  | list{(2, _), ..._} => OnePair
  | _ => HighCard
  }->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
