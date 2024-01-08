// @@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

module Card = {
  type t =
    Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

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
    | Joker => 1
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

module HandClass = {
  type t = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  let value = t => {
    switch t {
    | FiveOfAKind => 7
    | FourOfAKind => 6
    | FullHouse => 5
    | ThreeOfAKind => 4
    | TwoPair => 3
    | OnePair => 2
    | HighCard => 1
    }
  }
  let compare_Ord = (a, b) => Int.compare(value(a), value(b))
  let compare = (a, b) => compare_Ord(a, b)->Ordering.toInt
}

type hand = Hand(array<Card.t>, int) // (cards, bid)

type classifiedHand = CHand(HandClass.t, array<Card.t>, int)

type signatureElement = (int, array<Card.t>) // (count, cards)

type signature = array<signatureElement>

//sign :: [Card] -> Signature
//sign = reverse . sort . fmap (\g -> (length g, g)) . group . sort
//let sign: array<card> => signature = cards => {}
//let sign = (cards: array<Card.t>): array<(int, array<Card.t>)> => {
let sign = (cards: array<Card.t>): signature => {
  let innerSign = cards => {
    let sortedCards = cards->Array.toSorted(Card.compare_Ord)
    let groupedCards = sortedCards->Array.groupBy(module(Card), ~f=a => a)
    let mappedCards =
      groupedCards->Map.map(group => (List.length(group), group->List.toArray))->Map.valuesToArray
    let sortedMappedCards = mappedCards->Array.toSorted(((a1, a2), (b1, b2)) => {
      let c = Int.compare(a1, b1)
      c->Ordering.isEqual ? Card.compare_Ord(a2->Array.getUnsafe(0), b2->Array.getUnsafe(0)) : c
    })

    Array.toReversed(sortedMappedCards)
  }

  let (jokers, nonJokers) = cards->Array.partition(c => c == Card.Joker)

  let nonJokerSigned = nonJokers->innerSign

  let addJokers: (signature, signatureElement) => signature = (s, js) => {
    switch (s, js) {
    | ([], js) => [js]
    | (xs, (jn, js)) =>
      let (n, cs) = xs->Array.getUnsafe(0)
      Array.concat([(n + jn, cs->Array.concat(js))], Array.tail(xs))
    }
  }

  nonJokerSigned->addJokers((jokers->Array.length, jokers))
}

let classifySignature: signature => HandClass.t = signature => {
  switch signature {
  | [(5, _)] => FiveOfAKind
  | [(4, _), _] => FourOfAKind
  | [(3, _), (2, _)] => FullHouse
  | [(3, _), _]
  | [(3, _), _, _] =>
    ThreeOfAKind
  | [(2, _), (2, _), _] => TwoPair
  | [(2, _), _, _, _] => OnePair
  | _ => HighCard
  }
}

let classify: hand => classifiedHand = (Hand(cards, bid)) => {
  CHand(cards->sign->classifySignature, cards, bid)
}

let part1: array<hand> => int = hands => {
  let sortedHands =
    hands
    ->Array.map(classify)
    ->Array.toSorted((CHand(h1, c1, _), CHand(h2, c2, _)) => {
      let h = HandClass.compare_Ord(h1, h2)
      h->Ordering.isEqual
        ? Array.compare(c1, c2, (d, e) => Card.compare_Ord(d, e)->Ordering.invert)->Ordering.invert
        : h
    })
  let rankedHands = Array.zip(Array.range(~from=1, Array.length(sortedHands) + 1), sortedHands)

  let score = ((rank, CHand(_, _, bid))) => rank * bid

  rankedHands->Array.map(score)->Array.sum(module(Int))
}

let enJoker: hand => hand = (Hand(cards, bid)) => {
  let replaceJackWithJoker = card => {
    switch card {
    | Card.Jack => Card.Joker
    | _ => card
    }
  }

  Hand(cards->Array.map(replaceJackWithJoker), bid)
}

let part2: array<hand> => int = hands => {
  hands->Array.map(enJoker)->part1
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
  data->HandsParser.run->part1
}

let solvePart2 = data => {
  data->HandsParser.run->part2
}
