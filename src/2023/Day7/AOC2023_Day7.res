// @@uncurried

open Stdlib
let log = Console.log
let log2 = Console.log2

module Card = {
  type t =
    Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

  include TableclothComparator.Make({
    type t = t

    let compare = (a, b) => compare(a, b)
  })

  // mainly for Parser to pick up string literals
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

  // need to assign value to constructors so that comparison works
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
  // RescriptCore ordering - for Array sort
  let compare_Ord = (a, b) => Int.compare(value(a), value(b))

  // Array.groupBy is using Belt, so this compare is for that
  let compare = (a, b) => compare_Ord(a, b)->Ordering.toInt
}

module HandClass = {
  type t = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  let value = t => {
    switch t {
    | HighCard => 1
    | OnePair => 2
    | TwoPair => 3
    | ThreeOfAKind => 4
    | FullHouse => 5
    | FourOfAKind => 6
    | FiveOfAKind => 7
    }
  }
  let compare_Ord = (a, b) => Int.compare(value(a), value(b))
  let compare = (a, b) => compare_Ord(a, b)->Ordering.toInt
}

type hand = Hand(array<Card.t>, int) // (cards, bid)

type classifiedHand = CHand(HandClass.t, array<Card.t>, int)

type signatureElement = (int, array<Card.t>) // (count, cards)

let signatureElementCompare: (signatureElement, signatureElement) => Ordering.t = (
  (n1, c1),
  (n2, c2),
) => {
  let n = Int.compare(n1, n2)
  n->Ordering.isEqual ? Card.compare_Ord(c1->Array.getUnsafe(0), c2->Array.getUnsafe(0)) : n
}

type signature = array<signatureElement>

/**
 * This function is used to create a signature for a given array of cards.
 * A signature is an array of tuples where each tuple represents a group of cards of the same type.
 * The first element of the tuple is the count of cards in the group and the second element is an array of the cards in the group.
 * The signature is sorted in descending order based on the count of cards in each group and the value of the cards in the group.
 *
 * @param cards - An array of cards for which the signature is to be created.
 * @returns The signature for the given array of cards.
 */
let sign = (cards: array<Card.t>): signature => {
  /*
   * This function is used to create a signature for a given array of cards excluding jokers.
   *
   * @param cards - An array of cards excluding jokers for which the signature is to be created.
   * @returns The signature for the given array of cards excluding jokers.
   */
  let innerSign = cards => {
    cards
    ->Array.toSorted(Card.compare_Ord)
    ->Array.groupBy(module(Card), ~f=a => a)
    ->Belt.Map.valuesToArray
    ->Array.map(group => (List.length(group), group->List.toArray))
    ->Array.toSorted(signatureElementCompare)
    ->Array.toReversed
  }

  // Partition the array of cards into jokers and non-jokers.
  let (jokers, nonJokers) = cards->Array.partition(c => c == Card.Joker)

  // Create a signature for the non-jokers.
  let nonJokerSigned = nonJokers->innerSign

  /*
   * This function is used to add jokers to the signature of non-jokers.
   *
   * @param s - The signature of non-jokers.
   * @param js - The group of jokers to be added to the signature.
   * @returns The signature after adding the jokers.
   */
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

/**
 * This function is used to classify a signature into a hand class.
 * A signature is an array of tuples where each tuple represents a group of cards of the same type.
 * The first element of the tuple is the count of cards in the group and the second element is an array of the cards in the group.
 * The hand class is determined based on the count of cards in each group in the signature.
 *
 * @param signature - The signature to be classified.
 * @returns The hand class for the given signature.
 */
let classifySignature: signature => HandClass.t = signature => {
  switch signature {
  | [(5, _)] => FiveOfAKind // If there is a group of 5 cards of the same type, the hand class is FiveOfAKind.
  | [(4, _), _] => FourOfAKind // If there is a group of 4 cards of the same type, the hand class is FourOfAKind.
  | [(3, _), (2, _)] => FullHouse // If there is a group of 3 cards of the same type and a group of 2 cards of the same type, the hand class is FullHouse.
  | [(3, _), _]
  | [(3, _), _, _] =>
    ThreeOfAKind // If there is a group of 3 cards of the same type, the hand class is ThreeOfAKind.
  | [(2, _), (2, _), _] => TwoPair // If there are two groups of 2 cards of the same type, the hand class is TwoPair.
  | [(2, _), _, _, _] => OnePair // If there is a group of 2 cards of the same type, the hand class is OnePair.
  | _ => HighCard // If none of the above conditions are met, the hand class is HighCard.
  }
}

/**
 * This function is used to classify a hand into a classified hand.
 * A hand is a tuple containing an array of cards and a bid.
 * A classified hand is a tuple containing a hand class, an array of cards and a bid.
 * The hand class is determined by creating a signature for the array of cards in the hand and classifying the signature.
 *
 * @param hand - The hand to be classified.
 * @returns The classified hand for the given hand.
 */
let classify: hand => classifiedHand = (Hand(cards, bid)) => {
  CHand(cards->sign->classifySignature, cards, bid)
}

/**
 * This function is used to calculate the score for a given array of hands.
 * A hand is a tuple containing an array of cards and a bid.
 * The score is calculated by classifying each hand, sorting the classified hands based on the hand class and the value of the cards in the hand,
 * ranking the sorted hands, and then multiplying the rank of each hand by the bid of the hand.
 * The score for the array of hands is the sum of the scores for all the hands.
 *
 * @param hands - An array of hands for which the score is to be calculated.
 * @returns The score for the given array of hands.
 */
let part1: array<hand> => int = hands => {
  // Classify each hand and sort the classified hands based on the hand class and the value of the cards in the hand.
  let sortedHands =
    hands
    ->Array.map(classify)
    ->Array.toSorted((CHand(h1, c1, _), CHand(h2, c2, _)) => {
      let h = HandClass.compare_Ord(h1, h2)
      h->Ordering.isEqual
        ? Array.compare(c1, c2, (d, e) => Card.compare_Ord(d, e)->Ordering.invert)->Ordering.invert
        : h
    })

  // Rank the sorted hands.
  let rankedHands = Array.zip(Array.range(~from=1, Array.length(sortedHands) + 1), sortedHands)

  // Calculate the score for each hand by multiplying the rank of the hand by the bid of the hand.
  let score = ((rank, CHand(_, _, bid))) => rank * bid

  // Calculate the score for the array of hands by summing the scores for all the hands.
  rankedHands->Array.map(score)->Array.sum(module(Int))
}

/**
 * This function is used to replace all Jack cards in a hand with Joker cards.
 * A hand is a tuple containing an array of cards and a bid.
 * The function creates a new hand where all Jack cards have been replaced with Joker cards.
 *
 * @param hand - The hand in which Jack cards are to be replaced with Joker cards.
 * @returns The new hand with Jack cards replaced with Joker cards.
 */
let enJoker: hand => hand = (Hand(cards, bid)) => {
  /*
   * This function is used to replace a Jack card with a Joker card.
   *
   * @param card - The card to be replaced if it is a Jack card.
   * @returns The Joker card if the input card is a Jack card, otherwise the input card.
   */
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

  let handC = a =>
    b => {
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
