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

/**
 * This function calculates the number of matches between the winners and actuals arrays in a card.
 * It takes a card object as input and returns an integer.
 * The function uses the intersect method from the Belt.Set.Int module to find the intersection of the winners and actuals arrays.
 * The size of the intersection is returned as the number of matches.
 *
 * @param {card} card - A card object containing an id, winners array, and actuals array.
 * @returns {int} - The number of matches between the winners and actuals arrays in the card.
 *
 * @example
 * let card = {id: 1, winners: [|1, 2, 3|], actuals: [|2, 3, 4|]}
 * matchPerCard(card) // Returns 2
 */
let matchPerCard: card => int = ({winners, actuals}: card) => {
  module IntSet = Belt.Set.Int
  IntSet.intersect(winners->IntSet.fromArray, actuals->IntSet.fromArray)->IntSet.size
}

/**
 * This function creates a queue of cards with their match count and initial quantity.
 * It takes an array of cards as input and returns a queue.
 * Each card in the queue is represented as an object with the number of matches and a queued quantity of 1.
 *
 * @param {array<card>} cards - An array of card objects where each card has an id, winners array, and actuals array.
 * @returns {queue} - A queue of cards where each card is represented as an object with the number of matches and a queued quantity of 1.
 *
 * @example
 * let cards = [|{id: 1, winners: [|1, 2, 3|], actuals: [|2, 3, 4|]}, {id: 2, winners: [|4, 5, 6|], actuals: [|5, 6, 7|]}|]
 * mkQueue(cards) // Returns a queue of cards where each card is represented as an object with the number of matches and a queued quantity of 1.
 */
let mkQueue: array<card> => queue = cards => {
  cards->Array.map(c => {numMatches: c->matchPerCard, queuedQuantity: 1})
}

/**
 * This function duplicates the first `n` cards in the queue by increasing their `queuedQuantity` by `scale`.
 * It takes an integer `n`, an integer `scale`, and a queue of cards as input.
 * The function returns a new queue where the first `n` cards have their `queuedQuantity` increased by `scale`.
 *
 * @param {int} n - The number of cards to duplicate.
 * @param {int} scale - The amount to increase the `queuedQuantity` of each duplicated card.
 * @param {queue} queue - A queue of cards where each card is represented as an object with the number of matches and a queued quantity.
 * @returns {queue} - A new queue where the first `n` cards have their `queuedQuantity` increased by `scale`.
 *
 * @example
 * let queue = [|{numMatches: 2, queuedQuantity: 1}, {numMatches: 3, queuedQuantity: 1}, {numMatches: 4, queuedQuantity: 1}|]
 * duplicateCards(2, 2, queue) // Returns [|{numMatches: 2, queuedQuantity: 3}, {numMatches: 3, queuedQuantity: 3}, {numMatches: 4, queuedQuantity: 1}|]
 */
let duplicateCards: (int, int, queue) => queue = (n, scale, queue) => {
  let qPre = queue->Array.slice(~start=0, ~end=n)
  let qPost = queue->Array.sliceToEnd(~start=n)
  let dup = qPre->Array.map(({numMatches, queuedQuantity}) => {
    numMatches,
    queuedQuantity: queuedQuantity + scale,
  })
  Array.concat(dup, qPost)
}

/**
 * This function calculates the point value for a given number of matches.
 * It takes an integer as input and returns an integer.
 * If the input is 0, the function returns 0.
 * Otherwise, the function returns 2 to the power of (input - 1).
 *
 * @param {int} n - The number of matches.
 * @returns {int} - The point value for the given number of matches.
 *
 * @example
 * calculatePoint(0) // Returns 0
 * calculatePoint(3) // Returns 4
 */
let calculatePoint: int => int = n => {
  n == 0 ? 0 : Math.Int.pow(2, ~exp=n - 1)
}

/**
 * This function calculates the total point value for an array of cards.
 * It takes an array of cards as input and returns an integer.
 * The function maps over the array of cards and applies the `matchPerCard` and `calculatePoint` functions to each card.
 * The `matchPerCard` function calculates the number of matches for a card and the `calculatePoint` function calculates the point value for a given number of matches.
 * The function then sums up the point values of all cards and returns the total.
 *
 * @param {array<card>} cards - An array of card objects where each card has an id, winners array, and actuals array.
 * @returns {int} - The total point value for the array of cards.
 *
 * @example
 * let cards = [|{id: 1, winners: [|1, 2, 3|], actuals: [|2, 3, 4|]}, {id: 2, winners: [|4, 5, 6|], actuals: [|5, 6, 7|]}|]
 * part1(cards) // Returns the total point value for the array of cards.
 */
let part1: array<card> => int = cards => {
  cards
  ->Array.map(Utils.compose(matchPerCard, calculatePoint))
  ->sumIntArray
}

/**
 * This function calculates the total point value for an array of cards using a queue.
 * It takes an array of cards as input and returns an integer.
 * The function first creates a queue of cards with their match count and initial quantity.
 * It then enters a loop where it continuously dequeues the first card in the queue and adds its `queuedQuantity` to a running total.
 * If the dequeued card has a `numMatches` greater than 0, it duplicates the card by increasing its `queuedQuantity` and enqueues it at the end of the queue.
 * The loop continues until the queue is empty, at which point the function returns the total.
 *
 * @param {array<card>} cards - An array of card objects where each card has an id, winners array, and actuals array.
 * @returns {int} - The total point value for the array of cards.
 *
 * @example
 * let cards = [|{id: 1, winners: [|1, 2, 3|], actuals: [|2, 3, 4|]}, {id: 2, winners: [|4, 5, 6|], actuals: [|5, 6, 7|]}|]
 * part2(cards) // Returns the total point value for the array of cards.
 */
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

/**
 * This function parses a string of data into an array of card objects.
 * Each card object has an id, winners array, and actuals array.
 *
 * @param {string} data - A string of data where each line represents a card.
 * Each line is formatted as "Card {id}: {winners} | {actuals}".
 * @returns {array<card>} - An array of card objects where each card has an id, winners array, and actuals array.
 *
 * @example
 * let data = "Card 1: 1 2 3 | 2 3 4\nCard 2: 4 5 6 | 5 6 7"
 * parse(data) // Returns an array of card objects where each card has an id, winners array, and actuals array.
 */
let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => {
    let [cardIdStr, numberStrs] = l->String.trim->String.split(": ")
    let cardId = cardIdStr->String.replace("Card ", "")->Int.fromString->Option.getExn
    let [winnersStr, actualsStr] = numberStrs->String.split(" | ")
    let winners = winnersStr->String.split(" ")->Array.filterMap(Int.fromString(~radix=10))
    let actuals = actualsStr->String.split(" ")->Array.filterMap(Int.fromString(~radix=10))

    {id: cardId, winners, actuals}
  })

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
