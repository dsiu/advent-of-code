@@uncurried

open Stdlib
open Utils

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

/**
 * This function converts a string of spelled-out numbers into digits.
 * It takes a string and a match direction as input.
 * The match direction determines whether the string is processed from left to right or from right to left.
 * The function returns a string of digits.
 *
 * @param {string} str - A string of spelled-out numbers.
 * @param {matchDirection} matchDir - A match direction which is either Left2Right or Right2Left.
 * @returns {string} - A string of digits.
 *
 * @example
 * let str = "one two three"
 * spelledOutToDigits(str, Left2Right) // Returns "123"
 * spelledOutToDigits(str, Right2Left) // Returns "321"
 */
let spelledOutToDigits: (string, matchDirection) => string = (str, matchDir: matchDirection) => {
  switch matchDir {
  | Left2Right => str
  | Right2Left => str->String.reverse
  }
  ->Utils.splitChars
  ->Belt.Array.reduce("", (acc, c) => {
    numberNames->Belt.Array.reduce(
      switch matchDir {
      | Left2Right => acc ++ c
      | Right2Left => c ++ acc
      },
      (curStr, (name, digit)) => {
        curStr->String.replace(name, digit)
      },
    )
  })
}

/**
 * This function extracts the first and last digit from a string of digits.
 * It takes a string of digits as input and returns a tuple of the first and last digit as integers.
 *
 * @param {string} str - A string of digits.
 * @returns {(int, int)} - A tuple containing the first and last digit of the input string as integers.
 *
 * @example
 * let str = "12345"
 * get2Digits(str) // Returns (1, 5)
 */
let get2Digits: string => (int, int) = str => {
  let digits = str->splitChars->Array.filterMap(Int.fromString(~radix=10, ...))
  let first = digits->Array.at(0)->Option.getUnsafe
  let last = digits->Array.at(-1)->Option.getUnsafe
  (first, last)
}

/**
 * This function combines the first and last digit into a single integer.
 * It takes a tuple of the first and last digit as input and returns an integer.
 * The first digit is placed in the tens place and the last digit is placed in the ones place.
 *
 * @param {(int, int)} firstLast - A tuple containing the first and last digit as integers.
 * @returns {int} - An integer formed by combining the first and last digit.
 *
 * @example
 * let firstLast = (1, 5)
 * combineFirstAndLast(firstLast) // Returns 15
 */
let combineFirstAndLast: ((int, int)) => int = ((first, last)) => {first * 10 + last}

/**
 * This function calculates the sum of the combined first and last digits of each string in an array.
 * It takes an array of strings as input and returns an integer.
 * Each string is processed twice, once from left to right and once from right to left.
 * The first and last digit are extracted from each processed string and combined into a single integer.
 * The function returns the sum of these integers.
 *
 * @param {array<string>} xs - An array of strings where each string contains spelled-out numbers.
 * @returns {int} - The sum of the combined first and last digits of each string in the input array.
 *
 * @example
 * let xs = ["one two three", "four five six", "seven eight nine"]
 * part2(xs) // Returns the sum of the combined first and last digits of each string in the input array.
 */
let part2: array<string> => int = xs => {
  xs
  ->Array.map(x => {
    let (first, _) = x->spelledOutToDigits(Left2Right)->get2Digits
    let (_, last) = x->spelledOutToDigits(Right2Left)->get2Digits
    combineFirstAndLast((first, last))
  })
  ->sumIntArray
}

/**
 * This function calculates the sum of the combined first and last digits of each string in an array.
 * It takes an array of strings as input and returns an integer.
 * Each string is processed from left to right.
 * The first and last digit are extracted from each processed string and combined into a single integer.
 * The function returns the sum of these integers.
 *
 * @param {array<string>} xs - An array of strings where each string contains spelled-out numbers.
 * @returns {int} - The sum of the combined first and last digits of each string in the input array.
 *
 * @example
 * let xs = ["one two three", "four five six", "seven eight nine"]
 * part1(xs) // Returns the sum of the combined first and last digits of each string in the input array.
 */
let part1: array<string> => int = xs => {
  let result = xs->Array.map(Utils.compose(get2Digits, combineFirstAndLast))
  result->sumIntArray
}

let parse: string => array<string> = data => data->splitNewline->Array.map(String.trim)

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
