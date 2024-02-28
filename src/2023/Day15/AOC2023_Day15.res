open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type instruction = Remove(string) | Insert(string, int)
type lens = {lensLabel: string, lensPower: int}
type facility = Map.t<int, array<lens>>

/**
 * This function converts the first character of a string into its ASCII value.
 *
 * @param s: The string to be converted.
 * @returns: The ASCII value of the first character of the input string.
 * @throws: If the string is empty, it throws an exception.
 */
let charToASCII = s => {
  s->String.codePointAt(0)->Option.getExn
}

/**
 * This function calculates a hash value for a given string.
 *
 * @param str: The string to be hashed.
 * @returns: The hash value of the input string. It is calculated by splitting the string into
 * characters, converting each character into its ASCII value, adding it to the accumulated value,
 * multiplying by 17, and then taking the modulus 256 of the result.
 * @throws: If the string is empty, it throws an exception.
 *
 * Example:
 * let result = hash("example")
 * Js.log(result) // Outputs: 123 (this is a hypothetical output, actual output may vary)
 */
let hash = str => {
  str
  ->String.split("")
  ->Array.reduce(0, (acc, c) => {
    ((acc + charToASCII(c)) * 17)->Int.mod(256)
  })
}

/**
 * This function processes an instruction and updates the facility accordingly.
 *
 * @param facility: The current state of the facility.
 * @param instruction: The instruction to be processed. It can be either a Remove or Insert instruction.
 * @returns: The updated state of the facility after processing the instruction.
 *
 * The function works as follows:
 * - If the instruction is Remove(s), it calculates the hash of the string s to get the label,
 * retrieves the array of lenses associated with that label, filters out the lens with the label s,
 * and updates the facility with the new array of lenses.
 * - If the instruction is Insert(s, p), it calculates the hash of the string s to get the label,
 * creates a new lens with label s and power p, retrieves the array of lenses associated with that
 * label, if a lens with label s already exists in the array, it replaces it with the new lens,
 * otherwise, it adds the new lens to the array, and updates the facility with the new array of
 * lenses.
 */
let process: (facility, instruction) => facility = (facility, instruction) => {
  switch instruction {
  | Remove(s) => {
      let label = hash(s)
      let updatedLenses =
        facility
        ->Map.get(label)
        ->Option.mapOr([], lenses => {
          lenses->Array.filter(lens => lens.lensLabel != s)
        })
      facility->Map.set(label, updatedLenses)
      facility
    }
  | Insert(s, p) => {
      let label = hash(s)
      let newLens = {lensLabel: s, lensPower: p}
      let updatedLenses =
        facility
        ->Map.get(label)
        ->Option.mapOr([newLens], lenses => {
          lenses
          ->Array.findIndexOpt(lens => lens.lensLabel == s)
          ->Option.mapOr(Array.concat(lenses, [newLens]), i => {
            lenses->Array.set(i, newLens)
            lenses
          })
        })
      facility->Map.set(label, updatedLenses)
      facility
    }
  }
}

let processAll: array<instruction> => facility = xs => {
  let facility = Map.make()
  xs->Array.reduce(facility, process)
}

/**
 * This function calculates the power of a cell in the facility.
 *
 * @param i: The label of the cell (calculated by hashing the string of the lens).
 * @param lenses: The array of lenses associated with the cell.
 * @returns: The power of the cell. It is calculated by mapping over the array of lenses,
 * multiplying the power of each lens by its index (plus one) and by the label (plus one),
 * and then summing up the results.
 *
 * Example:
 * let result = powerCell((123, [{lensLabel: "example", lensPower: 5}, {lensLabel: "test", lensPower: 3}]))
 * Js.log(result) // Outputs: 1230 (this is a hypothetical output, actual output may vary)
 */
let powerCell: ((int, array<lens>)) => int = ((i, lenses)) => {
  lenses
  ->Array.mapWithIndex((lens, index) => (index + 1) * lens.lensPower * (i + 1))
  ->Array.sum(module(Int))
}

let power: facility => int = facility => {
  facility
  ->Map.entries
  ->Array.fromIterator
  ->Array.map(powerCell)
  ->Array.sum(module(Int))
}

module InstructionParser = {
  module P = ReludeParse.Parser
  open P.Infix

  let debug = P.tapLog

  let nonEmptyListToString = x => x->Relude.NonEmpty.List.toList->List.join(~sep="")

  let mkRemove = a => {
    Remove(a->nonEmptyListToString)
  }

  let removeP = mkRemove->\"<$>"(\"<*"(P.many1(P.anyAlpha), P.str("-")))

  let mkInsert = a => b => {
    Insert(a->nonEmptyListToString, b)
  }
  //  let insertP = P.many1(P.anyAlpha)->\"<*"(P.str("="))->\"<*>"(P.many1(P.anyDigit))
  let insertP = mkInsert->\"<$>"(P.many1(P.anyAlpha)->\"<*"(P.str("=")))->\"<*>"(P.anyInt)

  // <|> (or) may requires first argument to be optional, thus P.tries
  let instructionP = \"<|>"(P.tries(removeP), P.tries(insertP))
  let instructionsP = P.sepBy(P.str(","), instructionP)

  // would stack overflow if the input str is too long (ie: too many "," seperated items)
  // instead, just parse 1 instruction at a time.
  let run = str => {
    instructionP->P.runParser(str, _)->Result.getExn
  }
}

let part2 = data => {
  data
  ->String.split(",")
  ->Array.map(InstructionParser.run)
  ->processAll
  ->power
}

let part1 = xs => {
  xs->Array.map(hash)->Array.sum(module(Int))
}

let parsePart1 = data => data->String.trim->String.split(",")

let solvePart1 = data => {
  data->parsePart1->part1
}

let solvePart2 = data => {
  data->part2
}
