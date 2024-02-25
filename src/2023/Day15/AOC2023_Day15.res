open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type instruction = Remove(string) | Insert(string, int)
type lens = {lensLabel: string, lensPower: int}
type facility = Map.t<int, array<lens>>

let charToASCII = s => {
  s->String.codePointAt(0)->Option.getExn
}

let hash = str => {
  str
  ->String.split("")
  ->Array.reduce(0, (acc, c) => {
    //    c->(log2("c = ", _))
    //    c->charToASCII->(log2("ascii = ", _))
    //    (acc + charToASCII(c))->(log2("acc + ascii = ", _))
    ((acc + charToASCII(c)) * 17)->Int.mod(256)
  })
}

let process: (facility, instruction) => facility = (facility, instruction) => {
  switch instruction {
  | Remove(s) => {
      let label = hash(s)
      facility->Map.set(
        label,
        facility
        ->Map.get(label)
        ->Option.mapOr([], lenses => {
          lenses->Array.filter(lens => lens.lensLabel != s)
        }),
      )
      facility
    }
  | Insert(s, p) => {
      let label = hash(s)
      let newLens = {lensLabel: s, lensPower: p}

      facility->Map.set(
        label,
        facility
        ->Map.get(label)
        ->Option.mapOr([newLens], lenses => {
          lenses
          ->Array.findIndexOpt(lens => lens.lensLabel == s)
          ->Option.mapOr(Array.concat(lenses, [newLens]), i => {
            lenses->Array.set(i, newLens)
            lenses
          })
        }),
      )
      facility
    }
  }
}

let processAll: array<instruction> => facility = xs => {
  let facility = Map.make()
  xs->Array.reduce(facility, process)
}

let powerCell: ((int, array<lens>)) => int = ((i, lenses)) => {
  let len = lenses->Array.length

  Array.fromInitializer(~length=len, i => i + 1)
  ->Array.zipWith(lenses->Array.map(lens => lens.lensPower), (a, b) => a * b * (i + 1))
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
  let insts = data->String.split(",")->Array.map(InstructionParser.run)
  let processed = insts->processAll
  processed->power
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
