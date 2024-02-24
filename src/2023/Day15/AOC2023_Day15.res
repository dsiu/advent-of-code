open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type instruction = Remove(string) | Insert(string, int)
type lens = {lensLabel: string, lensPower: int}
type facility = Map.t<int, lens>

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

let part1 = xs => {
  xs->Array.map(hash)->Array.sum(module(Int))
}

let parsePart1 = data => data->String.trim->String.split(",")

let solvePart1 = data => {
  let d = data->parsePart1
  d->log
  d->part1
}

let solvePart2 = data => {
  data->String.split(",")->Array.map(InstructionParser.run)->log
  2
}
