@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type direction = L | R
type node = Node(string, string)
type desert = Stdlib.Map.t<string, node>

module ProblemParser = {
  module P = ReludeParse.Parser
  open P.Infix
  let justSpace: P.t<unit> = P.void(P.many(P.str(" ")))
  let debug = P.tapLog

  let mkNode = (a, b) => {
    Node(a, b)
  }

  let mkDesertLine = (a, b) => {
    (a, b)
  }

  let nameP = P.\"<#>"(P.many(P.anyAlphaOrDigit), l => l->List.toArray->Array.joinWith(""))

  let nodeP =
    mkNode->\"<$>"(P.str("(")->\"*>"(nameP)->\"<*"(P.str(", ")))->\"<*>"(nameP->\"<*"(P.str(")")))

  let desertLineP =
    mkDesertLine->\"<$>"(justSpace->\"*>"(nameP)->\"<*"(P.str(" = ")))->\"<*>"(nodeP)

  let mkDesert = a => {
    Stdlib.Map.fromArray(a->List.toArray)
  }

  let desertP = mkDesert->\"<$>"(P.sepBy(P.eol, desertLineP))

  let directionP = L->\"<$"(P.str("L"))->\"<|>"(R->\"<$"(P.str("R")))

  let mkProblem: (list<direction>, desert) => (array<direction>, desert) = (
    a: list<direction>,
    b: desert,
  ) => {
    (a->List.toArray, b)
  }

  let problemP = mkProblem->\"<$>"(P.many(directionP)->\"<*"(P.many1(P.eol)))->\"<*>"(desertP)

  let run: string => (array<direction>, desert) = data => {
    //    P.runParser("(AAA, BBB)", nodeP)->log
    //    P.runParser("CCC = (DDD, EEE) ", desertLineP)->log
    //    P.runParser(`C11 = (D22, E33)\nF4F = (G5G, 6HH)`, desertP)->log
    //    P.runParser(`LRRLRR`, dLineP)->log
    data->P.runParser(problemP)->Result.getExn
  }
}

let solvePart1 = data => {
  //  data->parse->log
  let (a, b) = data->ProblemParser.run
  a->log
  b->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
