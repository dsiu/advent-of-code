open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2
let log3 = Js.Console.log3

module P = Res_parser
module Rjs = ReScriptJs.Js

module SnailFish = {
  // Type
  //
  type rec pair = (elem, elem)
  and elem =
    | Int(int)
    | Pair(pair)

  let rec dumpElem = e => {
    switch e {
    | Int(i) => i->Int.toString
    | Pair((e1, e2)) => {
        let s1 = e1->dumpElem
        let s2 = e2->dumpElem
        j`[$s1, $s2]`
      }
    }
  }

  module Parser = {
    let charToString = c => c->int_of_char->Js.String.fromCharCode

    let rec concatStringList = chars => {
      switch chars {
      | list{} => ""
      | list{head, ...rest} => head ++ concatStringList(rest)
      }
    }

    let manyWhitespace = P.many(P.anyOf([' ', '\n', '\t', '\r']))
    let comma = P.char(',')

    let zero = P.char('0')
    let oneThroughNine = P.satisfy(c => c >= '1' && '9' >= c)
    let digit = zero->P.orElse(oneThroughNine)->P.map(charToString)
    let digits = P.atLeastOne(digit)->P.map(concatStringList)

    let pair = P.makeRecursive(p => {
      let numberLike = digits

      let betweenBraces = P.between(_, P.char('['), P.char(']'))
      let pairOrNumber = P.choice([
        p,
        numberLike->P.map(x => x->Int.fromString->Option.getExn->Int),
      ])

      pairOrNumber->P.keepLeft(comma)->P.andThen(pairOrNumber)->betweenBraces->P.map(x => Pair(x))
    })

    type result = P.parseResult<elem>
    let parse = (s): result => P.run(pair, s)
  }
}

//let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  open SnailFish
  let l = Parser.parse(data)
  l->Result.isOk->log2("parse result:", _)
  let p = l->Result.getExn->fst
  switch l {
  | Ok(_) =>
    p->dumpElem->log2("Parsed as: ", _)
    l->Result.getExn->snd->log2("Parse state:", _)
    log("\n")
  | Error(err) => {
      log(err)
      log("\n")
    }
  }
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
