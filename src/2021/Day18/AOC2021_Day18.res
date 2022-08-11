open Belt
//open Utils
let log = Js.Console.log
let log2 = Js.Console.log2
let log3 = Js.Console.log3

module P = Res_parser
module Rjs = ReScriptJs.Js

module SnailFish = {
  // type
  //
  type t = Tree.tree<int>

  let splittable = t => {
    open Tree
    let rec splittableC = loc => {
      switch loc {
      | Loc(Leaf(n), _) => n >= 10 ? Some(loc) : None
      | Loc(Pair(_, _), _) => splittableC(left(loc))->Option.flatMap(_ => splittableC(right(loc)))
      }
    }
    splittableC(top(t))
  }

  let split = num => {
    open Tree
    let mn0 = splittable(num)

    switch mn0 {
    | None => None
    | Some(_) => {
        let n0 = Option.getExn(mn0)
        let Loc(Leaf(sn), _) = n0
        let ln = sn / 2
        let rn = ln + mod(sn, 2)
        let n1 = modify(n0, _ => {Pair(Leaf(ln), Leaf(rn))})
        let Loc(num1, _) = upmost(n1)
        num1->Some
      }
    }
  }

  let pairAtDepth = (n, t) => {
    open Tree
    let rec pairAtDepthC = (n, l) => {
      switch (n, l) {
      | (_, Loc(Leaf(_), _)) => None
      | (0, Loc(Pair(_, _), _)) => Some(l)
      | (n, Loc(Pair(_, _), _)) =>
        pairAtDepthC(n - 1, left(l))->Option.flatMap(_ => pairAtDepthC(n - 1, right(l)))
      }
    }
    pairAtDepthC(n, top(t))
  }

  let rec rightmostNum = loc => {
    open Tree
    switch loc {
    | Loc(Leaf(_), _) => loc
    | Loc(Pair(_, _), _) => loc->right->rightmostNum
    }
  }

  let rec rightmostOnLeft = loc => {
    open Tree
    switch loc {
    | Loc(_, Top) => None
    | Loc(_, L(_, _)) => loc->up->rightmostOnLeft
    | Loc(_, R(_, _)) => loc->up->rightmostNum->Some
    }
  }

  // simple parser for elements
  //
  module Parser = {
    let charToString = c => c->int_of_char->Js.String.fromCharCode

    let rec concatStringList = chars => {
      switch chars {
      | list{} => ""
      | list{head, ...rest} => head ++ concatStringList(rest)
      }
    }

    let comma = P.char(',')

    let zero = P.char('0')
    let oneThroughNine = P.satisfy(c => c >= '1' && '9' >= c)
    let digit = zero->P.orElse(oneThroughNine)->P.map(charToString)
    let digits = P.atLeastOne(digit)->P.map(concatStringList)

    let pair = P.makeRecursive(p => {
      let numberLike = digits

      let betweenBraces = P.between(_, P.char('['), P.char(']'))
      let makeIntElem = x => Tree.Leaf(x->Int.fromString->Option.getExn)
      let pairOrNumber = P.choice([p, numberLike->P.map(makeIntElem)])

      pairOrNumber
      ->P.keepLeft(comma)
      ->P.andThen(pairOrNumber)
      ->betweenBraces
      ->P.map(((l, r)) => Tree.Pair(l, r))
    })

    type result = P.parseResult<t>
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
    p->Tree.treeToString->log2("Parsed as: ", _)
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
