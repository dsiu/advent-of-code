open Belt
open! FP_Utils
open Utils

let log = Js.Console.log
let log2 = Js.Console.log2
let log3 = Js.Console.log3

module P = Res_parser
module Rjs = ReScriptJs.Js
open Tree

@@warning("-8")
module SnailFish = {
  // type
  //
  type tree = Tree.tree<int>
  type loc = Tree.loc<int>

  //
  // https://work.njae.me.uk/2021/12/21/advent-of-code-2021-day-18/
  //

  // First is to find a splittable element. This is a function from a Tree to a Maybe Loc,
  // returning Nothing if there are no splittable numbers. splitable wraps the tree in a context
  // then recurses into it with splittableC.
  //
  //If that's given a Pair, it looks for splittable numbers in the branches, exploiting that Maybe
  // is Applicative so <|> handles the alternatives nicely.
  //
  type splittable = tree => option<loc>
  let splittable: splittable = t => {
    let rec splittableC = loc => {
      switch loc {
      | Loc(Leaf(n), _) => n >= 10 ? Some(loc) : None
      | Loc(Pair(_, _), _) => splittableC(left(loc))->optionOr(splittableC(right(loc)))
      }
    }
    splittableC(top(t))
  }

  // Given the ability to find splittable numbers, split will split the leftmost one. If there is
  // no splittable number, it returns Nothing.  If there is a splittable number, it focuses on
  // that number, replaces it with the pair, then returns focus to the root of the tree.
  //
  type split = tree => option<tree>
  let split: split = num => {
    let mn0 = splittable(num)

    switch mn0 {
    | None => None
    | Some(n0) => {
        //        let n0 = Option.getExn(mn0)
        let Loc(Leaf(sn), _) = n0
        let ln = sn / 2
        let rn = ln + mod(sn, 2)
        let n1 = modify(n0, _ => {Pair(Leaf(ln), Leaf(rn))})
        let Loc(num1, _) = upmost(n1)
        num1->Some
      }
    }
  }

  // Finding the pair to split is much the same as above, but using a counter to track now many
  // layers deep to find the pair.
  //
  type pairAtDepthC = (int, loc) => option<loc>
  let rec pairAtDepthC: pairAtDepthC = (n, l) => {
    //    log2("pairAtDepthC at n = ", n)
    switch (n, l) {
    | (_, Loc(Leaf(_), _)) => None
    | (0, Loc(Pair(_, _), _)) =>
      //        "at depth 0"->log
      //        l->locToString->log
      Some(l)
    | (n, Loc(Pair(_, _), _)) =>
      // pairAtDepthC(n - 1, left(l))->Option.flatMap(_ => pairAtDepthC(n - 1, right(l)))
      pairAtDepthC(n - 1, left(l))->optionOr(pairAtDepthC(n - 1, right(l)))
    }
  }

  type pairAtDepth = (int, tree) => option<loc>
  let pairAtDepth: pairAtDepth = (n, t) => {
    pairAtDepthC(n, top(t))
  }

  // Given a pair that explodes, I need to find the rightmost leaf that's to the left of this pair.
  // I find that by going back up the tree until I find where I took the right branch at a pair.
  // I then take the left branch of that pair and follow it down, always taking the right branch.
  // If I get to the root of the tree without finding branch to the left, I return Nothing.
  //
  type rightmostNum = loc => loc
  let rec rightmostNum: rightmostNum = loc => {
    switch loc {
    | Loc(Leaf(_), _) => loc
    | Loc(Pair(_, _), _) => loc->right->rightmostNum
    }
  }

  type rightmostOnLeft = loc => option<loc>
  let rec rightmostOnLeft: rightmostOnLeft = loc => {
    switch loc {
    | Loc(_, Top) => None
    | Loc(_, L(_, _)) => loc->up->rightmostOnLeft
    | Loc(_, R(_, _)) => loc->up->left->rightmostNum->Some
    }
  }

  type leftmostNum = loc => loc
  let rec leftmostNum: leftmostNum = loc => {
    switch loc {
    | Loc(Leaf(_), _) => loc
    | Loc(Pair(_, _), _) => loc->left->leftmostNum
    }
  }

  type leftmostOnRight = loc => option<loc>
  let rec leftmostOnRight: leftmostOnRight = loc => {
    switch loc {
    | Loc(_, Top) => None
    | Loc(_, R(_, _)) => loc->up->leftmostOnRight
    | Loc(_, L(_, _)) => loc->up->right->leftmostNum->Some
    }
  }

  type explode = tree => option<tree>
  let explode: explode = num => {
    //    log("explode")
    let mp0 = pairAtDepth(4, num)
    //    log2("mp0 = ", mp0)
    switch mp0 {
    | None => None
    | Some(p0) => {
        //        log("--> got depth 4")
        //        let p0 = Option.getExn(mp0)

        let Loc(Pair(Leaf(nl), Leaf(nr)), _) = p0

        let p1 = switch rightmostOnLeft(p0) {
        | None => p0
        | Some(leftReg) => modify(leftReg, (Leaf(n)) => {Leaf(n + nl)})
        }

        let p2 = switch pairAtDepthC(4, p1->upmost)->Option.flatMap(leftmostOnRight) {
        | None => p1
        | Some(rightReg) => modify(rightReg, (Leaf(n)) => {Leaf(n + nr)})
        }

        let p3 = switch pairAtDepthC(4, p2->upmost) {
        | None => p2
        | Some(centrePair) => modify(centrePair, _ => {Leaf(0)})
        }

        let Loc(num1, _) = p3->upmost
        Some(num1)
      }
    }
  }

  type reduce = tree => tree
  let rec reduce: reduce = num => {
    switch explode(num)->optionOr(split(num)) {
    | None => num
    | Some(num1) => reduce(num1)
    }
  }

  type snailAdd = (tree, tree) => tree
  let snailAdd: snailAdd = (a, b) => {
    //    log("snailAdd:")
    //    a->treeToString->log2("a: ", _)
    //    b->treeToString->log2("b: ", _)
    Pair(a, b)->reduce
  }

  let total = xs => xs->foldLeftArray(snailAdd)

  type magnitude = tree => int
  let rec magnitude: magnitude = t => {
    switch t {
    | Leaf(n) => n
    | Pair(a, b) => 3 * magnitude(a) + 2 * magnitude(b)
    }
  }

  let part1 = numbers => {
    let total = numbers->total
    //    total->Tree.treeToString->log2("result:", _)
    total->magnitude
  }

  let part2 = numbers => {
    combinationArray2(numbers, numbers, (a, b) => snailAdd(a, b)->magnitude)->maxIntInArray
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
      let makeIntElem = x => Tree.Leaf(x->intFromStringExn)
      let pairOrNumber = P.choice([p, numberLike->P.map(makeIntElem)])

      pairOrNumber
      ->P.keepLeft(comma)
      ->P.andThen(pairOrNumber)
      ->betweenBraces
      ->P.map(((l, r)) => Tree.Pair(l, r))
    })

    type result = P.parseResult<tree>
    let parse = (s): result => P.run(pair, s)
    let parseAndGetResult = s => {
      s->parse->Result.getExn->fst
    }
  }

  let makeParseTree = x => {
    //  l->Result.isOk->log2("parse result:", _)

    let p = Parser.parseAndGetResult(x)

    //  switch l {
    //  | Ok(_) =>
    //    p->Tree.treeToString->log2("Parsed as: ", _)
    //    l->Result.getExn->snd->log2("Parse state:", _)
    //    log("\n")
    //  | Error(err) => {
    //      log(err)
    //      log("\n")
    //    }
    //  }

    p
  }
}

open SnailFish

let parse = data => {
  data->splitNewline->Array.map(x => x->Js.String2.trim)->Array.map(makeParseTree)
}

let solvePart1 = data => {
  let lines = parse(data)
  part1(lines)
}

let solvePart2 = data => {
  let lines = parse(data)
  part2(lines)
}
