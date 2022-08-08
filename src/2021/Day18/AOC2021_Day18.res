open Belt
//open Utils
let log = Js.Console.log
let log2 = Js.Console.log2
let log3 = Js.Console.log3

module P = Res_parser
module Rjs = ReScriptJs.Js

module Tree = {
  exception Not_Expected(string)
  // Type
  //
  type rec tree<'a> =
    | Leaf('a)
    | Pair(tree<'a>, tree<'a>)

  // show tree
  //
  let rec dumpTree = e => {
    switch e {
    | Leaf(i) => i->Int.toString
    | Pair((e1, e2)) => {
        let s1 = e1->dumpTree
        let s2 = e2->dumpTree
        j`[$s1, $s2]`
      }
    }
  }

  // zipper
  // https://wiki.haskell.org/Zipper
  //
  type rec cxt<'a> =
    | Top
    | L(cxt<'a>, tree<'a>)
    | R(tree<'a>, cxt<'a>)

  let rec dumpCxt = c => {
    switch c {
    | Top => "Top"
    | L(c, t) => {
        let t_str = t->dumpTree
        let c_str = c->dumpCxt
        j`L ($c_str, $t_str)`
      }

    | R(t, c) => {
        let t_str = t->dumpTree
        let c_str = c->dumpCxt
        j`R ($t_str, $c_str)`
      }
    }
  }

  type loc<'a> = (tree<'a>, cxt<'a>)

  let dumpLoc = ((t, c)) => {
    let t_str = t->dumpTree
    let c_str = c->dumpCxt
    j`Loc[ cxt = $c_str\n      tree = $t_str]`
  }

  type left_<'a> = loc<'a> => loc<'a>
  let left: left_<'a> = ((t, c)) => {
    switch t {
    | Pair(l, r) => (l, L(c, r))
    | _ => raise(Not_Expected("left: not a Pair"))
    }
  }

  type right_<'a> = loc<'a> => loc<'a>
  let right: right_<'a> = ((t, c)) => {
    switch t {
    | Pair(l, r) => (r, R(l, c))
    | _ => raise(Not_Expected("right: not a Pair"))
    }
  }

  type top_<'a> = tree<'a> => loc<'a>
  let top: top_<'a> = t => (t, Top)

  type up_<'a> = loc<'a> => loc<'a>
  let up: up_<'a> = ((t, c)) => {
    switch c {
    | L(c, r) => (Pair(t, r), c)
    | R(l, c) => (Pair(l, t), c)
    | Top => raise(Not_Expected("up: not a L or R"))
    }
  }

  type upmost_<'a> = loc<'a> => loc<'a>
  let rec upmost: upmost_<'a> = ((_, c) as l) => {
    switch c {
    | Top => l
    | _ => upmost(up(l))
    }
  }

  type modify_<'a> = (loc<'a>, tree<'a> => tree<'a>) => loc<'a>
  let modify: modify_<'a> = ((t, c), f) => (t->f, c)

  let () = {
    let t = {
      let tl = Pair(Leaf(1), Leaf(2))
      let tr = Pair(Leaf(3), Leaf(4))
      Pair(tl, tr)
    }

    // Then to reach the location of Leaf 2:
    t->top->left->right->dumpLoc->log2("trying to reach Leaf 2\n", _)
    t->top->left->right->modify(_ => Leaf(0))->dumpLoc->log2("modifying Leaf 2\n", _)

    t->top->left->right->modify(_ => Leaf(0))->upmost->fst->dumpTree->log2("upmost\n", _)
  }
}

module SnailFish = {
  // type
  //
  type t = Tree.tree<int>

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
    p->Tree.dumpTree->log2("Parsed as: ", _)
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
