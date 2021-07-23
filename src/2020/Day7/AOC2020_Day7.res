open Belt
let log = Js.Console.log
open Utils

module Bag = {
  // if count = 0, it is the root bag
  type t = {
    count: int,
    color: string,
  }

  let color = t => t.color
  let count = t => t.count

  let make = (count, color): t => {count: count, color: color}

  let empty: t = {count: 0, color: ""}
}
module Rules = {
  type t = Map.String.t<array<Bag.t>>

  let numBagRe = %re("/\D*(\d+)\s+([\w\s]+)\s+bag[s]*.*/i")
  let justBagRe = %re("/([\w\s]+)\s+bag[s]*.*/i")

  let parseBag = (s, r, ~numIndex, ~bagIndex): Bag.t => {
    switch s->Js.String2.includes("no other bags") {
    | true => Bag.empty
    | false => {
        let c = switch s->Js.Re.exec_(r, _) {
        | Some(y) => y->Js.Re.captures->Array.map(z => z->Js.Nullable.toOption->Option.getExn)
        | None => []
        }
        Bag.make(
          numIndex == 0 ? 0 : c->Array.get(numIndex)->Option.getExn->Int.fromString->Option.getExn,
          c->Array.get(bagIndex)->Option.getExn,
        )
      }
    }
  }

  let parseNumBag = parseBag(_, numBagRe, ~numIndex=1, ~bagIndex=2)
  let parseJustBag = parseBag(_, justBagRe, ~numIndex=0, ~bagIndex=1)

  let nodeRe = %re("/(.*)\s+bags/i")

  let parseNode = s => {
    switch s->Js.Re.exec_(nodeRe, _) {
    | Some(x) => x->Js.Re.captures->Array.get(0)->Option.getExn
    | None => raise(Not_found)
    }
  }

  let leafRe = %re("/(.*)\s+bags/i")

  let parseLeaf = s => {
    switch s->Js.Re.exec_(leafRe, _) {
    | Some(x) => x->Js.Re.captures->Array.get(0)->Option.getExn
    | None => raise(Not_found)
    }
  }

  let addNode = (t, node, leaf): t => {
    t->Map.String.set(node->Bag.color, leaf)
  }

  let addRule = (t, l): t => {
    let node = l[0]->Option.getExn->parseJustBag
    let leaf = l[1]->Option.getExn->Js.String2.split(_, ",")->Array.map(parseNumBag)
    //    node->log
    //    leaf->log
    t->addNode(node, leaf)
  }

  let make = Map.String.empty
}

let parseLine = l =>
  l->Js.String2.trim->Js.String2.splitAtMost(_, "contain", ~limit=2)->Array.map(Js.String2.trim)

let parse = data => data->splitNewline->Array.map(parseLine)

let solvePart1 = data => {
  let rules = Rules.make
  let parsed = data->parse
  let newRules = parsed->Array.reduce(rules, (a, x) => {Rules.addRule(a, x)})
  //  newRules->Map.String.get("light red")->log
  newRules->Map.String.forEach((k, v) => {
    k->log
    v->log
  })
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
