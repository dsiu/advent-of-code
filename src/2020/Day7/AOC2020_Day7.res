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

  let isEmpty = t => t.count == 0 && t.color == ""

  let colorEq = (t, color) => t.color == color
  let make = (count, color): t => {count: count, color: color}

  let empty: t = {count: 0, color: ""}
}

module Rules = {
  type t = Map.String.t<array<Bag.t>>
  let set = Map.String.set
  let get = Map.String.get
  let forEach = Map.String.forEach
  let reduce = Map.String.reduce
  let map = Map.String.map
  let mapWithKey = Map.String.mapWithKey

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
    t->set(node->Bag.color, leaf)
  }

  let addRule = (t, l): t => {
    let node = l[0]->Option.getExn->parseJustBag
    let leaf = l[1]->Option.getExn->Js.String2.split(_, ",")->Array.map(parseNumBag)
    //    node->log
    //    leaf->log
    t->addNode(node, leaf)
  }

  let getBag = (t, b) => t->get(b->Bag.color)

  let rec doesThisBagContain = (t, srcColor, match) => {
    let leaf = t->get(srcColor)->Option.getExn
    leaf->Array.reduce([], (a, x) => {
      switch x->Bag.isEmpty {
      | true => a
      | false =>
        switch x->Bag.colorEq(match->Bag.color) {
        | true => a->Array.concat([srcColor])
        | false => a->Array.concat(t->doesThisBagContain(x->Bag.color, match))
        }
      }
    })
  }

  // part 1
  let whichBagContains = (t, match) => {
    t->reduce([], (a, k, v) => {
      v->ignore // value isn't used
      let ret = t->doesThisBagContain(k, match)
      switch ret->Array.size > 0 {
      | true => a->Array.concat([k])
      | false => a
      }
    })
  }

  let rec countBagsInside = (t, bag) => {
    let leaf = t->getBag(bag)->Option.getExn
    leaf->Array.reduce(1, (a, x) => {
      switch x->Bag.isEmpty {
      | true => a
      | false => a + x->Bag.count * t->countBagsInside(x)
      }
    })
  }
  // part 2
  let howManyBagsIn = (t, match) => {
    t->countBagsInside(match) - 1
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
  //  newRules->Rules.forEach((k, v) => {
  //    k->log
  //    v->log
  //  })

  //  "result"->log
  let result = newRules->Rules.whichBagContains(Bag.make(0, "shiny gold"))
  //  result->log

  result->Array.size
}

let solvePart2 = data => {
  let rules = Rules.make
  let parsed = data->parse
  let newRules = parsed->Array.reduce(rules, (a, x) => {Rules.addRule(a, x)})
  //  "result"->log
  let result = newRules->Rules.howManyBagsIn(Bag.make(0, "shiny gold"))
  result
}
