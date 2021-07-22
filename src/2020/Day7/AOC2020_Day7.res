open Belt
let log = Js.Console.log
open Utils

module Rules = {
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

  let addRule = l => {
    let node = l[0]->Option.getExn->parseNode
    let leaf = l[1]->Option.getExn->parseLeaf
    node->log
    leaf->log
    (node, leaf)
  }
}

let parseLine = l =>
  l
  ->Js.String2.trim
  ->Js.String2.splitAtMost(_, "contain", ~limit=2)
  ->Array.map(Js.String2.trim)
  ->Rules.addRule

let parse = data => data->splitNewline->Array.map(parseLine)

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
