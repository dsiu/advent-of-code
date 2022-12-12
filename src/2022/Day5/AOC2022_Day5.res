open Stdlib
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

module M = Belt.Map.Int

type crate = Crate(string)
type wharf = M.t<array<crate>>
type move = Move(int, int, int) // quantity, from, to
let extractName: crate => string = (Crate(c)) => c

let getCratesForWharf = (crates, wharf) => {
  let idx = (wharf - 1) * 4 + 1
  crates->Array.map(x => {x->Array.get(idx)->Option.flatMap(x => x == " " ? None : Some(Crate(x)))})
}

let catMaybes = xs => {
  xs->Array.keepMap(Function.identity)
}

let makeWharf = (wharfLines, colNames) => {
  colNames->Array.reduce(M.empty, (acc, colName) => {
    acc->M.set(colName, getCratesForWharf(wharfLines, colName)->catMaybes)
  })
}

let makeMoves = xs => {
  module O = Option
  xs->Array.map(x => {
    let parts = x->String.split(" ")
    Move(
      parts->Array.get(1)->O.flatMap(x => Belt.Int.fromString(x))->O.getExn,
      parts->Array.get(3)->O.flatMap(x => Belt.Int.fromString(x))->O.getExn,
      parts->Array.get(5)->O.flatMap(x => Belt.Int.fromString(x))->O.getExn,
    )
  })
}

let parse = data => {
  open Array
  open String

  let text = data->splitDoubleNewline->map(x => x->splitNewline)
  let wharfLines = text->getExn(0)->tail->init->Option.getExn->map(splitChars)
  let colNames = text->getExn(0)->tail->last->split(" ")->keepMap(Belt.Int.fromString)

  let moves = text->getExn(1)->init->Option.getExn->makeMoves

  let wharf = makeWharf(wharfLines, colNames)
  (wharf, moves)
}

let makeMove1 = (wharf, Move(_, from, to_)) => {
  open Array
  let f = wharf->M.getExn(from)
  let c = f->head
  let origin = f->tail
  let dest = append([c], wharf->M.getExn(to_))

  wharf->M.set(to_, dest)->M.set(from, origin)
}

let applyMove1 = (wharf, Move(n, _, _) as m) => {
  open Array
  Array.makeBy(n, _ => m)->reduce(wharf, (a, x) => makeMove1(a, x))
}

let applyMoves1 = (wharf, moves) => {
  open Array
  moves->reduce(wharf, (a, x) => applyMove1(a, x))
}

let showTops = wharf => {
  open Array
  wharf->M.valuesToArray->Array.map(x => x->Array.head->extractName)->foldLeft((a, x) => a ++ x)
}

let solvePart1 = data => {
  let (wharf, moves) = data->parse
  //  wharf->log
  applyMoves1(wharf, moves)->showTops
}

let solvePart2 = data => {
  data->ignore
  2
}
