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
  open Array
  let idx = (wharf - 1) * 4 + 1
  crates->map(x => {
    x->get(idx)->Option.flatMap(x => x == " " ? None : Some(Crate(x)))
  })
}

let catMaybes = Array.keepMap(_, Function.identity)

let makeWharf = (wharfLines, colNames) => {
  colNames->Array.reduce(M.empty, (acc, colName) => {
    acc->M.set(colName, getCratesForWharf(wharfLines, colName)->catMaybes)
  })
}

let makeMoves = xs => {
  module O = Option
  let get = Array.get
  let map = Array.map
  let intFromString = Belt.Int.fromString

  xs->map(x => {
    let parts = x->String.split(" ")
    Move(
      parts->get(1)->O.flatMap(intFromString)->O.getExn,
      parts->get(3)->O.flatMap(intFromString)->O.getExn,
      parts->get(5)->O.flatMap(intFromString)->O.getExn,
    )
  })
}

let parse = data => {
  open Array
  open String

  let text = data->splitDoubleNewline->map(splitNewline)
  let firstSection = text->getExn(0)->tail // drop first empty line
  let secondSection = text->getExn(1)->init // drop last empty line

  let wharfLines = firstSection->init->Option.getExn->map(splitChars)
  let colNames = firstSection->last->split(" ")->keepMap(Belt.Int.fromString)
  let moves = secondSection->Option.getExn->makeMoves

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
  makeBy(n, _ => m)->reduce(wharf, makeMove1)
}

let applyMoves1 = (wharf, moves) => {
  open Array
  moves->reduce(wharf, applyMove1)
}

let applyMove2 = (wharf, Move(n, from, to_)) => {
  open Array
  let origin = wharf->M.getExn(from)
  let moving = origin->take(n)
  let origin' = origin->drop(n)
  let dest = append(moving, wharf->M.getExn(to_))

  wharf->M.set(to_, dest)->M.set(from, origin')
}

let applyMoves2 = (wharf, moves) => {
  open Array
  moves->reduce(wharf, applyMove2)
}

let showTops = wharf => {
  open Array
  wharf->M.valuesToArray->map(compose(head, extractName))->foldLeft(String.concat)
}

let solvePart1 = data => {
  let (wharf, moves) = data->parse
  //  wharf->log
  applyMoves1(wharf, moves)->showTops
}

let solvePart2 = data => {
  let (wharf, moves) = data->parse
  applyMoves2(wharf, moves)->showTops
}
