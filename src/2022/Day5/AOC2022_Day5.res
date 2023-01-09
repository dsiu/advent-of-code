open Stdlib
open Utils

module A = Array
module S = String
module O = Option
module M = Belt.Map.Int

let log2 = Js.Console.log2
let log = Js.Console.log

type crate = Crate(string)
type wharf = M.t<array<crate>>
type move = Move(int, int, int) // quantity, from, to
let extractName: crate => string = (Crate(c)) => c

let getCratesForWharf = (crates, wharf) => {
  let idx = (wharf - 1) * 4 + 1
  crates->A.map(x => {
    x->A.get(idx)->O.flatMap(x => x == " " ? None : Some(Crate(x)))
  })
}

let catMaybes = Array.keepMap(_, Function.identity)

let makeWharf = (wharfLines, colNames) => {
  colNames->A.reduce(M.empty, (acc, colName) => {
    acc->M.set(colName, getCratesForWharf(wharfLines, colName)->catMaybes)
  })
}

let makeMoves = xs => {
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
  let text = data->splitDoubleNewline->A.map(splitNewline)
  let firstSection = text->A.getExn(0)->A.tail // drop first empty line
  let secondSection = text->A.getExn(1)->A.init // drop last empty line

  let wharfLines = firstSection->A.init->O.getExn->A.map(splitChars)
  let colNames = firstSection->A.last->S.split(" ")->A.keepMap(Belt.Int.fromString)
  let moves = secondSection->O.getExn->makeMoves

  let wharf = makeWharf(wharfLines, colNames)
  (wharf, moves)
}

let makeMove1 = (wharf, Move(_, from, to_)) => {
  let f = wharf->M.getExn(from)
  let c = f->A.head
  let origin = f->A.tail
  let dest = A.append([c], wharf->M.getExn(to_))

  wharf->M.set(to_, dest)->M.set(from, origin)
}

let applyMove1 = (wharf, Move(n, _, _) as m) => {
  A.makeBy(n, _ => m)->A.reduce(wharf, makeMove1)
}

let applyMoves1 = (wharf, moves) => {
  moves->A.reduce(wharf, applyMove1)
}

let applyMove2 = (wharf, Move(n, from, to_)) => {
  let origin = wharf->M.getExn(from)
  let moving = origin->A.take(n)
  let origin' = origin->A.drop(n)
  let dest = A.append(moving, wharf->M.getExn(to_))

  wharf->M.set(to_, dest)->M.set(from, origin')
}

let applyMoves2 = (wharf, moves) => {
  moves->A.reduce(wharf, applyMove2)
}

let showTops = wharf => {
  wharf->M.valuesToArray->A.map(compose(A.head, extractName))->A.foldLeft(String.concat)
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
