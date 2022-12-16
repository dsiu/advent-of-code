open Stdlib
open Utils
let log = Js.Console.log

module A = Array

type tree = Tree(int, bool) // height, isVisible
type forest = array<array<tree>>

let isVisible: tree => bool = (Tree(_, v)) => v
let treeHeight: tree => int = (Tree(h, _)) => h

let setVisibility: array<tree> => array<tree> = row => {
  let vis = ((highest, tagged), Tree(height, visible)) => {
    height > highest
      ? (height, A.concat([Tree(height, true)], tagged))
      : (highest, A.concat([Tree(height, visible)], tagged))
  }

  row->A.reduce((-1, []), vis)->snd->A.reverse
}
let setVisibilityOrient: forest => forest = A.map(_, setVisibility)

let setVisibilityForest: forest => forest = forest => {
  let rotate = x => x->transpose->A.map(A.reverse)
  let f = x => x->setVisibilityOrient->rotate
  forest->f->f->f->f
}

let countVisible: forest => int = forest => {
  forest->A.concatMany->A.keep(isVisible)->A.length
}

let part1: forest => int = compose(setVisibilityForest, countVisible)

let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim->splitChars->A.map(x => Tree(x->int_of_string, false))
  })
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->ignore
  2
}
