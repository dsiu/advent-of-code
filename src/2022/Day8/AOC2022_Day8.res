open Stdlib
open Utils
let log = Js.Console.log

module A = Array
module O = Option

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

  row->A.reduce((-1, []), vis)->snd->A.toReversed
}

let setVisibilityOrient: forest => forest = A.map(_, setVisibility)

let setVisibilityForest: forest => forest = forest => {
  let rotate = x => x->transpose->A.map(A.toReversed)
  let f = compose(setVisibilityOrient, rotate)
  forest->f->f->f->f
}

let countVisible: forest => int = forest => {
  forest->A.concatMany([], _)->A.filter(isVisible)->A.length
}

let part1: forest => int = compose(setVisibilityForest, countVisible)

let tracks: (forest, int, int) => forest = (forest, row, col) => {
  let (l, r) = forest->A.getUnsafe(row)->A.splitAt(_, ~index=col)
  let (u, d) = forest->transpose->A.getUnsafe(col)->A.splitAt(_, ~index=row)

  [l->A.toReversed, r->A.drop(1), u->A.toReversed, d->A.drop(1)]
}

// takeWhile1 stops once the predicate is false
let rec takeWhile1 = (xs, f) => {
  switch xs {
  | [] => []
  | _ => {
      let (h, t) = (xs->A.headUnsafe, xs->A.tail)
      switch f(h) {
      | true => A.concat([h], takeWhile1(t, f))
      | _ => [h]
      }
    }
  }
}

let viewDistance: (array<tree>, int) => int = (trees, h) => {
  trees->A.map(treeHeight)->takeWhile1(x => x < h)->A.length
}

let scenicScore: (forest, int, int) => int = (forest, row, col) => {
  let directions = tracks(forest, row, col)
  let h = forest->A.getUnsafe(row)->A.getUnsafe(col)->treeHeight

  directions->A.map(viewDistance(_, h))->A.reduce(1, (a, x) => a * x)
}

let part2: forest => int = forest => {
  let id = Function.identity
  let nrows = forest->A.length
  let ncols = forest->A.headUnsafe->A.length
  let scores = A.combination2(A.makeBy(nrows - 1, id), A.makeBy(ncols - 1, id), (r, c) =>
    scenicScore(forest, r, c)
  )
  scores->maxIntInArray
}

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
  data->parse->part2
}
