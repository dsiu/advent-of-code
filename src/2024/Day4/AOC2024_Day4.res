open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type grid = Array2D.t<string>
type position = Coord_V2.t // (x,y) (col, row)

let pointExtensions: (position, int) => array<array<position>> = (startPos, steps) => {
  open Coordinate.StepFunctions

  let directions = [stepNW, stepN, stepNE, stepW, stepE, stepSW, stepS, stepSE]

  directions->Array.map(f => {
    let rec loop = (acc, cur, i) => {
      let next = f(cur)
      i >= steps - 1 ? acc : loop([...acc, next], next, i + 1)
    }
    loop([startPos], startPos, 0)
  })
}
// assuming c is center.  the 4 directions are NE, NW, SE, SW
let xExtensions: position => array<array<position>> = c => {
  open Coordinate.StepFunctions

  [[c, stepNW(c), stepNE(c), stepSW(c), stepSE(c)]]
}

let potentialWords: (position => array<array<position>>, grid) => array<array<position>> = (
  exts,
  grid,
) => {
  grid->Array2D.reduceWithIndex([], (acc, _, pos) => {
    [...acc, ...exts(pos)]
  })
}

let validWords: (array<array<position>>, grid) => array<array<position>> = (words, grid) => {
  let validWord = word => word->Array.every(pos => grid->Array2D.isValidXY(pos))
  words->Array.filter(validWord)
}

let foundWords: (array<array<position>>, grid) => array<string> = (words, grid) => {
  let getWordFromGrid = word => word->Array.map(Array2D.getExn(grid, _))->Array.join("")
  words->Array.map(getWordFromGrid)
}

// for the X-MAS cross case. A is the center. match with all combinations of Xs and Ms
let isXmas: string => bool = word => {
  switch word {
  | "AMMSS"
  | "ASMSM"
  | "AMSMS"
  | "ASSMM" => true
  | _ => false
  }
}

let part1: (grid, string) => int = (grid, word) => {
  let len = word->String.length

  (pointExtensions(_, len))
  ->potentialWords(grid)
  ->validWords(grid)
  ->foundWords(grid)
  ->Array.filter(w => w == word)
  ->Array.length
}

let part2: grid => int = grid => {
  (xExtensions(_))
  ->potentialWords(grid)
  ->validWords(grid)
  ->foundWords(grid)
  ->Array.filter(isXmas)
  ->Array.length
}

let parse: string => grid = data =>
  data->splitNewline->Array.map(x => x->String.trim->String.split(""))

let solvePart1 = data => {
  data->parse->part1("XMAS")
}

let solvePart2 = data => {
  data->parse->part2
}
