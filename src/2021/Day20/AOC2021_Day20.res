//
// https://work.njae.me.uk/2021/12/23/advent-of-code-2021-day-20/
//
open Belt
open Utils
let log = Js.Console.log

module TC = Tablecloth

module Pixel = Coord_V2

type pixelSet = TC.Set.t<Pixel.t, Pixel.identity>

type image = Image({grid: pixelSet, distantPixel: bool, explicitRegion: (Pixel.t, Pixel.t)})

// refactor
let inRange = (range: (Pixel.t, Pixel.t), pixel: Pixel.t): bool => {
  let ((ax, ay), (bx, by)) = range
  let (px, py) = pixel

  ((px <= ax && px >= bx) || (px >= ax && px <= bx)) &&
    ((py <= ay && py >= by) || (py >= ay && py <= by))
}

let findContents = (
  grid: pixelSet,
  distant: bool,
  region: (Pixel.t, Pixel.t),
  here: Pixel.t,
): bool => {
  inRange(region, here) ? grid->TC.Set.includes(here) : distant
}

// refactor
let neighbours = Stdlib.Array.combination2([-1, 0, 1], [-1, 0, 1], (x, y) => (x, y))

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  //  neighbours->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
