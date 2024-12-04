@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

open! BigInt

// todo: refactor
module Coord_V2_Big = {
  type t = Tuple2.t<bigint, bigint>

  // int compare
  let bigIntCompareOrd = (a: bigint, b: bigint) =>
    a < b ? Ordering.less : a > b ? Ordering.greater : Ordering.equal

  // int compare
  let bigIntCompare = (a, b) => bigIntCompareOrd(a, b)->Ordering.toInt
  let intCompare = (a, b) => Int.compare(a, b)->Ordering.toInt
  let compare = Tuple2.compare(~f=bigIntCompare, ~g=bigIntCompare, ...)

  include Tablecloth.Comparator.Make({
    type t = t
    let compare = compare
  })

  let add = ((a, b), (a', b')) => (a + a', b + b')
  let mul = ((a, b), x) => (a * x, b * x)
}

type position = Position(Coord_V2_Big.t)

type galaxies = Galaxies(Set.t<position>)

let dumpGalaxies = (Galaxies(galaxies)) => {
  galaxies->Set.forEach((Position(p)) => log(p))
}

let maxCoord = (Galaxies(galaxies)) => {
  let origin = (0n, 0n)

  galaxies
  ->Set.toArray
  ->Array.reduce(origin, ((xAcc, yAcc), Position((x, y))) => {
    (max(xAcc, x), max(y, yAcc))
  })
}

let emptyRowCol = (Galaxies(galaxies) as g) => {
  let (maxX, maxY) = g->maxCoord
  let origin = (0n, 0n)
  let galaxies = galaxies->Set.toArray

  let xs = Array.fromInitializer(~length=toInt(maxX), Function.id)
  let cols = xs->Array.filter(x => {
    galaxies->Array.filter((Position((x', _))) => x' == fromInt(x))->Array.length == 0
  })

  let ys = Array.fromInitializer(~length=toInt(maxY), Function.id)
  let rows = ys->Array.filter(y => {
    galaxies->Array.filter((Position((_, y'))) => y' == fromInt(y))->Array.length == 0
  })

  (cols, rows)
}

let expand = (Galaxies(galaxies), (expX, expY), scale) => {
  let nElemLessThan = (arr, n) => {
    arr->Array.count(x => fromInt(x) < n)
  }

  let expandXY = arr =>
    arr->Array.map((Position((x, y))) => {
      (
        x + nElemLessThan(expX, x)->fromInt * (scale - 1n),
        y + nElemLessThan(expY, y)->fromInt * (scale - 1n),
      )->Position
    })

  galaxies->Set.toArray->expandXY->Set.fromArray->Galaxies
}

let absBigInt = (x: bigint) => x < 0n ? 0n - x : x

let manhattan: (position, position) => bigint = (Position((x, y)), Position((x', y'))) => {
  (x - x')->absBigInt + (y - y')->absBigInt
}

let distances = (Galaxies(galaxies)) => {
  let galaxies = galaxies->Set.toArray
  galaxies->Array.reduce([], (acc, p) => {
    galaxies->Array.filter(p' => p != p')->Array.map(p' => manhattan(p, p'))->Array.concat(acc)
  })
}

let part1: (galaxies, bigint) => bigint = (g, scale) => {
  g
  ->expand(emptyRowCol(g), scale)
  ->distances
  // sum of all distances / 2 since we are counting each distance twice
  ->Array.reduce(0n, (acc, d) => acc + d) / 2n
}

let parse = data => {
  let xy =
    data
    ->splitNewline
    ->Array.map(r => {
      r->String.trim->String.split("")
    })

  xy
  ->Array.reduceWithIndex([], (acc, row, rowIndex) => {
    row
    ->Array.reduceWithIndex([], (acc, col, colIndex) => {
      col == "#"
        ? {
            acc->Array.push(Position((BigInt.fromInt(colIndex), BigInt.fromInt(rowIndex))))
            acc
          }
        : acc
    })
    ->Array.concat(acc)
  })
  ->Set.fromArray
  ->Galaxies
}

let solvePart1 = data => {
  data->parse->part1(fromInt(2))
}

let solvePart2 = data => {
  data->parse->part1(fromInt(1000000))
}
