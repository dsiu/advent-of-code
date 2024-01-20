@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

open BigInt

// todo: refactor
module Coord_V2_Big = {
  type t = Tuple2.t<BigInt.t, BigInt.t>

  // int compare
  let bigIntCompareOrd = (a: BigInt.t, b: BigInt.t) =>
    a < b ? Core__Ordering.less : a > b ? Core__Ordering.greater : Core__Ordering.equal

  // int compare
  let bigIntCompare = (a, b) => bigIntCompareOrd(a, b)->Ordering.toInt
  let intCompare = (a, b) => Int.compare(a, b)->Ordering.toInt
  let compare = Tuple2.compare(~f=bigIntCompare, ~g=bigIntCompare)

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

// todo: refactor to stdlib
let setToArray = set => set->Set.values->Iterator.toArray

let maxCoord = (Galaxies(galaxies)) => {
  let origin = (Constants.zero, Constants.zero)

  galaxies
  ->setToArray
  ->Array.reduce(origin, ((xAcc, yAcc), Position((x, y))) => {
    (x > xAcc ? x : xAcc, y > yAcc ? y : yAcc)
  })
}

let emptyRowCol = (Galaxies(galaxies) as g) => {
  let (maxX, maxY) = g->maxCoord
  let origin = (Constants.zero, Constants.zero)
  let galaxies = galaxies->setToArray

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
    arr->Array.filter(x => fromInt(x) < n)->Array.length
  }

  let expandX = arr =>
    arr->Array.map((Position((x, y))) => {
      (x + nElemLessThan(expX, x)->fromInt * (scale - Constants.one), y)->Position
    })

  let expandY = arr =>
    arr->Array.map((Position((x, y))) => {
      (x, y + nElemLessThan(expY, y)->fromInt * (scale - Constants.one))->Position
    })

  galaxies->setToArray->expandX->expandY->Set.fromArray->Galaxies
}

let absBigInt = (x: BigInt.t) => x < Constants.zero ? Constants.zero - x : x

let manhanten: (position, position) => BigInt.t = (Position((x, y)), Position((x', y'))) => {
  (x - x')->absBigInt + (y - y')->absBigInt
}

let distances = (Galaxies(galaxies)) => {
  let galaxies = galaxies->setToArray
  galaxies->Array.reduce([], (acc, p) => {
    galaxies->Array.filter(p' => p != p')->Array.map(p' => manhanten(p, p'))->Array.concat(acc)
  })
}

let part1: (galaxies, BigInt.t) => BigInt.t = (g, scale) => {
  g
  ->expand(emptyRowCol(g), scale)
  ->distances
  ->Array.reduce(Constants.zero, (acc, d) => acc + d) / fromInt(2)
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
