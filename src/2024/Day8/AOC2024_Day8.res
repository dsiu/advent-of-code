open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // r, c
type bounds = (position, position)
type grid = Map.t<string, array<position>>

// todo: put in Std map
let getWithDefault = (m, k, d) => {
  Map.get(m, k)->Option.getOr(d)
}

let inRange: (bounds, position) => bool = ((min, max), c) => {
  let (r, c) = c
  let (minR, minC) = min
  let (maxR, maxC) = max
  r >= minR && r <= maxR && c >= minC && c <= maxC
}

let antinodeOfNode: (position, position) => position = (a, b) => {
  Coord_V2.mul(a, 2)->(Coord_V2.sub(_, b))
}

let antinodesOf: (bounds, array<position>) => array<position> = (bounds, ps) => {
  Array.combinationIf2(ps, ps, (a, b) => {
    switch Coord_V2.compare(a, b) {
    | 0 => None
    | _ => Some(antinodeOfNode(a, b))
    }
  })->Array.filter(p => inRange(bounds, p))
}

let allFreqAntinodes: (bounds, grid) => array<position> = (bounds, grid) => {
  grid
  ->Map.values
  ->Iterator.toArray
  ->Array.flatMap(ps => antinodesOf(bounds, ps))
}

let harmonicAntinodesOfNode: (bounds, position, position) => array<position> = (bounds, a, b) => {
  let (_, (kMax, _)) = bounds
  Array.fromInitializer(~length=kMax, k => {
    Coord_V2.sub(a, Coord_V2.sub(a, b)->Coord_V2.mul(k))
  })
}

let harmonicAntinodesOf: (bounds, array<position>) => array<position> = (bounds, ps) => {
  Array.combinationIf2(ps, ps, (a, b) => {
    switch Coord_V2.compare(a, b) {
    | 0 => None
    | _ => Some(harmonicAntinodesOfNode(bounds, a, b))
    }
  })
  ->Array.flatMap(x => x)
  ->Array.filter(p => inRange(bounds, p))
}

let allFreqHarmonicAntinodes: (bounds, grid) => array<position> = (bounds, grid) => {
  grid
  ->Map.values
  ->Iterator.toArray
  ->Array.flatMap(ps => harmonicAntinodesOf(bounds, ps))
}

let parse: string => (grid, bounds) = data => {
  let rows = data->splitNewline->Array.map(r => r->String.trim->String.split(""))
  let rMax = rows->Array.length - 1
  let cMax = rows[0]->Option.map(Array.length)->Option.getExn - 1

  let grid =
    rows
    ->Array.flatMapWithIndex((row, r) =>
      row
      ->Array.mapWithIndex((elem, c) =>
        switch elem {
        | "." => None
        | _ => Some((elem, (r, c)))
        }
      )
      ->Array.keepSome
    )
    ->Array.reduce(Map.make(), (m, (elem, pos)) => {
      m->Map.set(elem, [pos, ...m->getWithDefault(elem, [])])
      m
    })
  (grid, ((0, 0), (rMax, cMax)))
}

let solvePart1 = data => {
  let (grid, bounds) = data->parse
  grid->(allFreqAntinodes(bounds, _))->Array.uniq->Array.length
}

let solvePart2 = data => {
  let (grid, bounds) = data->parse
  grid->(allFreqHarmonicAntinodes(bounds, _))->Array.uniq->Array.length
}
