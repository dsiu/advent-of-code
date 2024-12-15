open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // r, c
type bounds = (position, position)
type grid = Map.t<string, array<position>>

// todo: put in Std map
let getWithDefault = (m, k, d) => {
  Map.get(m, k)->Option.getWithDefault(d)
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
  grid->log
  bounds->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
