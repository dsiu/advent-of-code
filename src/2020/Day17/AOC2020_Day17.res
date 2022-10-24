open Belt
open Utils
open FP_Utils

let log = Js.Console.log

module TC = Tablecloth

module Coord = {
  open TC
  type t = Tuple3.t<int, int, int>
  let compare = Tuple3.compare(~f=Int.compare, ~g=Int.compare, ~h=Int.compare)

  include Comparator.Make({
    type t = t
    let compare = compare
  })
}

type grid = TC.Set.t<Coord.t, Coord.identity>

let makeGrid = (lines: array<array<string>>) => {
  let createActive = (. x, y) => {
    lines->Array.getExn(y)->Array.getExn(x) === "#" ? Some((x, y, 0)) : None
  }
  let maxX = lines->Array.getExn(0)->Array.length - 1
  let maxY = lines->Array.length - 1
  let xs = Array.range(0, maxX)
  let ys = Array.range(0, maxY)
  combinationIfArray2(xs, ys, createActive)->TC.Set.fromArray(module(Coord))
}

let parse = data => data->splitNewline->Array.map(compose(Js.String2.trim, splitChars))

let solvePart1 = data => {
  data->parse->log
  data->parse->makeGrid->TC.Set.toArray->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
