open StdlibFp
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // col, row

type trail = array<position>

type grid = Array2D.t<int>

type tMap = {
  grid: grid,
  starts: array<position>,
  goals: array<position>,
}

type agendum = {
  current: position,
  trail: trail,
}

type agenda = array<agendum>

let parse: string => Array2D.t<int> = data => {
  data
  ->splitNewline
  ->Array.map(Fn.compose(String.trim, Utils.splitChars, _))
  ->Array.reduceWithIndex([], (acc, row, _r) => {
    [...acc, row->Array.mapWithIndex((col, _c) => Int.fromString(col)->Option.getOr(-1))]
  })
}

let part1 = tMap => {
  module Tuple2 = Serializable.MakeTuple2(JSONSerializable.Int, JSONSerializable.Int)
  module NodeSet = StdlibFp.Set.Make(Tuple2)
  module Traversal = Graph_Ext.Traversal(
    {
      type node = Tuple2.t
    },
    NodeSet,
  )

  let neighbors = (grid, node) => {
    let (c, r) = node
    let cur = grid->Array2D.get(node)->Option.getUnsafe
    //    (c, r)->log2("node", _)
    //    cur->log2("cur", _)
    // neighbors should be current value + 1
    [(c - 1, r), (c + 1, r), (c, r - 1), (c, r + 1)]->Array.filter(n => {
      grid->Array2D.isValidXY(n) &&
        grid->Array2D.get(n)->Option.flatMap(next => Some(next == cur + 1))->Option.getOr(false)
    })
  }

  tMap.starts->Array.reduce([], (acc, start) => {
    let bfsResult = Traversal.bfs(start, neighbors(tMap.grid, _), (_node, _distance) => false)
    [
      tMap.goals->Array.reduce([], (acc, end) => {
        [bfsResult->Traversal.path(start, end), ...acc]
      }),
      ...acc,
    ]
  })
}

let solvePart1 = data => {
  let grid = data->parse

  // todo: refactor to Array2D.filterWithIndex ??
  let starts =
    grid->Array2D.reduceWithIndex([], (acc, v, (c, r)) => {v == 0 ? [...acc, (c, r)] : acc}) // remember position is (row, col)
  let goals =
    grid->Array2D.reduceWithIndex([], (acc, v, (c, r)) => {v == 9 ? [...acc, (c, r)] : acc}) // remember position is (row, col)
  let tMap = {grid, starts, goals}
  // tMap->log
  let paths = part1(tMap)
  paths->Array.map(p => p->Array.keepSome)->Array.map(Array.length)->Array.sum(module(Int))
}

let solvePart2 = data => {
  data->ignore
  2
}
