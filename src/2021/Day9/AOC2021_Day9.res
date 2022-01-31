@@warning("-27")

open Belt
open Utils
let log = Js.Console.log

module HeightMap = {
  type t = Array2D.t<int>
  type coord = (int, int)

  let north = ((x, y)) => {(x, y - 1)}
  let east = ((x, y)) => {(x + 1, y)}
  let south = ((x, y)) => {(x, y + 1)}
  let west = ((x, y)) => {(x - 1, y)}

  let stepFunc = ((x, y): coord, f) => (x, y)->f

  let stepN = stepFunc(_, north)
  let stepE = stepFunc(_, east)
  let stepS = stepFunc(_, south)
  let stepW = stepFunc(_, west)

  let adjCoords = c => {
    list{stepN, stepW, stepE, stepS}->List.map(f => c->f)
  }

  type elem = CoordAndVal(coord, int)

  let getAdjacents = (t, (x, y)) => {
    (x, y)
    ->adjCoords
    ->List.keepMap(c => {
      t->Array2D.isValidXY(c) ? Some(CoordAndVal(c, t->Array2D.getExn(c))) : None
    })
  }

  let rec isLowest = (x, adjs) => {
    switch adjs {
    | list{} => true
    | list{CoordAndVal(coord, adj), ...others} => x >= adj ? false : isLowest(x, others)
    }
  }

  let getLowPoints = t => {
    t->Array2D.reduceWithIndex([], (a, p, (x, y)) => {
      p->isLowest(t->getAdjacents((x, y))) ? a->Array.concat([CoordAndVal((x, y), p)]) : a
    })
  }

  let getAllPoints = t => {
    t->Array2D.reduceWithIndex([], (a, p, (x, y)) => {
      a->Array.concat([CoordAndVal((x, y), p)])
    })
  }

  //   make a PairSet (see https://rescript-lang.org/docs/manual/latest/api/belt/set)
  module PairComparator = Belt.Id.MakeComparable({
    type t = (int, int)
    let cmp = ((a0, a1), (b0, b1)) =>
      switch Pervasives.compare(a0, b0) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      }
  })

  let getBasin = (t, (x, y)) => {
    let rec helper = (t, (x, y), visited) => {
      let v = t->Array2D.getExn((x, y))

      v < 0
        ? {
            visited
          }
        : {
            let new_set = Belt.Set.add(visited, (x, y))
            t->Array2D.set((x, y), -1)->ignore

            let candidates =
              t
              ->getAdjacents((x, y))
              ->List.keep((CoordAndVal(coord, va)) => {
                va != 9 && v >= 0 && !(new_set->Belt.Set.has(coord))
              })

            candidates->List.reduce(new_set, (acc, CoordAndVal((xa, ya), s)) => {
              Belt.Set.union(acc, helper(t, (xa, ya), acc))
            })
          }
    }

    let b_set = Belt.Set.make(~id=module(PairComparator))

    helper(t, (x, y), b_set)
  }

  let getBasinSize = (t, (x, y)) => {
    t->getBasin((x, y))->Belt.Set.size
  }

  // from https://github.com/yangdanny97/advent-of-code/blob/main/src/day9.res
  let rec search = (grid, x, y) => {
    let row = grid[y]->Option.getWithDefault([])
    let height = row[x]->Option.getWithDefault(10)
    if height >= 9 || height < 0 {
      0
    } else {
      // mark grid as visited
      ignore(row[x] = -1)
      1 +
      search(grid, x - 1, y) +
      search(grid, x + 1, y) +
      search(grid, x, y - 1) +
      search(grid, x, y + 1)
    }
  }

  let make = xs => {
    let x = xs->Array.getExn(0)->Array.length
    let y = xs->Array.length
    let ret = Array2D.make((x, y), 0)

    xs->Array.forEachWithIndex((y, ys) => {
      ys->Array.forEachWithIndex((x, c) => {
        ret->Array2D.set((x, y), c)->ignore
      })
    })
    ret
  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(
    FP_Utils.compose(Js.String2.trim, x => x->Utils.splitChars->Array.map(intFromStringExn)),
  )

let solvePart1 = data => {
  let hmap = data->parse->HeightMap.make
  let add1 = Utils.add(_, 1)
  hmap
  ->HeightMap.getLowPoints
  ->Array.map((HeightMap.CoordAndVal(coord, v)) => {v->add1})
  ->Array.reduce(0, (a, x) => add(a, x))
}

let solvePart2 = data => {
  let hmap = data->parse->HeightMap.make

  //  hmap->HeightMap.getLowPoints->Array.forEach(Js.log)
  let basins =
    hmap
    ->HeightMap.getLowPoints
    //    ->HeightMap.getAllPoints
    ->Array.map((HeightMap.CoordAndVal(coord, v)) => {
      hmap->HeightMap.getBasinSize(coord)
    })
    ->SortArray.Int.stableSort
    ->Array.reverse

  let largest3 = basins->Array.slice(~offset=0, ~len=3)

  largest3->Array.reduce(1, Utils.mul)
}

let solvePart2_from_github = data => {
  let hmap = data->parse->HeightMap.make

  let basins =
    hmap
    ->HeightMap.getLowPoints
    //    ->HeightMap.getAllPoints
    ->Array.map((HeightMap.CoordAndVal((x, y), v)) => {
      hmap->HeightMap.search(x, y)
    })
    ->SortArray.Int.stableSort
    ->Array.reverse

  let largest3 = basins->Array.slice(~offset=0, ~len=3)

  //  largest3->Js.log
  largest3->Array.reduce(1, Utils.mul)
}
