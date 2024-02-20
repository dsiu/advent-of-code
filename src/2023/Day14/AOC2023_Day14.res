@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type element = Empty | Cube | Round
type grid = array<array<element>>

let readElement = str => {
  switch str {
  | "." => Empty
  | "#" => Cube
  | "O" => Round
  | _ => failwith("Invalid element")
  }
}

let makeGrid = xss => {
  xss->Array.map(Array.map(_, readElement))->Array.transpose
}

let showElement = el =>
  switch el {
  | Empty => "."
  | Cube => "#"
  | Round => "O"
  }

let showGrid: grid => string = grid => {
  grid
  ->Array.transpose
  ->Array.map(row =>
    row
    ->Array.map(el =>
      switch el {
      | Empty => "."
      | Cube => "#"
      | Round => "O"
      }
    )
    ->Array.joinWith("")
  )
  ->Array.joinWith("\n")
}

let gridEq = (g1, g2) => {
  //  g1
  //  ->Array.zip(g2)
  //  ->Array.all(((a, b)) => Array.equal(a, b, (a, b) => a == b))
  g1->showGrid == g2->showGrid
}

let rollStep: ((array<element>, element), element) => (array<element>, element) = (
  (handled, target),
  source,
) => {
  switch (target, source) {
  | (Empty, Round) => (handled->Array.concat([Round]), Empty)
  | (target, source) => (handled->Array.concat([target]), source)
  }
}

let roll: array<element> => array<element> = xs => {
  switch xs {
  | [] => []
  | _ => {
      let l = xs->Array.headUnsafe
      let ls = xs->Array.tail
      let (rs, r) = ls->Array.fold(~initial=([], l), ~f=rollStep)
      rs->Array.concat([r])
    }
  }
}

let rollGrid: grid => grid = Array.map(_, roll)

let rec rollToCompletion: grid => grid = grid => {
  let grid' = grid->rollGrid
  gridEq(grid, grid') ? grid' : rollToCompletion(grid')
}

let scoreGrid: grid => int = grid => {
  let normalizedGrid = grid->Array.transpose
  let l = normalizedGrid->Array.length
  let indexedGird = Array.zip(
    Array.fromInitializer(~length=l, i => i + 1),
    normalizedGrid->Array.toReversed,
  )
  let scoreRow = ((i, row)) => i * row->Array.filter(el => el == Round)->Array.length
  indexedGird->Array.map(scoreRow)->Array.sum(module(Int))
}

let rotate1: grid => grid = Fn.compose(Array.map(_, Array.toReversed), Array.transpose, ...)

let rec runNTimes = (n, f, x) => n == 0 ? x : runNTimes(n - 1, f, f(x))

let rec runNTimesWithCache = (n, f, x, cacheFn, predFn) => {
  n == 0
    ? (0, x)
    : {
        let x' = f(x)

        predFn(x')
          ? {
              log2("done. cycles = ", n)
              (n, x')
            }
          : {
              cacheFn(x', n)
              runNTimesWithCache(n - 1, f, x', cacheFn, predFn)
            }
      }
}

let rollCycle: grid => grid = grid => {
  let oneTurn = grid => grid->rollToCompletion->rotate1
  runNTimes(4, oneTurn, grid)
}

let part1 = grid => {
  grid->rollToCompletion->scoreGrid
}

let part2 = grid => {
  let cycles = 1000000000

  let cache = Map.make()
  let predFn = grid => {
    Map.has(cache, grid->showGrid)
  }
  let cacheFn = (grid, i) => {
    Map.set(cache, grid->showGrid, (grid, cycles - i))
  }
  let getCached = grid => {
    Map.get(cache, grid->showGrid)
  }

  let (i, g) = runNTimesWithCache(cycles, rollCycle, grid, cacheFn, predFn)
  let repeatEnd = cycles - i
  let firstSeen = getCached(g)

  switch firstSeen {
  | Some((_, repeatStart)) => {
      let repeatLen = repeatEnd - repeatStart
      let finalIndex = Int.mod(cycles - repeatStart, repeatLen) + repeatStart

      let ret =
        cache->Map.entries->Iterator.toArray->Array.filter(((k, (g, i))) => i == finalIndex - 1)
      let Some(_, (result, _)) = ret[0]

      result
    }
  | None => raise(Not_found)
  }->scoreGrid
}

let trimAndSplit = str => {
  str
  ->String.trim
  ->String.split("")
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(trimAndSplit)

let solvePart1 = data => {
  data->parse->makeGrid->part1
}

let solvePart2 = data => {
  data->parse->makeGrid->part2
}
