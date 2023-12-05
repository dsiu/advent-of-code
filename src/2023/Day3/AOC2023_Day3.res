@@uncurried

open RescriptCore
open Utils
open Coordinate.Direction

let log = Console.log

type elem = Dot | Symbol | Digit(int)

let makeElem = char => {
  switch char->Int.fromString(~radix=10) {
  | Some(d) => Digit(d)
  | None =>
    switch char {
    | "." => Dot
    | _ => Symbol
    }
  }
}

let isDigit = e => {
  switch e {
  | Digit(_) => true
  | _ => false
  }
}

let isSymbol = e => {
  switch e {
  | Symbol => true
  | _ => false
  }
}

type neighbors = array<elem>

let getNeighborsIf = (c: Coordinate.t, fn): array<Coordinate.t> => {
  let neighbors = [north, south, east, west, northEast, northWest, southEast, southWest]

  neighbors->Array.reduce([], (acc, dir) => {
    let thisNeighbors = dir(c)
    //    `checking on ${c->Coordinate.toString} with ${thisNeighbors->Coordinate.toString}`->log

    switch fn(thisNeighbors) {
    | Some(_) => {
        acc->Array.push(thisNeighbors)
        acc
      }
    | None => acc
    }
  })
}

let isElemDigit = (elemMap, c) => {
  elemMap
  ->Array2D.get(c)
  ->Option.flatMap(e => {
    switch e {
    | Digit(_) =>
      //        `found digit at ${c->Coordinate.toString}`->log
      Some(c)
    | _ => None
    }
  })
}

let makeElemMap = Array2D.mapU(_, makeElem)

let elemMapFilter = (elemMap, fn): array<Coordinate.t> => {
  elemMap->Array2D.reduceWithIndex([], (acc, e, c) => {
    switch fn(e) {
    | true => acc->Array.push(c)
    | false => ()
    }
    acc
  })
}

let part1 = charMap => {
  let elemMap = charMap->makeElemMap

  let numberCoords = elemMap->elemMapFilter(isDigit)
  "numberCoords"->log
  numberCoords->log

  let sortedNumberCoords = numberCoords->Array.toSorted(Coordinate.Compare.yx)
  "sortedNumberCoords"->log
  sortedNumberCoords->log

  let uniqueYs =
    sortedNumberCoords
    ->Array.map(((_, y)) => y)
    ->Array.toSorted(Int.compare)
    ->Array.reduce([], (acc, e) => {
      let last = acc->Array.at(acc->Array.length - 1)
      switch last {
      | Some(lastExist) if lastExist == e => acc
      | Some(_)
      | None => {
          acc->Array.push(e)
          acc
        }
      }
    })
  "uniqueYs"->log
  uniqueYs->log

  let groupedByY = uniqueYs->Array.reduce([], (acc, y) => {
    acc->Array.push(sortedNumberCoords->Array.filter(((_, y2)) => y2 == y))
    acc
  })
  "groupedByY"->log
  groupedByY->log

  let groupedByIncX =
    groupedByY
    ->Array.map(Array.toSorted(_, Coordinate.Compare.xy))
    ->Array.map(l => {
      l->Array.push((0, 0)) // hack to add extra last arg so reduce can end correctly
      l
      ->Array.reduce(([], []), ((parsed, buf), (x, y)) => {
        let last = buf->Array.at(buf->Array.length - 1)
        switch last {
        | Some((lastExistX, _)) if lastExistX === x - 1 => {
            buf->Array.push((x, y))
            (parsed, buf)
          }
        | Some(_) => {
            parsed->Array.pushMany([buf])
            (parsed, [(x, y)])
          }
        | None => {
            buf->Array.push((x, y))
            (parsed, buf)
          }
        }
      })
      ->fst
    })

  "groupedByIncX"->log
  groupedByIncX->log
  groupedByIncX->Array.forEach(log)

  let symCoords = elemMap->elemMapFilter(isSymbol)
  "symCoords"->log
  symCoords->log

  let partNumCoords = symCoords->Array.reduce([], (acc, c) => {
    acc->Array.pushMany(getNeighborsIf(c, isElemDigit(elemMap, _)))
    acc
  })
  "partNumCoords"->log
  partNumCoords->log
}

let parse = data => data->splitNewline->Array.map(compose(String.trim, splitChars))

let solvePart1 = data => {
  data->parse->part1->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
