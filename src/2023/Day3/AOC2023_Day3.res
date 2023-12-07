@@uncurried

open RescriptCore
open Utils
open Coordinate.Direction

let log = Console.log
let log2 = Console.log2

type elem = Dot | Symbol(string) | Digit(int)

let makeElem = char => {
  switch char->Int.fromString(~radix=10) {
  | Some(d) => Digit(d)
  | None =>
    switch char {
    | "." => Dot
    | c => Symbol(c)
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
  | Symbol(_) => true
  | _ => false
  }
}

let isStar = e => {
  switch e {
  | Symbol("*") => true
  | _ => false
  }
}

type engine = Array2D.t<elem>
type position = Coordinate.t
type neighbors = array<elem>
type region = array<position>

let getNeighborsIf: (position, position => option<position>) => region = (
  c: position,
  fn,
): region => {
  let directions = [north, south, east, west, northEast, northWest, southEast, southWest]

  directions->Array.filterMap(dir => {
    fn(c->dir)
  })
}

let isElemDigit: (engine, position) => option<position> = (engine, p) => {
  engine
  ->Array2D.get(p)
  ->Option.flatMap(e => {
    isDigit(e) ? Some(p) : None
  })
}

let makeEngine: Array2D.t<string> => engine = Array2D.mapU(_, makeElem)

let engineFilter: (engine, elem => bool) => region = (engine, fn): region => {
  // Todo: can add filterMapWithIndex to Array2D
  engine->Array2D.reduceWithIndex([], (acc, e, c) => {
    switch fn(e) {
    | true => acc->Array.push(c)
    | false => ()
    }
    acc
  })
}

let rowsFromRegion: region => array<int> = region => {
  region
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
}

// returns array of regions, each region is a number with its positions
let findNumbers: engine => array<region> = engine => {
  let numberRegions: region = engine->engineFilter(isDigit)->Array.toSorted(Coordinate.Compare.yx)

  let rowsWithNumber = rowsFromRegion(numberRegions)

  let groupedByRow =
    rowsWithNumber->Array.map(y => numberRegions->Array.filter(((_, y2)) => y2 == y))

  groupedByRow
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
          parsed->Array.push(buf)
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
  ->Array.flat // flatten so that each elem is a number with all of digits positions
}

let findSymbols: engine => region = engineFilter(_, isSymbol)
let findStars: engine => region = engineFilter(_, isStar)

// returns region that is digit touched by a symbol
let touchedDigit: (engine, region) => region = (engine, symbols) => {
  symbols->Array.flatMap(getNeighborsIf(_, isElemDigit(engine, _)))
}

let isNumberTouched: (region, region) => bool = (number, symTouched) => {
  symTouched->Array.some(sPos => {
    number->Array.some(nPos => nPos == sPos)
  })
}

let getNumber: (engine, region) => int = (engine, number) => {
  number
  ->Array.map(n => {
    switch Array2D.get(engine, n) {
    | Some(Digit(d)) => d
    | _ => failwith("expected digit")
    }
  })
  ->Array.reduce(0, (acc, d) => acc * 10 + d)
}

let findNumbersTouched: (engine, array<region>, region) => array<int> = (
  engine,
  numbers,
  symTouched,
) => {
  numbers
  ->Array.filter(isNumberTouched(_, symTouched))
  ->Array.map(getNumber(engine, _))
}

let part1: engine => int = engine => {
  let symbols = findSymbols(engine)
  //  log2("symbols", symbols)

  let numbers = findNumbers(engine)
  //  log2("numbers", numbers)

  let symTouched = touchedDigit(engine, symbols)
  //  log2("symTouched", symTouched)

  let nums = findNumbersTouched(engine, numbers, symTouched)
  nums->sumIntArray
}

let part2: engine => int = engine => {
  let stars = findStars(engine)
  //  log2("stars", stars)

  let numbers = findNumbers(engine)
  //  log2("numbers", numbers)

  let starTouched = stars->Array.filterMap(s => {
    let t = touchedDigit(engine, [s])
    // number of position touched must be at least 2 for gear to be valid
    t->Array.length >= 2 ? Some(t) : None
  })
  //  log2("starTouched", starTouched)

  let gearParts = starTouched->Array.filterMap(x => {
    let touchedNums = findNumbersTouched(engine, numbers, x)
    // number of numbers must be exactly 2 for gear to be valid
    touchedNums->Array.length == 2 ? Some(touchedNums->mulIntArray) : None
  })
  //  log2("gearParts", gearParts)
  gearParts->sumIntArray
}

let parse: string => Array2D.t<string> = data =>
  data->splitNewline->Array.map(compose(String.trim, splitChars))

let solvePart1: string => int = data => {
  data->parse->makeEngine->part1
}

let solvePart2 = data => {
  data->parse->makeEngine->part2
}
