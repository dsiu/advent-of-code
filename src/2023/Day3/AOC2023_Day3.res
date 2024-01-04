@@uncurried

open Stdlib
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

/**
 * This function is used to get the neighbors of a given position based on a provided function.
 * It takes two parameters: a position and a function that takes a position and returns an optional position.
 * The function iterates over all the directions (north, south, east, west, northEast, northWest, southEast, southWest)
 * and applies the provided function to each direction. The result is then filtered to remove any
 * None values.
 *
 * @param {position} c - The current position.
 * @param {function} fn - The function to apply to each direction. It should take a position and return an optional position.
 * @returns {region} - An array of positions that are neighbors of the current position based on the provided function.
 */
let getNeighborsIf: (position, position => option<position>) => region = (
  c: position,
  fn,
): region => {
  let directions = [north, south, east, west, northEast, northWest, southEast, southWest]

  directions->Array.filterMap(dir => {
    fn(c->dir)
  })
}

/**
 * This function checks if the element at a given position in the engine is a digit.
 * It takes two parameters: the engine and a position.
 * The function retrieves the element at the given position in the engine and checks if it is a digit.
 * If the element is a digit, it returns Some(position), otherwise it returns None.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @param {position} p - The position to check.
 * @returns {option<position>} - Some(position) if the element at the given position is a digit, None otherwise.
 */
let isElemDigit: (engine, position) => option<position> = (engine, p) => {
  engine
  ->Array2D.get(p)
  ->Option.flatMap(e => {
    isDigit(e) ? Some(p) : None
  })
}

let makeEngine: Array2D.t<string> => engine = Array2D.mapU(_, makeElem)

/**
 * This function filters the elements of the engine based on a provided function.
 * It takes two parameters: the engine and a function that takes an element and returns a boolean.
 * The function iterates over all the elements in the engine and applies the provided function to each element.
 * If the function returns true for an element, the position of that element is added to the result.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @param {function} fn - The function to apply to each element. It should take an element and return a boolean.
 * @returns {region} - An array of positions of the elements for which the provided function returned true.
 */
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

/**
 * This function is used to get the unique rows from a given region.
 * It takes one parameter: a region which is an array of positions.
 * The function maps each position in the region to its y-coordinate (row number) and sorts the resulting array.
 * It then reduces the sorted array to a new array that contains only the unique row numbers.
 *
 * @param {region} region - The region to get the rows from. It should be an array of positions.
 * @returns {array<int>} - An array of unique row numbers from the given region.
 */
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

/**
 * This function is used to find all the numbers in the engine.
 * It takes one parameter: the engine which is a 2D array of elements.
 * The function first filters the elements of the engine to get the positions of the digits and sorts them by their y-coordinate.
 * It then gets the unique rows from the number regions and groups the number regions by row.
 * Each group is sorted by x-coordinate and then reduced to an array of numbers, where each number is an array of positions.
 * The function ends by flattening the array of numbers so that each element is a number with all of its digit positions.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @returns {array<region>} - An array of numbers, where each number is an array of positions.
 */
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

/**
 * This function returns a region that represents digits in the engine that are touched by a symbol.
 * It takes two parameters: the engine and a region representing the symbols.
 * The function maps each symbol in the symbols region to its neighbors in the engine that are digits.
 * The result is then flattened to a single region.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @param {region} symbols - The region representing the symbols.
 * @returns {region} - A region representing the digits in the engine that are touched by a symbol.
 */
let touchedDigit: (engine, region) => region = (engine, symbols) => {
  symbols->Array.flatMap(getNeighborsIf(_, isElemDigit(engine, _)))
}

/**
 * This function checks if a number is touched by a symbol.
 * It takes two parameters: a region representing a number and a region representing the symbols that have touched a digit.
 * The function checks if any position in the number region is also in the symTouched region.
 *
 * @param {region} number - The region representing a number.
 * @param {region} symTouched - The region representing the symbols that have touched a digit.
 * @returns {bool} - True if the number is touched by a symbol, false otherwise.
 */
let isNumberTouched: (region, region) => bool = (number, symTouched) => {
  symTouched->Array.some(sPos => {
    number->Array.some(nPos => nPos == sPos)
  })
}

/**
 * This function is used to get the number represented by a region in the engine.
 * It takes two parameters: the engine and a region representing a number.
 * The function maps each position in the number region to its corresponding digit in the engine.
 * It then reduces the array of digits to a single number by treating the digits as the digits of the number in base 10.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @param {region} number - The region representing a number.
 * @returns {int} - The number represented by the given region in the engine.
 */
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

/**
 * This function is used to find all the numbers in the engine that are touched by a symbol.
 * It takes three parameters: the engine, an array of regions representing numbers, and a region representing the symbols that have touched a digit.
 * The function filters the numbers to include only those that are touched by a symbol and maps each touched number to its corresponding number in the engine.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @param {array<region>} numbers - An array of regions representing numbers.
 * @param {region} symTouched - The region representing the symbols that have touched a digit.
 * @returns {array<int>} - An array of numbers that are touched by a symbol.
 */
let findNumbersTouched: (engine, array<region>, region) => array<int> = (
  engine,
  numbers,
  symTouched,
) => {
  numbers->Array.filterMap(num => {
    isNumberTouched(num, symTouched) ? Some(getNumber(engine, num)) : None
  })
}

/**
 * This function is used to solve the first part of the problem.
 * It takes one parameter: the engine which is a 2D array of elements.
 * The function first finds all the symbols in the engine and the positions of the digits.
 * It then finds the digits that are touched by a symbol.
 * Finally, it finds the numbers that are touched by a symbol and sums them up.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @returns {int} - The sum of the numbers that are touched by a symbol.
 */
let part1: engine => int = engine => {
  let symbols = findSymbols(engine) // Find all the symbols in the engine

  let numbers = findNumbers(engine) // Find the positions of the digits in the engine

  let symTouched = touchedDigit(engine, symbols) // Find the digits that are touched by a symbol

  let nums = findNumbersTouched(engine, numbers, symTouched) // Find the numbers that are touched by a symbol

  nums->sumIntArray // Sum up the numbers
}

/**
 * This function is used to solve the second part of the problem.
 * It takes one parameter: the engine which is a 2D array of elements.
 * The function first finds all the stars in the engine and the positions of the digits.
 * It then finds the digits that are touched by a star, but only if at least two positions are touched.
 * Finally, it finds the numbers that are touched by a star and multiplies them together, but only if exactly two numbers are touched.
 * The function ends by summing up all the multiplied numbers.
 *
 * @param {engine} engine - The 2D array representing the engine.
 * @returns {int} - The sum of the multiplied numbers that are touched by a star.
 */
let part2: engine => int = engine => {
  let stars = findStars(engine) // Find all the stars in the engine

  let numbers = findNumbers(engine) // Find the positions of the digits in the engine

  let starTouched = stars->Array.filterMap(s => {
    let t = touchedDigit(engine, [s]) // Find the digits that are touched by a star
    // number of position touched must be at least 2 for gear to be valid
    t->Array.length >= 2 ? Some(t) : None
  })

  let gearParts = starTouched->Array.filterMap(x => {
    let touchedNums = findNumbersTouched(engine, numbers, x) // Find the numbers that are touched by a star
    // number of numbers must be exactly 2 for gear to be valid
    touchedNums->Array.length == 2 ? Some(touchedNums->mulIntArray) : None
  })

  gearParts->sumIntArray // Sum up the multiplied numbers
}

let parse: string => Array2D.t<string> = data =>
  data->splitNewline->Array.map(compose(String.trim, splitChars))

let solvePart1: string => int = data => {
  data->parse->makeEngine->part1
}

let solvePart2 = data => {
  data->parse->makeEngine->part2
}
