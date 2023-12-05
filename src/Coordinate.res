open RescriptCore

type t = (int, int)

module Direction = {
  let north = (. (x, y): t) => {(x, y - 1)}
  let east = (. (x, y): t) => {(x + 1, y)}
  let south = (. (x, y): t) => {(x, y + 1)}
  let west = (. (x, y): t) => {(x - 1, y)}
  let northEast = (. c) => east(. north(. c))
  let northWest = (. c) => west(. north(. c))
  let southEast = (. c) => east(. south(. c))
  let southWest = (. c) => west(. south(. c))
}

module StepFunctions = {
  let stepFunc = (. (x, y): t, f) => f(. (x, y))

  open Direction
  let stepN = (. c) => stepFunc(. c, north)
  let stepE = (. c) => stepFunc(. c, east)
  let stepS = (. c) => stepFunc(. c, south)
  let stepW = (. c) => stepFunc(. c, west)
  let stepNE = (. c) => stepFunc(. c, northEast)
  let stepNW = (. c) => stepFunc(. c, northWest)
  let stepSE = (. c) => stepFunc(. c, southEast)
  let stepSW = (. c) => stepFunc(. c, southWest)
}

let toString = (. (x, y)) => `(${x->Int.toString},${y->Int.toString})`

module Compare = {
  let xy = ((x1, y1), (x2, y2)) => {
    let c = Int.compare(x1, x2)
    if c == Ordering.equal {
      Int.compare(y1, y2)
    } else {
      c
    }
  }

  let yx = ((x1, y1), (x2, y2)) => {
    let c = Int.compare(y1, y2)
    if c == Ordering.equal {
      Int.compare(x1, x2)
    } else {
      c
    }
  }
}
