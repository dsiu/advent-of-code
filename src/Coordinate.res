type t = (int, int)

let north = (. (x, y): t) => {(x, y - 1)}
let east = (. (x, y): t) => {(x + 1, y)}
let south = (. (x, y): t) => {(x, y + 1)}
let west = (. (x, y): t) => {(x - 1, y)}
let northEast = (. c) => east(. north(. c))
let northWest = (. c) => west(. north(. c))
let southEast = (. c) => east(. south(. c))
let southWest = (. c) => west(. south(. c))

let stepFunc = (. (x, y): t, f) => f(. (x, y))

let stepN = (. c) => stepFunc(. c, north)
let stepE = (. c) => stepFunc(. c, east)
let stepS = (. c) => stepFunc(. c, south)
let stepW = (. c) => stepFunc(. c, west)
let stepNE = (. c) => stepFunc(. c, northEast)
let stepNW = (. c) => stepFunc(. c, northWest)
let stepSE = (. c) => stepFunc(. c, southEast)
let stepSW = (. c) => stepFunc(. c, southWest)
