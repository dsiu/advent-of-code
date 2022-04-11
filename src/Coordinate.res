type t = (int, int)

let north = ((x, y): t) => {(x, y - 1)}
let east = ((x, y): t) => {(x + 1, y)}
let south = ((x, y): t) => {(x, y + 1)}
let west = ((x, y): t) => {(x - 1, y)}
let northEast = c => c->north->east
let northWest = c => c->north->west
let southEast = c => c->south->east
let southWest = c => c->south->west

let stepFunc = ((x, y): t, f) => (x, y)->f

let stepN = stepFunc(_, north)
let stepE = stepFunc(_, east)
let stepS = stepFunc(_, south)
let stepW = stepFunc(_, west)
let stepNE = stepFunc(_, northEast)
let stepNW = stepFunc(_, northWest)
let stepSE = stepFunc(_, southEast)
let stepSW = stepFunc(_, southWest)
