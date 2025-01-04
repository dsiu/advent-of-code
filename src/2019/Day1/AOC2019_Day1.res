open Belt
open Utils
let log = Js.Console.log
module TC = Tablecloth

let fuel = mass => {
  (mass->Float.fromInt /. 3.)->Float.toInt - 2
}

let part1 = xs => {
  xs->Array.map(fuel)->TC.Array.sum(module(TC.Int))
}

let fuelCompound = mass => {
  let rec inner = (mass, acc) => {
    let mass' = fuel(mass)
    mass' <= 0 ? acc : inner(mass', acc + mass')
  }
  inner(mass, 0)
}

let part2 = xs => {
  xs->Array.map(fuelCompound)->TC.Array.sum(module(TC.Int))
}

let parse = data => data->splitNewline->Array.map(intFromStringExn)

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
