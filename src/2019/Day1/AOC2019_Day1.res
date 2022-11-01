open Belt
open Utils
let log = Js.Console.log

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let fuel = mass => {
  (mass->float_of_int /. 3.)->int_of_float - 2
}

let part1 = xs => {
  open FP_Utils
  xs->Array.map(fuel)->foldLeftArray(add)
}

let fuelCompound = mass => {
  let rec inner = (mass, acc) => {
    let mass' = fuel(mass)
    mass' <= 0 ? acc : inner(mass', acc + mass')
  }
  inner(mass, 0)
}

let part2 = xs => {
  open FP_Utils
  xs->Array.map(fuelCompound)->foldLeftArray(add)
}

let parse = data => data->splitNewline->Array.map(intFromStringExn)

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
