open Belt
open Utils
let log = Js.Console.log

module CountContainer = {
  // key = item like 1, 0
  // value = count of the item
  type t = Map.Int.t<int>

  let set = Map.Int.set
  let get = Map.Int.get
  let getWithDefault = Map.Int.getWithDefault
  let reduce = Map.Int.reduce

  let inc = (t, k) => t->set(k, t->getWithDefault(k, 0) + 1)

  let mostCommon = t => {
    let one_count = t->get(1)
    let zero_count = t->get(0)

    one_count == zero_count ? 1 : one_count > zero_count ? 1 : 0
  }

  let leastCommon = t => {
    let one_count = t->get(1)
    let zero_count = t->get(0)
    one_count == zero_count ? 0 : one_count < zero_count ? 1 : 0
  }

  let make = _ => {
    // use Set to make sure items are unique
    [0, 1]->Array.reduce(Map.Int.empty, (a, x) => a->set(x, 0))
  }
}

module Total = {
  // key = bit pos
  // value = content counts
  type t = MutableMap.Int.t<CountContainer.t>

  let set = MutableMap.Int.set
  let get = MutableMap.Int.get
  let getSafe = (m, k) => m->MutableMap.Int.getWithDefault(k, CountContainer.make())
  let forEach = MutableMap.Int.forEach
  let map = MutableMap.Int.map

  let make = MutableMap.Int.make
}

let toBinaryString = MutableMap.Int.valuesToArray

let logAndCont = x => {
  Js.log2("binStringToInt", x)
  x
}
let bitArrayToInt = x => {
  x->Array.map(Int.toString)->Js.Array2.joinWith("")->Utils.parseInt(~x=_, ~base=2)
}

let calTotal = xs => {
  xs->Array.reduce(Total.make(), (a, bits) => {
    bits->Array.forEachWithIndex((idx, bit_val) => {
      let orig_total = a->Total.getSafe(idx)
      a->Total.set(idx, orig_total->CountContainer.inc(bit_val))
    })
    a
  })
}

let calGamma = xs => xs->calTotal->Total.map(CountContainer.mostCommon)->toBinaryString
let calEpsilon = xs => xs->calTotal->Total.map(CountContainer.leastCommon)->toBinaryString

let part1 = xs => {
  let (gamma, epsilon) = (xs->calGamma, xs->calEpsilon)

  gamma->bitArrayToInt * epsilon->bitArrayToInt
}

let findRating = (xs, func) => {
  let rec inner = (bit_pos, inputs, func) => {
    switch inputs {
    | [] => raise(Not_found)
    | [item] => item
    | items => {
        let filter = items->func
        let criteria = filter->Array.getExn(bit_pos)
        let next_inputs = items->Array.keep(x => x->Array.getExn(bit_pos) == criteria)
        inner(bit_pos + 1, next_inputs, func)
      }
    }
  }
  inner(0, xs, func)
}

let calOxygen = findRating(_, calGamma)
let calCO2 = findRating(_, calEpsilon)

let part2 = xs => {
  let oxygen = xs->calOxygen
  let co2 = xs->calCO2
  oxygen->bitArrayToInt * co2->bitArrayToInt
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(line => line->Js.String2.trim->Utils.splitChars->Array.map(Utils.intFromStringExn))

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
