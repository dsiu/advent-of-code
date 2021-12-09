open Belt
open Utils
let log = Js.Console.log

module CountContainer = {
  type t = Map.Int.t<int>

  let set = Map.Int.set
  let get = Map.Int.get
  let getWithDefault = Map.Int.getWithDefault
  let reduce = Map.Int.reduce

  let inc = (t, k) => t->set(k, t->getWithDefault(k, 0) + 1)

  let mostCommon = t => {
    let (k, v) = t->reduce((Js.Int.min, Js.Int.min), ((lk, lv), k, v) => {
      v > lv ? (k, v) : (lk, lv)
    })
    v->ignore
    k
  }

  let leastCommon = t => {
    let (k, v) = t->reduce((Js.Int.max, Js.Int.max), ((lk, lv), k, v) => {
      v < lv ? (k, v) : (lk, lv)
    })
    v->ignore
    k
  }

  let make = items => {
    // use Set to make sure items are unique
    items->Array.reduce(Map.Int.empty, (a, x) => a->set(x, 0))
  }
}

// Key = bit pos
// value = content counts
module Total = {
  type t = MutableMap.Int.t<CountContainer.t>

  let set = MutableMap.Int.set
  let get = MutableMap.Int.get
  let getSafe = (m, k) => m->MutableMap.Int.getWithDefault(k, CountContainer.make([0, 1]))
  let forEach = MutableMap.Int.forEach
  let map = MutableMap.Int.map

  let make = MutableMap.Int.make
}

let toBinaryString = MutableMap.Int.valuesToArray

let part1 = xs => {
  let total = xs->Array.reduce(Total.make(), (a, x) => {
    // x = "10101010"
    let items = x->Js.String2.trim->Utils.splitChars
    //    let n_items = items->Array.length

    items->Array.forEachWithIndex((idx, c) => {
      //      let i = n_items - 1 - idx // lsb = idx 0
      let item = c->Utils.intFromStringExn
      let orig_total = a->Total.getSafe(idx)
      a->Total.set(idx, orig_total->CountContainer.inc(item))
    })

    a
  })

  let logAndCont = x => {
    Js.log2("binStringToInt", x)
    x
  }

  let binStringToInt = x => {
    x
    ->toBinaryString
    ->Array.map(Int.toString)
    ->Js.Array2.joinWith("")
    ->logAndCont
    ->Utils.parseInt(~x=_, ~base=2)
  }

  //  let toGamma = FP_Utils.compose(, binStringToInt)

  let gamma = total->Total.map(CountContainer.mostCommon)->binStringToInt
  Js.log2("gamma", gamma)
  let epsilon = total->Total.map(CountContainer.leastCommon)->binStringToInt
  Js.log2("epsilon", epsilon)
  gamma * epsilon
}

let parse = data => data->splitNewline

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->ignore
  2
}
