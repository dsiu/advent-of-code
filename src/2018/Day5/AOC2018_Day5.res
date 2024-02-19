@@uncurried
@@uncurried.swap

let data = AOC2018_Day5_Data.data
let sampleData = AOC2018_Day5_Data_Sample.data
open Belt
//open Utils

let log = Js.Console.log

let charArray = data->Js.String2.split("")
let charList = charArray->List.fromArray

let testCharArray = sampleData->Js.String2.split("")
let testCharList = testCharArray->List.fromArray

// charList -> List.forEach(Console.log)
// da bA cC aC BA cC ca DA
let groupByN = (l, n) => {
  let (r, _cur) = l->List.reduce((list{}, list{}), (a, x) => {
    let (r, cur) = a
    cur->List.length == n - 1
      ? (List.add(r, cur->List.add(x)->List.reverse), list{})
      : (r, cur->List.add(x))
  })
  r->List.reverse
}

let groupBy2 = groupByN(_, 2)
let groupBy3 = groupByN(_, 3)

// let charList2 = charList->groupBy2

let fuse = pair => {
  // let a = pair->List.get(0)->Option.getExn
  // let b = pair->List.get(1)->Option.getExn
  let (a, b) = pair
  open Js
  a !== b &&
    ((a === b->String2.toLowerCase && b === a->String2.toUpperCase) ||
      (b === a->String2.toLowerCase && a === b->String2.toUpperCase))
    ? true
    : false
}

let findPairIndex = l => {
  let (_last, _has_last, found, founded_idx) = l->List.reduceWithIndex(("", false, false, -1), (
    a,
    x,
    i,
  ) => {
    switch a {
    | (_, _, true, _) => a
    | (_last, false, false, _) => (x, true, false, -1)
    | (last, true, false, _) => fuse((last, x)) ? ("", false, true, i - 1) : (x, true, false, -1)
    }
  })
  found ? Some(founded_idx) : None
}

let findPairIndex_array = l => {
  let rec helper = (l, i, len, last, has_last) => {
    let x = l->Array.get(i)
    let cont = i < len
    switch (last, has_last, cont) {
    | (_, _, false) => None
    | (_last, false, true) => helper(l, i + 1, len, x, true)
    | (last, true, true) =>
      fuse((last->Option.getExn, x->Option.getExn)) ? Some(i - 1) : helper(l, i + 1, len, x, true)
    }
  }

  helper(l, 0, l->Array.length, None, false)
}

// i=3, remove d, e
// [0,1,2,3,4,5,6]
// [a,b,c,d,e,f,g]
// h=[a,b,c] List.take(3)
// t=[f,g] List.drop(5)

let rec defuse = l => {
  switch findPairIndex(l) {
  | Some(i) => {
      let h = l->List.take(i)->Option.getExn
      let t = l->List.drop(i + 2)->Option.getExn
      List.concat(h, t)->defuse
    }
  | None => l
  }
}

let rec defuse_array = l => {
  switch findPairIndex_array(l) {
  | Some(i) => {
      let h = l->Array.slice(~offset=0, ~len=i)
      let t = l->Array.sliceToEnd(i + 2)
      Array.concat(h, t)->defuse_array
    }
  | None => l
  }
}

let isLetterAndUpper = (which, c) => {
  let u = which->Js.String2.toUpperCase
  let l = which->Js.String2.toLowerCase
  // log(`which=${which}`)
  // log(`u=${u} l=${l}`)
  // log(`c=${c}`)
  c == u || c == l
}

let notIsLetterAndUpper = (. which, c) => {!(c->isLetterAndUpper(which))}

let aToz = [
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z",
]
let aTod = ["a", "b", "c", "d"]

let makeATozData = (letters, data) => {
  letters->Array.reduce(Map.String.empty, (a, c) => {
    let v = data->Array.keep(notIsLetterAndUpper(c, ...))
    // log(`c=${c}`)
    // v->log
    a->Map.String.set(c, v)
  })
}

// fuse(list{"a", "a"})->Utils.list_dump

let solvePart1 = _d => 240
let solvePart2 = (polymars, d) => {
  makeATozData(polymars, d)
  ->Map.String.map(defuse_array)
  ->Map.String.map(Array.length)
  ->Map.String.reduce(Js.Int.max, (a, _k, v) => {
    v < a ? v : a
  })
}
