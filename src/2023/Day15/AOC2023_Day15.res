open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

let charToASCII = s => {
  s->String.codePointAt(0)->Option.getExn
}

let hash = str => {
  str
  ->String.split("")
  ->Array.reduce(0, (acc, c) => {
    //    c->(log2("c = ", _))
    //    c->charToASCII->(log2("ascii = ", _))
    //    (acc + charToASCII(c))->(log2("acc + ascii = ", _))
    ((acc + charToASCII(c)) * 17)->Int.mod(256)
  })
}

let part1 = xs => {
  xs->Array.map(hash)->Array.sum(module(Int))
}

let parse = data => data->String.trim->String.split(",")

let solvePart1 = data => {
  let d = data->parse
  d->log
  d->part1
}

let solvePart2 = data => {
  data->ignore
  2
}
