open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type calibration = (bigint, array<int>)

let extendOne: (array<bigint>, bigint) => array<bigint> = (partials, next) => {
  let go = p => [p + next, p * next]
  partials->Array.flatMap(go)
}

let extend: array<bigint> => array<bigint> = arr => {
  let x = arr->Array.headUnsafe
  let xs = arr->Array.tail
  xs->Array.reduce([x], extendOne)
}

let isValid = ((target, factors)) => {
  factors->extend->Array.find(x => x == target)
}

let concatBigInt: (bigint, bigint) => bigint = (a, b) => {
  (a->BigInt.toString ++ b->BigInt.toString)->BigInt.fromStringExn
}

let extendOneC: (array<bigint>, bigint) => array<bigint> = (partials, next) => {
  let go = p => [p + next, p * next, concatBigInt(p, next)]
  partials->Array.flatMap(go)
}

let extendC: array<bigint> => array<bigint> = arr => {
  let x = arr->Array.headUnsafe
  let xs = arr->Array.tail
  xs->Array.reduce([x], extendOneC)
}

let isValidC = ((target, factors)) => {
  factors->extendC->Array.find(x => x == target)
}

@warning("-8")
let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => {
    let arr = l->String.trim->String.split(": ")
    let [first, second] = arr
    (first->BigInt.fromStringExn, second->String.split(" ")->Array.map(BigInt.fromStringExn))
  })

let sumBigIntArray = Array.reduce(_, 0n, (a: bigint, b: bigint) => a + b)

let solvePart1 = data => {
  data->parse->Array.map(isValid)->Array.keepSome->sumBigIntArray
}

let solvePart2 = data => {
  data->parse->Array.map(isValidC)->Array.keepSome->sumBigIntArray
}
