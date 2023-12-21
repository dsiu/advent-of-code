//
// Todo: - make Interval generic that works with float / Int / BigInt
//
open RescriptCore
open BigInt

let log = Console.log
let log2 = Console.log2

// lower and upper bound are inclusive
type t = (BigInt.t, BigInt.t)

let toString: t => string = ((lower, upper)) => {
  `Interval(${lower->BigInt.toString}, ${upper->BigInt.toString})`
}

let make: (BigInt.t, BigInt.t) => t = (lower, upper) => {
  lower > upper ? (upper, lower) : (lower, upper)
}

let makeWithLength: (BigInt.t, ~length: BigInt.t) => t = (lower, ~length) => {
  if length < BigInt.fromInt(0) {
    failwith("length must be non-negative")
  }
  open! BigInt
  let upper = lower + length - fromInt(1)
  make(lower, upper)
}

let length = ((lower, upper)) => {
  open! BigInt
  upper > lower ? upper - lower + fromInt(1) : lower - upper + fromInt(1)
}

let equals = ((aLower, aUpper), (bLower, bUpper)) => {
  aLower === bLower && aUpper === bUpper
}

let contains: (t, BigInt.t) => bool = ((lower, upper), num) => {
  num >= lower && num <= upper
}

let isOverlap: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  contains(a, bLower) || contains(a, bUpper) || contains(b, aLower) || contains(b, aUpper)
}

let intersect: (t, t) => option<t> = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  isOverlap(a, b) ? make(max(aLower, bLower), min(aUpper, bUpper))->Some : None
}

let below: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  aUpper < bLower
}

// a is adjacent to b but not overlapping asssuming a is below b
let adjacent: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  // make sure t1 is all below t2 and both are not connected
  add(aUpper, fromInt(1)) === bLower || add(bUpper, fromInt(1)) === aLower
}

let belowAndAdjacent: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  below(a, b) && adjacent(a, b)
}

let add: (t, BigInt.t) => t = ((lower, upper), num) => {
  make(lower + num, upper + num)
}

// remove interval b from a
let remove: (t, t) => option<t> = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  switch (
    isOverlap(a, b),
    equals(a, b),
    bUpper >= aUpper && contains(a, bLower),
    bLower <= aLower && contains(a, bUpper),
  ) {
  | (false, _, _, _) => Some(a)
  | (_, true, _, _) => None
  | (_, _, true, _) => Some((aLower, bLower - BigInt.fromInt(1)))
  | (_, _, _, true) => Some((bUpper + BigInt.fromInt(1), aUpper))
  | _ => None
  }
}

// requirement range a must be below range b and a is adjacent to b
let merge: (t, t) => t = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  adjacent(a, b) || isOverlap(a, b)
    ? {
        let lower = min(aLower, bLower)
        let upper = max(aUpper, bUpper)
        make(lower, upper)
      }
    : failwith("intervals must be adjacent or overlapping")
}

// sort intervals by lower bound ascending, then upper bound ascending
let sort: array<t> => array<t> = intervals => {
  let lowerBoundAscendingCmp = ((aLower, aUpper), (bLower, bUpper)) => {
    switch (aLower, bLower, aUpper, bUpper) {
    | (lower1, lower2, _, _) if lower1 < lower2 => Core__Ordering.less
    | (lower1, lower2, _, _) if lower1 > lower2 => Core__Ordering.greater
    | (_, _, upper1, upper2) if upper1 < upper2 => Core__Ordering.less
    | (_, _, upper1, upper2) if upper1 > upper2 => Core__Ordering.greater
    | _ => Core__Ordering.equal
    }
  }

  intervals->Array.toSorted(lowerBoundAscendingCmp)
}

let sortAndMergeOverlaps: array<t> => array<t> = intervals => {
  let sortedIntervals = intervals->sort
  let rec loop: array<t> => array<t> = sortedIntervals =>
    switch sortedIntervals {
    | [] => []
    | [interval] => [interval]
    | _ as xs => {
        let a = xs->Array.getUnsafe(0)
        let b = xs->Array.getUnsafe(1)
        below(a, b) && !adjacent(a, b)
          ? Array.concat([a], loop(xs->Array.sliceToEnd(~start=1)))
          : loop(Array.concat([merge(a, b)], xs->Array.sliceToEnd(~start=2)))
      }
    }
  loop(sortedIntervals)
}
