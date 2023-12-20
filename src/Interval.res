//
// Todo: - make Interval generic that works with float / Int / BigInt
//
open RescriptCore
open BigInt

// lower and upper bound are inclusive
type t = (BigInt.t, BigInt.t)

let toString: t => string = ((lower, upper)) => {
  `Interval(${lower->BigInt.toString}, ${upper->BigInt.toString})`
}

let make: (BigInt.t, BigInt.t) => t = (lower, upper) => {
  lower > upper ? (upper, lower) : (lower, upper)
}

let makeWithLength: (BigInt.t, ~length: BigInt.t) => t = (lower, ~length) => {
  if length < fromInt(0) {
    failwith("length must be non-negative")
  }
  let upper = lower + length - fromInt(1)
  make(lower, upper)
}

let length = ((lower, upper)) => {
  upper > lower ? upper - lower + fromInt(1) : lower - upper + fromInt(1)
}

let contains: (t, BigInt.t) => bool = ((lower, upper), num) => {
  num >= lower && num <= upper
}

let overlaps: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  contains(a, bLower) || contains(a, bUpper)
}

// a is adjacent to b but not overlapping asssuming a is below b
let adjacent: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  // make sure t1 is all below t2 and both are not connected
  aUpper + fromInt(1) < bLower
}

// requirement range a must be below range b and a is adjacent to b
let merge: (t, t) => t = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  adjacent(a, b)
    ? failwith("intervals must be all below and not connected")
    : {
        let lower = min(aLower, bLower)
        let upper = max(aUpper, bUpper)
        make(lower, upper)
      }
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
        adjacent(a, b)
          ? Array.concat([a], loop(xs->Array.sliceToEnd(~start=1)))
          : loop(Array.concat([merge(a, b)], xs->Array.sliceToEnd(~start=2)))
      }
    }
  loop(sortedIntervals)
}
