//
// Todo: - make Interval generic that works with float / Int / BigInt
//
open RescriptCore
open BigInt

// lower and upper bound are inclusive
type t = {
  lower: BigInt.t,
  upper: BigInt.t,
}

let toString: t => string = t => {
  `Interval(${t.lower->BigInt.toString}, ${t.upper->BigInt.toString})`
}

let makeWithBounds: (BigInt.t, BigInt.t) => t = (lower, upper) => {
  if lower > upper {
    failwith("lower bound must be less than or equal to upper bound")
  }
  {lower, upper}
}

let makeWithLength: (BigInt.t, ~length: BigInt.t) => t = (lower, ~length) => {
  if length < fromInt(0) {
    failwith("length must be non-negative")
  }
  let upper = lower + length - fromInt(1)
  makeWithBounds(lower, upper)
}

let belowNotConnected: (t, t) => bool = (a, b) => {
  // make sure t1 is all below t2 and both are not connected
  a.upper + fromInt(1) < b.lower
}

// requirement range a must be below range b
let merge: (t, t) => t = (a, b) => {
  belowNotConnected(a, b)
    ? failwith("intervals must be all below and not connected")
    : {
        let lower = min(a.lower, b.lower)
        let upper = max(a.upper, b.upper)
        makeWithBounds(lower, upper)
      }
}

let inInterval: (t, BigInt.t) => bool = (t, num) => {
  num >= t.lower && num <= t.upper
}

// sort intervals by lower bound ascending, then upper bound ascending
let sort: array<t> => array<t> = intervals => {
  let lowerBoundAscendingCmp = (a, b) => {
    switch (a.lower, b.lower, a.upper, b.upper) {
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
        belowNotConnected(a, b)
          ? Array.concat([a], loop(xs->Array.sliceToEnd(~start=1)))
          : loop(Array.concat([merge(a, b)], xs->Array.sliceToEnd(~start=2)))
      }
    }
  loop(sortedIntervals)
}
