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

/**
 * This function creates an interval from two BigInts.
 * It takes two BigInts as input and returns a tuple of two BigInts.
 * The first BigInt in the tuple is the smaller of the two input BigInts, and the second BigInt is the larger of the two input BigInts.
 * If the two input BigInts are equal, it returns a tuple of two equal BigInts.
 *
 * @param {BigInt.t} lower - The first BigInt to create the interval from.
 * @param {BigInt.t} upper - The second BigInt to create the interval from.
 * @returns {t} - The created interval. It is a tuple of two BigInts. The first BigInt is the smaller of the two input BigInts, and the second BigInt is the larger of the two input BigInts.
 *
 * @example
 * let lower = BigInt.fromInt(1)
 * let upper = BigInt.fromInt(2)
 * let interval = make(lower, upper)
 * // interval is now a tuple (1, 2)
 */
let make: (BigInt.t, BigInt.t) => t = (lower, upper) => {
  lower > upper ? (upper, lower) : (lower, upper)
}

/**
 * Creates an interval from a lower bound and a length.
 * The function fails if the length is negative.
 *
 * @param {BigInt.t} lower - The lower bound of the interval.
 * @param {BigInt.t} length - The length of the interval.
 * @returns {t} - The created interval.
 */
let makeWithLength: (BigInt.t, ~length: BigInt.t) => t = (lower, ~length) => {
  if length < BigInt.fromInt(0) {
    failwith("length must be non-negative")
  }
  open! BigInt
  let upper = lower + length - fromInt(1)
  make(lower, upper)
}

/**
 * This function calculates the length of an interval.
 * It takes an interval as input and returns the length of the interval.
 * The length of the interval is the difference between the upper and lower bounds plus 1.
 * If the upper bound is greater than the lower bound, it subtracts the lower bound from the upper bound and adds 1.
 * If the upper bound is not greater than the lower bound, it subtracts the upper bound from the lower bound and adds 1.
 *
 * @param {t} interval - The interval to calculate the length of. It is a tuple of two BigInts.
 * @returns {BigInt.t} - The length of the interval. It is a BigInt.
 *
 * @example
 * let interval = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let length = Interval.length(interval)
 * // length is now a BigInt 2
 */
let length = ((lower, upper)) => {
  open! BigInt
  upper > lower ? upper - lower + fromInt(1) : lower - upper + fromInt(1)
}

let equals = ((aLower, aUpper), (bLower, bUpper)) => {
  aLower === bLower && aUpper === bUpper
}

/**
 * This function checks if a number is contained within an interval.
 * It takes an interval and a number as input and returns a boolean.
 * If the number is greater than or equal to the lower bound of the interval and less than or equal to the upper bound of the interval, it returns true.
 * Otherwise, it returns false.
 *
 * @param {t} interval - The interval to check. It is a tuple of two BigInts.
 * @param {BigInt.t} num - The number to check if it is contained within the interval.
 * @returns {bool} - A boolean indicating whether the number is contained within the interval.
 *
 * @example
 * let interval = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let num = BigInt.fromInt(1)
 * let isContained = Interval.contains(interval, num)
 * // isContained is now true
 */
let contains: (t, BigInt.t) => bool = ((lower, upper), num) => {
  num >= lower && num <= upper
}

/**
 * This function checks if two intervals overlap.
 * It takes two intervals as input and returns a boolean.
 * If any of the bounds of the first interval is contained within the second interval or vice versa, it returns true.
 * Otherwise, it returns false.
 *
 * @param {t} a - The first interval to check. It is a tuple of two BigInts.
 * @param {t} b - The second interval to check. It is a tuple of two BigInts.
 * @returns {bool} - A boolean indicating whether the two intervals overlap.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let interval2 = Interval.make(BigInt.fromInt(2), BigInt.fromInt(3))
 * let isOverlap = Interval.isOverlap(interval1, interval2)
 * // isOverlap is now true
 */
let isOverlap: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  contains(a, bLower) || contains(a, bUpper) || contains(b, aLower) || contains(b, aUpper)
}

/**
 * This function calculates the intersection of two intervals.
 * It takes two intervals as input and returns an optional interval.
 * If the two intervals overlap, it creates a new interval from the maximum lower bound and the minimum upper bound of the two intervals and returns it wrapped in a `Some`.
 * If the two intervals do not overlap, it returns `None`.
 *
 * @param {t} a - The first interval to calculate the intersection of. It is a tuple of two BigInts.
 * @param {t} b - The second interval to calculate the intersection of. It is a tuple of two BigInts.
 * @returns {option<t>} - The intersection of the two intervals. It is an optional interval. If the two intervals overlap, it is `Some` of the intersection, otherwise it is `None`.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(3))
 * let interval2 = Interval.make(BigInt.fromInt(2), BigInt.fromInt(4))
 * let intersection = Interval.intersect(interval1, interval2)
 * // intersection is now `Some(Interval(2, 3))`
 */
let intersect: (t, t) => option<t> = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  isOverlap(a, b) ? make(max(aLower, bLower), min(aUpper, bUpper))->Some : None
}

/**
 * This function checks if the first interval is below the second interval.
 * It takes two intervals as input and returns a boolean.
 * If the upper bound of the first interval is less than the lower bound of the second interval, it returns true.
 * Otherwise, it returns false.
 *
 * @param {t} a - The first interval to check. It is a tuple of two BigInts.
 * @param {t} b - The second interval to check. It is a tuple of two BigInts.
 * @returns {bool} - A boolean indicating whether the first interval is below the second interval.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let interval2 = Interval.make(BigInt.fromInt(3), BigInt.fromInt(4))
 * let isBelow = Interval.below(interval1, interval2)
 * // isBelow is now true
 */
let below: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  aUpper < bLower
}

/**
 * This function checks if two intervals are adjacent and not overlapping.
 * It assumes that the first interval is below the second interval.
 * It takes two intervals as input and returns a boolean.
 * If the upper bound of the first interval plus 1 is equal to the lower bound of the second interval or the upper bound of the second interval plus 1 is equal to the lower bound of the first interval, it returns true.
 * Otherwise, it returns false.
 *
 * @param {t} a - The first interval to check. It is a tuple of two BigInts.
 * @param {t} b - The second interval to check. It is a tuple of two BigInts.
 * @returns {bool} - A boolean indicating whether the two intervals are adjacent and not overlapping.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let interval2 = Interval.make(BigInt.fromInt(3), BigInt.fromInt(4))
 * let isAdjacent = Interval.adjacent(interval1, interval2)
 * // isAdjacent is now true
 */
let adjacent: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  // make sure t1 is all below t2 and both are not connected
  add(aUpper, fromInt(1)) === bLower || add(bUpper, fromInt(1)) === aLower
}

/**
 * This function checks if the first interval is below and adjacent to the second interval.
 * It takes two intervals as input and returns a boolean.
 * If the first interval is below the second interval and the two intervals are adjacent, it returns true.
 * Otherwise, it returns false.
 *
 * @param {t} a - The first interval to check. It is a tuple of two BigInts.
 * @param {t} b - The second interval to check. It is a tuple of two BigInts.
 * @returns {bool} - A boolean indicating whether the first interval is below and adjacent to the second interval.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let interval2 = Interval.make(BigInt.fromInt(3), BigInt.fromInt(4))
 * let isBelowAndAdjacent = Interval.belowAndAdjacent(interval1, interval2)
 * // isBelowAndAdjacent is now true
 */
let belowAndAdjacent: (t, t) => bool = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  below(a, b) && adjacent(a, b)
}

/**
 * This function adds a number to the lower and upper bounds of an interval.
 * It takes an interval and a number as input and returns a new interval.
 * The lower and upper bounds of the new interval are the lower and upper bounds of the input interval plus the input number.
 *
 * @param {t} interval - The interval to add the number to. It is a tuple of two BigInts.
 * @param {BigInt.t} num - The number to add to the interval.
 * @returns {t} - The new interval. It is a tuple of two BigInts. The lower and upper bounds of the new interval are the lower and upper bounds of the input interval plus the input number.
 *
 * @example
 * let interval = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let num = BigInt.fromInt(1)
 * let newInterval = Interval.add(interval, num)
 * // newInterval is now a tuple (2, 3)
 */
let add: (t, BigInt.t) => t = ((lower, upper), num) => {
  make(lower + num, upper + num)
}

/**
 * This function removes one interval from another.
 * It takes two intervals as input and returns an optional interval.
 * If the two intervals do not overlap, it returns the first interval wrapped in a `Some`.
 * If the two intervals are equal, it returns `None`.
 * If the upper bound of the second interval is greater than or equal to the upper bound of the first interval and the first interval contains the lower bound of the second interval, it returns a new interval from the lower bound of the first interval to the lower bound of the second interval minus 1 wrapped in a `Some`.
 * If the lower bound of the second interval is less than or equal to the lower bound of the first interval and the first interval contains the upper bound of the second interval, it returns a new interval from the upper bound of the second interval plus 1 to the upper bound of the first interval wrapped in a `Some`.
 * Otherwise, it returns `None`.
 *
 * @param {t} a - The first interval to remove from. It is a tuple of two BigInts.
 * @param {t} b - The second interval to remove. It is a tuple of two BigInts.
 * @returns {option<t>} - The result of removing the second interval from the first interval. It is an optional interval.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(3))
 * let interval2 = Interval.make(BigInt.fromInt(2), BigInt.fromInt(4))
 * let removed = Interval.remove(interval1, interval2)
 * // removed is now `Some(Interval(1, 1))`
 */
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

/**
 * This function merges two intervals into one.
 * It takes two intervals as input and returns a new interval.
 * The function requires that the first interval is below the second interval and the two intervals are either adjacent or overlapping.
 * If the two intervals are adjacent or overlapping, it creates a new interval from the minimum lower bound and the maximum upper bound of the two intervals.
 * If the two intervals are not adjacent or overlapping, it throws an error.
 *
 * @param {t} a - The first interval to merge. It is a tuple of two BigInts.
 * @param {t} b - The second interval to merge. It is a tuple of two BigInts.
 * @returns {t} - The merged interval. It is a tuple of two BigInts. The lower bound is the minimum lower bound of the two input intervals, and the upper bound is the maximum upper bound of the two input intervals.
 *
 * @example
 * let interval1 = Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))
 * let interval2 = Interval.make(BigInt.fromInt(2), BigInt.fromInt(3))
 * let merged = Interval.merge(interval1, interval2)
 * // merged is now a tuple (1, 3)
 */
let merge: (t, t) => t = ((aLower, aUpper) as a, (bLower, bUpper) as b) => {
  adjacent(a, b) || isOverlap(a, b)
    ? {
        let lower = min(aLower, bLower)
        let upper = max(aUpper, bUpper)
        make(lower, upper)
      }
    : failwith("intervals must be adjacent or overlapping")
}

/**
 * This function sorts an array of intervals in ascending order by their lower bounds, then by their upper bounds.
 * It takes an array of intervals as input and returns a new array of sorted intervals.
 * The sorting is done in two steps. First, it sorts the intervals by their lower bounds in ascending order. If two intervals have the same lower bound, it sorts them by their upper bounds in ascending order.
 *
 * @param {array<t>} intervals - The array of intervals to sort. Each interval is a tuple of two BigInts.
 * @returns {array<t>} - The sorted array of intervals. Each interval is a tuple of two BigInts.
 *
 * @example
 * let intervals = [Interval.make(BigInt.fromInt(2), BigInt.fromInt(3)), Interval.make(BigInt.fromInt(1), BigInt.fromInt(2))]
 * let sortedIntervals = Interval.sort(intervals)
 * // sortedIntervals is now an array of intervals [Interval(1, 2), Interval(2, 3)]
 */
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
/**
 * This function sorts an array of intervals in ascending order and merges any overlapping intervals into one.
 * It takes an array of intervals as input and returns a new array of sorted and merged intervals.
 * The function first sorts the intervals in ascending order by their lower bounds, then by their upper bounds.
 * Then it iterates over the sorted intervals and for each pair of intervals, it checks if the first interval is below and not adjacent to the second interval.
 * If the first interval is below and not adjacent to the second interval, it keeps the first interval and continues with the next interval.
 * If the first interval is not below or is adjacent to the second interval, it merges the two intervals into one and continues with the next pair of intervals.
 * The function returns a new array of sorted and merged intervals.
 *
 * @param {array<t>} intervals - The array of intervals to sort and merge. Each interval is a tuple of two BigInts.
 * @returns {array<t>} - The array of sorted and merged intervals. Each interval is a tuple of two BigInts.
 *
 * @example
 * let intervals = [Interval.make(BigInt.fromInt(1), BigInt.fromInt(3)), Interval.make(BigInt.fromInt(2), BigInt.fromInt(4))]
 * let sortedAndMergedIntervals = Interval.sortAndMergeOverlaps(intervals)
 * // sortedAndMergedIntervals is now an array of intervals [Interval(1, 4)]
 */
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
