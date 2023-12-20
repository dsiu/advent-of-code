open Jest
open Expect

open RescriptCore

describe("Interval", () => {
  open Interval
  describe("length", () => {
    test(
      "length - returns correct length for positive interval",
      () => {
        let interval = (BigInt.fromInt(1), BigInt.fromInt(5))
        let expected = BigInt.fromInt(5)
        expect(Interval.length(interval))->toEqual(expected)
      },
    )

    test(
      "length - returns correct length for zero-length interval",
      () => {
        let interval = (BigInt.fromInt(3), BigInt.fromInt(3))
        let expected = BigInt.fromInt(1)
        expect(Interval.length(interval))->toEqual(expected)
      },
    )

    test(
      "length - returns correct length for negative interval",
      () => {
        let interval = (BigInt.fromInt(5), BigInt.fromInt(1))
        let expected = BigInt.fromInt(5)
        expect(Interval.length(interval))->toEqual(expected)
      },
    )
  })

  describe("contains", () => {
    test(
      "contains - returns true when number is within the interval",
      () => {
        let interval = (BigInt.fromInt(5), BigInt.fromInt(10))
        let num = BigInt.fromInt(7)
        expect(contains(interval, num))->toBe(true)
      },
    )

    test(
      "contains - returns true when number is equal to the lower bound",
      () => {
        let interval = (BigInt.fromInt(5), BigInt.fromInt(10))
        let num = BigInt.fromInt(5)
        expect(contains(interval, num))->toBe(true)
      },
    )

    test(
      "contains - returns true when number is equal to the upper bound",
      () => {
        let interval = (BigInt.fromInt(5), BigInt.fromInt(10))
        let num = BigInt.fromInt(10)
        expect(contains(interval, num))->toBe(true)
      },
    )

    test(
      "contains - returns false when number is less than the lower bound",
      () => {
        let interval = (BigInt.fromInt(5), BigInt.fromInt(10))
        let num = BigInt.fromInt(4)
        expect(contains(interval, num))->toBe(false)
      },
    )

    test(
      "contains - returns false when number is greater than the upper bound",
      () => {
        let interval = (BigInt.fromInt(5), BigInt.fromInt(10))
        let num = BigInt.fromInt(11)
        expect(contains(interval, num))->toBe(false)
      },
    )
  })

  describe("isOverlap", () => {
    test(
      "isOverlap - returns true when intervals overlap",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(7), BigInt.fromInt(12))
        expect(isOverlap(interval1, interval2))->toBe(true)
      },
    )

    test(
      "isOverlap - returns false when intervals do not overlap",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(11), BigInt.fromInt(15))
        expect(isOverlap(interval1, interval2))->toBe(false)
      },
    )

    test(
      "isOverlap - returns true when intervals touch at one point",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(10), BigInt.fromInt(15))
        expect(isOverlap(interval1, interval2))->toBe(true)
      },
    )
  })

  describe("intersect", () => {
    test(
      "intersect - returns intersection when intervals overlap",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(7), BigInt.fromInt(12))
        expect(intersect(interval1, interval2))->toEqual(
          Some(make(BigInt.fromInt(7), BigInt.fromInt(10))),
        )
      },
    )

    test(
      "intersect - returns None when intervals do not overlap",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(11), BigInt.fromInt(15))
        expect(intersect(interval1, interval2))->toBe(None)
      },
    )

    test(
      "intersect - returns intersection when intervals touch at one point",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(10), BigInt.fromInt(15))
        expect(intersect(interval1, interval2))->toEqual(
          Some(make(BigInt.fromInt(10), BigInt.fromInt(10))),
        )
      },
    )
  })

  describe("below", () => {
    test(
      "below - returns true when first interval is below the second",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(12), BigInt.fromInt(15))
        expect(below(interval1, interval2))->toBe(true)
      },
    )

    test(
      "below - returns false when first interval is above the second",
      () => {
        let interval1 = make(BigInt.fromInt(12), BigInt.fromInt(15))
        let interval2 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        expect(below(interval1, interval2))->toBe(false)
      },
    )

    test(
      "below - returns false when intervals overlap",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(7), BigInt.fromInt(12))
        expect(below(interval1, interval2))->toBe(false)
      },
    )
  })

  describe("adjacent", () => {
    test(
      "adjacent - returns true when intervals are adjacent and not overlapping",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(11), BigInt.fromInt(15))
        expect(adjacent(interval1, interval2))->toBe(true)
      },
    )

    test(
      "adjacent - returns false when intervals are overlapping",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(7), BigInt.fromInt(12))
        expect(adjacent(interval1, interval2))->toBe(false)
      },
    )

    test(
      "adjacent - returns false when intervals are not adjacent and not overlapping",
      () => {
        let interval1 = make(BigInt.fromInt(5), BigInt.fromInt(10))
        let interval2 = make(BigInt.fromInt(15), BigInt.fromInt(20))
        expect(adjacent(interval1, interval2))->toBe(false)
      },
    )
  })

  describe("Sort", () => {
    test(
      "sort - sorts intervals by lower bound ascending, then upper bound ascending",
      () => {
        let intervals = [
          (BigInt.fromInt(3), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(4)),
        ]
        let expected = [
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(4)),
          (BigInt.fromInt(3), BigInt.fromInt(5)),
        ]
        expect(Interval.sort(intervals))->toEqual(expected)
      },
    )

    test(
      "sort - handles equal lower bounds by sorting by upper bound",
      () => {
        let intervals = [
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(4)),
        ]
        let expected = [
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(4)),
          (BigInt.fromInt(1), BigInt.fromInt(5)),
        ]
        expect(Interval.sort(intervals))->toEqual(expected)
      },
    )

    test(
      "sort - handles equal intervals correctly",
      () => {
        let intervals = [
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
        ]
        let expected = [
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
        ]
        expect(Interval.sort(intervals))->toEqual(expected)
      },
    )

    test(
      "sort - handles empty array correctly",
      () => {
        let intervals = []
        let expected = []
        expect(Interval.sort(intervals))->toEqual(expected)
      },
    )
  })

  describe("merge", () => {
    test(
      "merge - merges two overlapping intervals",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(3))
        let interval2 = (BigInt.fromInt(2), BigInt.fromInt(4))
        let expected = (BigInt.fromInt(1), BigInt.fromInt(4))
        expect(Interval.merge(interval1, interval2))->toEqual(expected)
      },
    )

    test(
      "merge - merges two connected intervals",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(2))
        let interval2 = (BigInt.fromInt(3), BigInt.fromInt(4))
        let expected = (BigInt.fromInt(1), BigInt.fromInt(4))
        expect(Interval.merge(interval1, interval2))->toEqual(expected)
      },
    )

    test(
      "merge - throws error when intervals are not connected or overlapping",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(2))
        let interval2 = (BigInt.fromInt(4), BigInt.fromInt(5))
        let mergeFunction = () => Interval.merge(interval1, interval2)
        expect(mergeFunction)->toThrow
      },
    )

    test(
      "merge - interval within another interval",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(5))
        let interval2 = (BigInt.fromInt(2), BigInt.fromInt(4))
        let expected = (BigInt.fromInt(1), BigInt.fromInt(5))
        expect(Interval.merge(interval1, interval2))->toEqual(expected)
      },
    )
  })

  describe("sortAndMergeOverlaps", () => {
    test(
      "sortAndMergeOverlaps - overlapping intervals",
      () => {
        let intervals = [
          (BigInt.fromInt(3), BigInt.fromInt(7)),
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(2), BigInt.fromInt(6)),
          (BigInt.fromInt(4), BigInt.fromInt(8)),
        ]
        let result = Interval.sortAndMergeOverlaps(intervals)
        let expected = [(BigInt.fromInt(1), BigInt.fromInt(8))]

        expect(result)->toEqual(expected)
      },
    )

    test(
      "sortAndMergeOverlaps - non-overlapping connecting intervals",
      () => {
        let intervals = [
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(3), BigInt.fromInt(4)),
          (BigInt.fromInt(5), BigInt.fromInt(6)),
          (BigInt.fromInt(7), BigInt.fromInt(8)),
        ]
        let result = Interval.sortAndMergeOverlaps(intervals)
        let expected = [(BigInt.fromInt(1), BigInt.fromInt(8))]

        expect(result)->toEqual(expected)
      },
    )

    test(
      "sortAndMergeOverlaps - non-overlapping non-connecting intervals",
      () => {
        let intervals = [
          (BigInt.fromInt(4), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(10), BigInt.fromInt(11)),
          (BigInt.fromInt(7), BigInt.fromInt(8)),
        ]
        let result = Interval.sortAndMergeOverlaps(intervals)
        let expected = [
          (BigInt.fromInt(1), BigInt.fromInt(2)),
          (BigInt.fromInt(4), BigInt.fromInt(5)),
          (BigInt.fromInt(7), BigInt.fromInt(8)),
          (BigInt.fromInt(10), BigInt.fromInt(11)),
        ]

        expect(result)->toEqual(expected)
      },
    )

    test(
      "sortAndMergeOverlaps - some overlapping some connecting intervals",
      () => {
        let intervals = [
          (BigInt.fromInt(3), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(4)),
          (BigInt.fromInt(9), BigInt.fromInt(10)),
          (BigInt.fromInt(7), BigInt.fromInt(8)),
        ]
        let result = Interval.sortAndMergeOverlaps(intervals)
        let expected = [
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(7), BigInt.fromInt(10)),
        ]

        expect(result)->toEqual(expected)
      },
    )

    test(
      "sortAndMergeOverlaps - identical intervals",
      () => {
        let intervals = [
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(1), BigInt.fromInt(5)),
        ]
        let result = Interval.sortAndMergeOverlaps(intervals)
        let expected = [(BigInt.fromInt(1), BigInt.fromInt(5))]

        expect(result)->toEqual(expected)
      },
    )

    test(
      "sortAndMergeOverlaps - interval within another interval",
      () => {
        let intervals = [
          (BigInt.fromInt(1), BigInt.fromInt(5)),
          (BigInt.fromInt(2), BigInt.fromInt(4)),
          (BigInt.fromInt(3), BigInt.fromInt(3)),
          (BigInt.fromInt(1), BigInt.fromInt(5)),
        ]
        let result = Interval.sortAndMergeOverlaps(intervals)
        let expected = [(BigInt.fromInt(1), BigInt.fromInt(5))]

        expect(result)->toEqual(expected)
      },
    )
  })
})
