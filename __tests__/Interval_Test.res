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

  describe("belowNotConnected", () => {
    test(
      "belowNotConnected - intervals are below and not connected",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(2))
        let interval2 = (BigInt.fromInt(4), BigInt.fromInt(5))
        expect(Interval.adjacent(interval1, interval2))->toBe(true)
      },
    )

    test(
      "belowNotConnected - intervals are connected",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(2))
        let interval2 = (BigInt.fromInt(3), BigInt.fromInt(4))
        expect(Interval.adjacent(interval1, interval2))->toBe(false)
      },
    )

    test(
      "belowNotConnected - intervals are overlapping",
      () => {
        let interval1 = (BigInt.fromInt(1), BigInt.fromInt(3))
        let interval2 = (BigInt.fromInt(2), BigInt.fromInt(4))
        expect(Interval.adjacent(interval1, interval2))->toBe(false)
      },
    )

    test(
      "belowNotConnected - intervals are above and not connected",
      () => {
        let interval1 = (BigInt.fromInt(4), BigInt.fromInt(5))
        let interval2 = (BigInt.fromInt(1), BigInt.fromInt(2))
        expect(Interval.adjacent(interval1, interval2))->toBe(false)
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
