open Jest
open Expect

open RescriptCore

describe("Interval", () => {
  open AOC2023_Day5
  open Interval
  test("sort - ascending order", () => {
    let intervals = [
      {lower: BigInt.fromInt(3), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(2), upper: BigInt.fromInt(4)},
    ]
    let result = Interval.sort(intervals)
    let expected = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(2), upper: BigInt.fromInt(4)},
      {lower: BigInt.fromInt(3), upper: BigInt.fromInt(5)},
    ]

    expect(result)->toEqual(expected)
  })

  test("sort - descending order", () => {
    let intervals = [
      {lower: BigInt.fromInt(3), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(5), upper: BigInt.fromInt(7)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
    ]
    let result = Interval.sort(intervals)
    let expected = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(3), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(5), upper: BigInt.fromInt(7)},
    ]

    expect(result)->toEqual(expected)
  })

  test("sort - same lower bounds", () => {
    let intervals = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(4)},
    ]
    let result = Interval.sort(intervals)
    let expected = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(4)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)},
    ]

    expect(result)->toEqual(expected)
  })

  test("sort - empty array", () => {
    let intervals = []
    let result = Interval.sort(intervals)
    let expected = []

    expect(result)->toEqual(expected)
  })

  test("belowNotConnected - intervals are below and not connected", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)}
    let interval2 = {lower: BigInt.fromInt(4), upper: BigInt.fromInt(5)}
    let result = Interval.belowNotConnected(interval1, interval2)

    expect(result)->toBe(true)
  })

  test("belowNotConnected - intervals are connected", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)}
    let interval2 = {lower: BigInt.fromInt(3), upper: BigInt.fromInt(4)}
    let result = Interval.belowNotConnected(interval1, interval2)

    expect(result)->toBe(false)
  })

  test("belowNotConnected - intervals are overlapping", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(3)}
    let interval2 = {lower: BigInt.fromInt(2), upper: BigInt.fromInt(4)}
    let result = Interval.belowNotConnected(interval1, interval2)

    expect(result)->toBe(false)
  })

  test("belowNotConnected - intervals are above and not connected", () => {
    let interval1 = {lower: BigInt.fromInt(4), upper: BigInt.fromInt(5)}
    let interval2 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)}
    let result = Interval.belowNotConnected(interval1, interval2)

    expect(result)->toBe(false)
  })

  test("merge - overlapping intervals", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}
    let interval2 = {lower: BigInt.fromInt(3), upper: BigInt.fromInt(7)}
    let result = Interval.merge(interval1, interval2)
    let expected = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(7)}

    expect(result)->toEqual(expected)
  })

  test("merge - non-overlapping intervals", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)}
    let interval2 = {lower: BigInt.fromInt(3), upper: BigInt.fromInt(4)}
    let result = Interval.merge(interval1, interval2)
    let expected = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(4)}

    expect(result)->toEqual(expected)
  })

  test("merge - identical intervals", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}
    let interval2 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}
    let result = Interval.merge(interval1, interval2)
    let expected = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}

    expect(result)->toEqual(expected)
  })

  test("merge - interval within another interval", () => {
    let interval1 = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}
    let interval2 = {lower: BigInt.fromInt(2), upper: BigInt.fromInt(4)}
    let result = Interval.merge(interval1, interval2)
    let expected = {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}

    expect(result)->toEqual(expected)
  })

  test("sortAndMergeOverlaps - overlapping intervals", () => {
    let intervals = [
      {lower: BigInt.fromInt(3), upper: BigInt.fromInt(7)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)},
    ]
    let result = Interval.sortAndMergeOverlaps(intervals)
    let expected = [{lower: BigInt.fromInt(1), upper: BigInt.fromInt(7)}]

    expect(result)->toEqual(expected)
  })

  test("sortAndMergeOverlaps - non-overlapping connecting intervals", () => {
    let intervals = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(3), upper: BigInt.fromInt(4)},
    ]
    let result = Interval.sortAndMergeOverlaps(intervals)
    let expected = [{lower: BigInt.fromInt(1), upper: BigInt.fromInt(4)}]

    expect(result)->toEqual(expected)
  })

  test("sortAndMergeOverlaps - non-overlapping non-connecting intervals", () => {
    let intervals = [
      {lower: BigInt.fromInt(4), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
    ]
    let result = Interval.sortAndMergeOverlaps(intervals)
    let expected = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(2)},
      {lower: BigInt.fromInt(4), upper: BigInt.fromInt(5)},
    ]

    expect(result)->toEqual(expected)
  })

  test("sortAndMergeOverlaps - identical intervals", () => {
    let intervals = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)},
    ]
    let result = Interval.sortAndMergeOverlaps(intervals)
    let expected = [{lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}]

    expect(result)->toEqual(expected)
  })

  test("sortAndMergeOverlaps - interval within another interval", () => {
    let intervals = [
      {lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)},
      {lower: BigInt.fromInt(2), upper: BigInt.fromInt(4)},
    ]
    let result = Interval.sortAndMergeOverlaps(intervals)
    let expected = [{lower: BigInt.fromInt(1), upper: BigInt.fromInt(5)}]

    expect(result)->toEqual(expected)
  })
})
