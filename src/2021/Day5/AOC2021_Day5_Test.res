open Jest
open Expect
//open! Expect.Operators
open Jest2
//open Belt

let data = AOC2021_Day5_Data.data
let sampleData = AOC2021_Day5_Data_Sample.data

describe("2021 Day5", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day5.solvePart1(sampleData)
    let expected = 5

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day5.solvePart1(data)
    let expected = 7085

    expect(result)->toEqual(expected)
  })

  open AOC2021_Day5

  let point_tests = [
    (
      Line.makePoints({Point.x: 0, y: 0}, {Point.x: 0, y: 2}),
      //      [{Point.x: 0, y: 2}],
      [{Point.x: 0, y: 0}, {Point.x: 0, y: 1}, {Point.x: 0, y: 2}],
    ),
    (
      Line.makePoints({Point.x: 3, y: 3}, {Point.x: 5, y: 3}),
      //      [{Point.x: 0, y: 2}],
      [{Point.x: 3, y: 3}, {Point.x: 4, y: 3}, {Point.x: 5, y: 3}],
    ),
    (
      Line.makePoints({Point.x: 9, y: 7}, {Point.x: 7, y: 9}),
      //      [{Point.x: 0, y: 2}],
      [{Point.x: 9, y: 7}, {Point.x: 8, y: 8}, {Point.x: 7, y: 9}],
    ),
  ]

  testEach2("makePoints", point_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day5.solvePart2(sampleData)
    let expected = 12

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day5.solvePart2(data)
    let expected = 20271

    expect(result)->toEqual(expected)
  })
})
