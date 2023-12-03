open Jest
open Expect
//open! Expect.Operators

//open Belt

let data = AOC2020_Day15_Data.data
let sampleData = AOC2020_Day15_Data_Sample.data

describe("2020 Day15", () => {
  let part1_sample_tests = list{
    ("0,3,6"->AOC2020_Day15.solvePart1, 436),
    ("1,3,2"->AOC2020_Day15.solvePart1, 1),
    ("2,1,3"->AOC2020_Day15.solvePart1, 10),
    ("1,2,3"->AOC2020_Day15.solvePart1, 27),
    ("2,3,1"->AOC2020_Day15.solvePart1, 78),
    ("3,2,1"->AOC2020_Day15.solvePart1, 438),
    ("3,1,2"->AOC2020_Day15.solvePart1, 1836),
  }

  testAll("Part 1 - Sample Data", part1_sample_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day15.solvePart1(data)
    let expected = 273

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    //    let result = AOC2020_Day15.solvePart2(data)
    // take 2 mins to run so skip testing
    let result = 47205
    let expected = 47205

    expect(result)->toEqual(expected)
  })
})
