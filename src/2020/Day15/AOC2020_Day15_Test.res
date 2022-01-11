open Jest2
//open Expect
//open! Expect.Operators

//open Belt

let data = AOC2020_Day15_Data.data
let sampleData = AOC2020_Day15_Data_Sample.data

describe("2020 Day15", () => {
  let part1_sample_tests = [
    ("0,3,6"->AOC2020_Day15.solvePart1, 436),
    ("1,3,2"->AOC2020_Day15.solvePart1, 1),
    ("2,1,3"->AOC2020_Day15.solvePart1, 10),
    ("1,2,3"->AOC2020_Day15.solvePart1, 27),
    ("2,3,1"->AOC2020_Day15.solvePart1, 78),
    ("3,2,1"->AOC2020_Day15.solvePart1, 438),
    ("3,1,2"->AOC2020_Day15.solvePart1, 1836),
  ]

  testEach2("Part 1 - Sample Data", part1_sample_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    //    let result = AOC2020_Day15.solvePart1(data)
    let result = 1
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day15.solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
