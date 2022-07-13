open Jest
open Expect
open! Expect.Operators
open AOC2020_Day1

let data = AOC2020_Day1_Data.data
let sampleData = AOC2020_Day1_Data_Sample.data

describe("2020 Day1", () => {
  test("Part 1 - Test Data", () => {
    let result = solvePart1(sampleData)
    let expected = [514_579, 514_579]

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = [1_013_211, 1_013_211]

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = [13_891_280, 13_891_280, 13_891_280]

    expect(result)->toEqual(expected)
  })
})
