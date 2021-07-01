open Jest
open Expect
open! Expect.Operators
open AOC2020_Day1

let data = AOC2020_Day1_Data.data
let testData = AOC2020_Day1_Data_Test.data

describe("2020 Day1", () => {
  test("Part 1 - Test Data", () => {
    let result = solvePart1(testData)
    let expected = [514579, 514579]

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = [1013211, 1013211]

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = [13891280, 13891280, 13891280]

    expect(result) |> toEqual(expected)
  })
})
