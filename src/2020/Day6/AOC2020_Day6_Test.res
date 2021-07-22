open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day6_Data.data
let testData = AOC2020_Day6_Data_Sample.data

describe("2020 Day6", () => {
  test("Part 1 - Test Data", () => {
    let result = AOC2020_Day6.solvePart1(testData)
    let expected = 11

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day6.solvePart1(data)
    let expected = 6457

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day6.solvePart2(data)
    let expected = 3260

    expect(result) |> toEqual(expected)
  })
})
