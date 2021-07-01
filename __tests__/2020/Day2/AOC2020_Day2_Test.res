open Jest
open Expect
open! Expect.Operators
open AOC2020_Day2

let data = AOC2020_Day2_Data.data
let testData = AOC2020_Day2_Data_Test.data

describe("2020 Day2", () => {
  test("Part 1 - Test Data", () => {
    let result = solvePart1(testData)
    let expected = 2

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 383

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 272

    expect(result) |> toEqual(expected)
  })
})
