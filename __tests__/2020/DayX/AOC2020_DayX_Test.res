open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_DayX_Data.data
let testData = AOC2020_DayX_Data_Test.data

describe("2020 DayX", () => {
  test("Part 1 - Test Data", () => {
    let result = AOC2020_DayX.solvePart1(testData)
    let expected = 2

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_DayX.solvePart1(data)
    let expected = 206

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_DayX.solvePart2(data)
    let expected = 123

    expect(result) |> toEqual(expected)
  })
})
