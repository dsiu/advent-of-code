open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day5_Data.data
let testData = AOC2020_Day5_Data_Sample.data

describe("2020 Day5", () => {
  test("Part 1 - Test Data", () => {
    let result = AOC2020_Day5.solvePart1(testData)
    let expected = 820

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day5.solvePart1(data)
    let expected = 908

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day5.solvePart2(data)
    let expected = 619

    expect(result) |> toEqual(expected)
  })
})
