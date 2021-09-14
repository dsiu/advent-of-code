open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day10_Data.data
let sampleData = AOC2020_Day10_Data_Sample.data

describe("2020 Day10", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day10.solvePart1(sampleData)
    let expected = 35

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day10.solvePart1(data)
    let expected = 2470

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day10.solvePart2(data)
    let expected = 1973822685184.0

    expect(result) |> toEqual(expected)
  })
})
