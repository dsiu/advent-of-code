open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day9_Data.data
let sampleData = AOC2020_Day9_Data_Sample.data

describe("2020 Day9", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day9.solvePart1(sampleData, 5)
    let expected = 127

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day9.solvePart1(data, 25)
    let expected = 31161678

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2020_Day9.solvePart2(sampleData, 5)
    let expected = 62

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day9.solvePart2(data, 25)
    let expected = 5453868

    expect(result) |> toEqual(expected)
  })
})
