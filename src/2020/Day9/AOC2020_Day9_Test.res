open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day9_Data.data
let sampleData = AOC2020_Day9_Data_Sample.data

describe("2020 Day9", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day9.solvePart1(sampleData)
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day9.solvePart1(data)
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day9.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
