open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day9_Data.data
let sampleData = AOC2021_Day9_Data_Sample.data

describe("2021 Day9", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day9.solvePart1(sampleData)
    let expected = 15

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day9.solvePart1(data)
    let expected = 465

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day9.solvePart2(sampleData)
    let expected = 1134

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day9.solvePart2(data)
    let expected = 1269555

    expect(result) |> toEqual(expected)
  })
})
