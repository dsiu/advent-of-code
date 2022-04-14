open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day11_Data.data
let sampleData = AOC2021_Day11_Data_Sample.data

describe("2021 Day11", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day11.solvePart1(sampleData)
    let expected = 1656

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day11.solvePart1(data)
    let expected = 1793

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day11.solvePart2(sampleData)
    let expected = 195

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day11.solvePart2(data)
    let expected = 247

    expect(result) |> toEqual(expected)
  })
})
