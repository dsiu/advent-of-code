open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day12_Data.data
let sampleData = AOC2020_Day12_Data_Sample.data

describe("2020 Day12", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day12.solvePart1(sampleData)
    let expected = 25

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day12.solvePart1(data)
    let expected = 1010

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2020_Day12.solvePart2(sampleData)
    let expected = 286

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day12.solvePart2(data)
    let expected = 52742

    expect(result) |> toEqual(expected)
  })
})
