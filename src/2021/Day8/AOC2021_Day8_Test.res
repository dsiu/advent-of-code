open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day8_Data.data
let sampleData = AOC2021_Day8_Data_Sample.data

describe("2021 Day8", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day8.solvePart1(sampleData)
    let expected = 26

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day8.solvePart1(data)
    let expected = 532

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day8.solvePart2(sampleData)
    let expected = 61_229

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day8.solvePart2(data)
    let expected = 1_011_284

    expect(result) |> toEqual(expected)
  })
})
