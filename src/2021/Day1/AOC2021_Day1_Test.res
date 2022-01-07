open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day1_Data.data
let sampleData = AOC2021_Day1_Data_Sample.data

describe("2021 Day1", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day1.solvePart1(sampleData)
    let expected = 7

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day1.solvePart1(data)
    let expected = 1502

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day1.solvePart2(data)
    let expected = 1538

    expect(result) |> toEqual(expected)
  })
})
