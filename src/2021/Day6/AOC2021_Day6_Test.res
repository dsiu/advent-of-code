open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day6_Data.data
let sampleData = AOC2021_Day6_Data_Sample.data

describe("2021 Day6", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day6.solvePart1(sampleData)
    let expected = 5934->Belt.Int.toString

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day6.solvePart1(data)
    let expected = 379414->Belt.Int.toString

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day6.solvePart2(sampleData)
    let expected = "26984457539"

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day6.solvePart2(data)
    let expected = "1705008653296"

    expect(result) |> toEqual(expected)
  })
})
