open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day2_Data.data
let sampleData = AOC2021_Day2_Data_Sample.data

describe("2020 DayX", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day2.solvePart1(sampleData)
    let expected = 150

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day2.solvePart1(data)
    let expected = 1804520

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day2.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
