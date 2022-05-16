open Jest
open Expect
open! Expect.Operators

let data = AOC2021_Day14_Data.data
let sampleData = AOC2021_Day14_Data_Sample.data

describe("2021 Day14", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day14.solvePart1(sampleData)
    let expected = 1588L

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day14.solvePart1(data)
    let expected = 3306L

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day14.solvePart2(sampleData)
    let expected = 2188189693529L
    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day14.solvePart2(data)
    let expected = 3760312702877L

    expect(result) |> toEqual(expected)
  })
})
