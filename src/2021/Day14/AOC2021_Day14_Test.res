open Jest
open Expect
open! Expect.Operators

let data = AOC2021_Day14_Data.data
let sampleData = AOC2021_Day14_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day14)

describe("2021 Day14", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 1588L

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 3306L

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2188189693529L
    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 3760312702877L

    expect(result) |> toEqual(expected)
  })
})
