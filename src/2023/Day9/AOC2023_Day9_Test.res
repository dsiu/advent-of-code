@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day9_Data.data
let sampleData = AOC2023_Day9_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day9)

describe("2023 Day9", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 114

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1980437560

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 977

    expect(result)->toEqual(expected)
  })
})
