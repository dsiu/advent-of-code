@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day6_Data.data
let sampleData = AOC2023_Day6_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day6)

describe("2023 Day6", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 288

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 316800

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 71503

    expect(result)->toEqual(expected)
  })

  // skipping because it takes too long (but the result is correct
  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 45647654

    expect(result)->toEqual(expected)
  })
})
