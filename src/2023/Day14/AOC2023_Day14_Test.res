@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day14_Data.data
let sampleData = AOC2023_Day14_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day14)

describe("2023 Day14", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 136

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 109654

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 64

    expect(result)->toEqual(expected)
  })

  // takes too long
  Skip.test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 94876

    expect(result)->toEqual(expected)
  })
})
