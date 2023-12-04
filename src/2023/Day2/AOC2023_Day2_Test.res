@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day2_Data.data
let sampleData = AOC2023_Day2_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day2)

describe("2023 Day2", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 8

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 2679

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2286

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 77607

    expect(result)->toEqual(expected)
  })
})
