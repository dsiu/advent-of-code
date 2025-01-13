@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day7_Data.data
let sampleData = AOC2023_Day7_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day7)

describe("2023 Day7", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 6440

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 250120186

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 5905

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 250665248

    expect(result)->toEqual(expected)
  })
})
