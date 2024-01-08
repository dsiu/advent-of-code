@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day8_Data.data
let sampleData = AOC2023_Day8_Data_Sample.data
let sampleData2 = AOC2023_Day8_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2023_Day8)

describe("2023 Day8", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 6

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 13207

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData2)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
