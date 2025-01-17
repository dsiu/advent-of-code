@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day1_Data.data
let sampleData = AOC2023_Day1_Data_Sample.data
let sampleData2 = AOC2023_Day1_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2023_Day1)

describe("2023 Day1", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 142

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 54159

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData2)
    let expected = 281

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 53866

    expect(result)->toEqual(expected)
  })
})
