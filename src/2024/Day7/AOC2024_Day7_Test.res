open Jest
open Expect

//open Belt
let data = AOC2024_Day7_Data.data
let sampleData = AOC2024_Day7_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2024_Day7)

describe("2024 Day7", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 3749n

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 850435817339n

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 11387n

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 104824810233437n

    expect(result)->toEqual(expected)
  })
})
