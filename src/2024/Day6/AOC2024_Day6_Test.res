open Jest
open Expect

//open Belt
let data = AOC2024_Day6_Data.data
let sampleData = AOC2024_Day6_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2024_Day6)

describe("2024 Day6", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 41

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 4819

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
