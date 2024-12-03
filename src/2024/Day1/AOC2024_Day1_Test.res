open Jest
open Expect

//open Belt
let data = AOC2024_Day1_Data.data
let sampleData = AOC2024_Day1_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2024_Day1)

describe("2024 Day1", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 11

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1889772

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 31

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 23228917

    expect(result)->toEqual(expected)
  })
})
