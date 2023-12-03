open Jest
open Expect

//open Belt
let data = AOC2021_Day19_Data.data
let sampleData = AOC2021_Day19_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day19)

describe("2021 Day19", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 79

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 394

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 3621.

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 12304.

    expect(result)->toEqual(expected)
  })
})
