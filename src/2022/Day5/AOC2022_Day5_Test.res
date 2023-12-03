open Jest
open Expect

//open Belt
let data = AOC2022_Day5_Data.data
let sampleData = AOC2022_Day5_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2022_Day5)

describe("2022 Day5", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = "CMZ"

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = "PTWLTDSJV"

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = "MCD"

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = "WZMFVGGZP"

    expect(result)->toEqual(expected)
  })
})
